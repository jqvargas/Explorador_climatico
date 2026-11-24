# Orquestador diario robusto: Docker + ingesta + alertas + pronostico con resume.
param(
    [switch]$Resume,
    [switch]$ResetCheckpoint,
    [switch]$DryRun,
    [switch]$ScrapeOnly,
    [switch]$PostOnly,
    # Post ligero tras gapfill: alertas + concordancia; no re-corre skill/bias/refresh.
    [switch]$FromGapfill,
    [switch]$Watchdog,
    [string]$TaskName = "",
    [string]$TriggerSource = "",
    [int]$StepTimeoutMin = 1440,
    # Corte absoluto de ingesta (hora local). Pasado este reloj: docker stop + post-pasos.
    [string]$IngestDeadlineLocal = "09:00",
    # Deadline gapfill Vigilancia (solo aplica con -Watchdog; se reenvia a pipeline_watchdog.ps1).
    [string]$DeadlineLocal = "20:00"
)

$ErrorActionPreference = "Stop"
$ProjectRoot = Resolve-Path (Join-Path $PSScriptRoot "..\..")
Set-Location $ProjectRoot

. (Join-Path $PSScriptRoot "profile_spans.ps1")

$LogsDir = Join-Path $ProjectRoot "ingest\logs"
$LockPath = Join-Path $LogsDir "actualizar_diario.lock"
$CkptPath = Join-Path $LogsDir "actualizar_diario.checkpoint"
$StatusPath = Join-Path $LogsDir "actualizar_diario.status.json"
$LiveTailPath = Join-Path $LogsDir "pipeline_live_tail.json"
$TimelineOpenPath = Join-Path $LogsDir "pipeline_timeline_open.json"
$script:TimelineSteps = New-Object System.Collections.Generic.List[hashtable]
$Day = Get-Date -Format "yyyyMMdd"
$LogPath = Join-Path $LogsDir "actualizar_diario_$Day.log"

if (-not (Test-Path $LogsDir)) { New-Item -ItemType Directory -Path $LogsDir -Force | Out-Null }

# Override one-shot para corridas manuales via schtasks (sin /Change que pide password).
# Escrito por run_actualizar_diario_manual.ps1; se consume y borra al arrancar.
$ManualOverridePath = Join-Path $LogsDir "pipeline_manual_override.json"
if (Test-Path $ManualOverridePath) {
    try {
        $ov = Get-Content $ManualOverridePath -Raw -Encoding UTF8 | ConvertFrom-Json
        if ($ov.IngestDeadlineLocal -and "$($ov.IngestDeadlineLocal)" -match '^\d{1,2}:\d{2}$') {
            $IngestDeadlineLocal = [string]$ov.IngestDeadlineLocal
        }
        if ($ov.DeadlineLocal -and "$($ov.DeadlineLocal)" -match '^\d{1,2}:\d{2}$') {
            $DeadlineLocal = [string]$ov.DeadlineLocal
        }
        if ($ov.ResetCheckpoint -eq $true) { $ResetCheckpoint = $true }
        if ($ov.TriggerSource) { $TriggerSource = [string]$ov.TriggerSource }
    } catch {
        Write-Host "WARN: pipeline_manual_override.json invalido: $($_.Exception.Message)"
    }
    Remove-Item $ManualOverridePath -Force -ErrorAction SilentlyContinue
}

$ComposeArgs = @(
    "compose", "-f", "docker-compose.yml", "-f", "docker-compose.prod.yml"
)
. (Join-Path $PSScriptRoot "docker_bounded.ps1")

$PipelineSteps = @(
    @{ Id = "preflight";       Label = "preflight Docker" },
    @{ Id = "ingest";          Label = "ingesta DGA+DMC" },
    @{ Id = "alertas_top10";   Label = "alertas top10" },
    @{ Id = "refresh_pronostico"; Label = "refresh pronostico" },
    @{ Id = "extremos_sombra"; Label = "extremos sombra" },
    @{ Id = "alertas_pronostico"; Label = "alertas pronostico" },
    @{ Id = "verificar_concordancia"; Label = "verificar concordancia" },
    @{ Id = "evaluar_precision_alertas"; Label = "evaluar precision alertas" },
    @{ Id = "actualizar_skill_alertas"; Label = "actualizar skill alertas" },
    @{ Id = "senales_ensemble"; Label = "ensemble sombra" },
    @{ Id = "bias_historico";  Label = "backfill Open-Meteo + QC precision" }
)

$script:ExitCode = 0
$script:RunId = (Get-Date).ToString('yyyyMMddTHHmmss')
$script:StartedAt = (Get-Date).ToString('o')
$script:LastStep = ""
$script:SkippedRun = $false
$script:StepResults = @()
$script:IngestMetrics = $null
$script:IngestVarKeys = @(
    "dga/pp", "dga/t_min", "dga/u_max_diario",
    "dmc/pp", "dmc/t_max", "dmc/t_min", "dmc/t_mean", "dmc/hr_mean", "dmc/rs_mean", "dmc/u_10_kt", "dmc/u_rafaga"
)
$script:PipelineMode = "full"
$script:PipelineTaskName = "Rodaja_Actualizar_Diario"
$script:PipelinePhase = "orchestrator"
# Si ingest incompleto: encolar alertas pero no enviar digests (flush en FromGapfill / deferred).
$script:DeferDigests = $false

function Resolve-TriggerSource([string]$Explicit = "") {
    # Usar parametro explicito: NUNCA leer $TriggerSource aqui tras asignar
    # $script:TriggerSource (en PS param y script scope son la misma variable).
    $explicit = "$Explicit".Trim().ToLowerInvariant()
    if ($explicit -in @("scheduler", "scheduled", "manual", "chained")) {
        if ($explicit -eq "scheduled") { return "scheduler" }
        return $explicit
    }
    # Task Scheduler suele ser padre taskeng/taskhost/svchost
    try {
        $proc = Get-CimInstance Win32_Process -Filter "ProcessId=$PID" -ErrorAction Stop
        $ppid = [int]$proc.ParentProcessId
        $depth = 0
        while ($ppid -gt 0 -and $depth -lt 6) {
            $parent = Get-CimInstance Win32_Process -Filter "ProcessId=$ppid" -ErrorAction Stop
            $pn = "$($parent.Name)".ToLowerInvariant()
            if ($pn -match '^(taskeng|taskhostw|taskhost)\.exe$') { return "scheduler" }
            if ($pn -match '^(sshd|cursor|code|windowsterminal|wt|cmd)\.exe$') { return "manual" }
            $ppid = [int]$parent.ParentProcessId
            $depth++
        }
    } catch {}
    if (-not [Environment]::UserInteractive) { return "scheduler" }
    return "manual"
}

function Resolve-PipelineIdentity {
    if ($ScrapeOnly) {
        $script:PipelineMode = "scrape"
        $script:PipelineTaskName = if ($TaskName) { $TaskName } else { "Rodaja_Ingest_Scrape" }
        return
    }
    if ($FromGapfill) {
        $script:PipelineMode = "gapfill_post"
        $script:PipelineTaskName = if ($TaskName) { $TaskName } else { "Rodaja_Actualizar_Resume" }
        return
    }
    if ($PostOnly) {
        $script:PipelineMode = "post"
        $script:PipelineTaskName = if ($TaskName) { $TaskName } else { "Rodaja_Ingest_Post" }
        return
    }
    if ($TaskName -match "Resume" -or $TaskName -eq "Rodaja_Actualizar_Resume") {
        $script:PipelineMode = if ($Watchdog) { "watchdog" } else { "resume" }
        $script:PipelineTaskName = "Rodaja_Actualizar_Resume"
        return
    }
    if ($Watchdog) {
        $script:PipelineMode = "watchdog"
        $script:PipelineTaskName = if ($TaskName) { $TaskName } else { "Rodaja_Actualizar_Resume" }
        return
    }
    if ($TaskName) {
        $script:PipelineTaskName = $TaskName
        $script:PipelineMode = "full"
        return
    }
    $script:PipelineMode = "full"
    $script:PipelineTaskName = "Rodaja_Actualizar_Diario"
}

Resolve-PipelineIdentity
# Capturar param ANTES de tocar $script:TriggerSource (mismo binding en PS)
$script:TriggerSource = Resolve-TriggerSource -Explicit $TriggerSource

# Watchdog: delegar a pipeline_watchdog.ps1 (cerebro + reporte + gapfill)
if ($Watchdog) {
    $wd = Join-Path $PSScriptRoot "pipeline_watchdog.ps1"
    $wdArgs = @("-TaskName", $script:PipelineTaskName, "-TriggerSource", $script:TriggerSource, "-DeadlineLocal", $DeadlineLocal)
    if ($DryRun) { $wdArgs += "-DryRun" }
    Write-Host "[$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')] delegando a pipeline_watchdog.ps1 trigger=$($script:TriggerSource)"
    & powershell -NoProfile -ExecutionPolicy Bypass -File $wd @wdArgs
    exit $LASTEXITCODE
}

try {
    Initialize-OrchProfile -RunId $script:RunId -TaskName $script:PipelineTaskName -Mode $script:PipelineMode -Trigger $script:TriggerSource
} catch {
    Write-Host "WARN profile init: $($_.Exception.Message)"
}

# Env INGEST_PROFILE_* para contenedores (misma corrida JSONL que el orch).
function Get-ProfileDockerEnvArgs([string]$StepId = "", [string]$ProfileDir = "") {
    $args = @()
    if (-not $script:ProfileRunId) { return $args }
    $t0 = if ($script:ProfileT0Ms) { "$($script:ProfileT0Ms)" } else { "" }
    $parent = if ($script:ProfileStepSpanId) { "$($script:ProfileStepSpanId)" } else { "" }
    $pairs = @(
        "INGEST_PROFILE_TASK=$($script:PipelineTaskName)",
        "INGEST_PROFILE_MODE=$($script:PipelineMode)",
        "INGEST_PROFILE_RUN_ID=$($script:ProfileRunId)",
        "INGEST_PROFILE_TRIGGER=$($script:TriggerSource)"
    )
    if ($t0) { $pairs += "INGEST_PROFILE_T0_MS=$t0" }
    if ($parent) { $pairs += "INGEST_PROFILE_PARENT=$parent" }
    if ($StepId) { $pairs += "INGEST_PROFILE_STEP=$StepId" }
    if ($ProfileDir) { $pairs += "INGEST_PROFILE_DIR=$ProfileDir" }
    foreach ($e in $pairs) { $args += @("-e", $e) }
    return $args
}

# Env de worker/api por paso (perfil + defer digests si ingest incompleto).
function Get-WorkerStepEnvArgs([string]$StepId = "", [string]$ProfileDir = "") {
    $args = @(Get-ProfileDockerEnvArgs -StepId $StepId -ProfileDir $ProfileDir)
    if ($script:DeferDigests -and $StepId -eq "alertas_pronostico") {
        $args += @("-e", "ALERTAS_DIGEST_DEFER=1")
    }
    return $args
}

function Update-DeferDigestsFlag {
    # FromGapfill / flush: siempre enviar digests (completos o parciales al mediodia).
    if ($FromGapfill) {
        $script:DeferDigests = $false
        return
    }
    $completed = @(Get-CompletedVarsFromCkpt)
    $incomplete = @($script:IngestVarKeys | Where-Object { $completed -notcontains $_ })
    $script:DeferDigests = ($incomplete.Count -gt 0)
    if ($script:DeferDigests) {
        Write-Log "ALERTAS_DIGEST_DEFER=1 (incomplete=$($incomplete.Count): $($incomplete -join ','))"
    }
}

# ponytail: Add-Content exclusive lock + ErrorActionPreference=Stop → ORCH_LOG_FILE_LOCKED
# (falso STEP_INGEST_FAIL tras scrape OK). FileShare.ReadWrite + retry; NUNCA throw.
function Test-OrchLogFileLockMessage([string]$Msg) {
    if (-not $Msg) { return $false }
    return ($Msg -match 'utilizado en otro proceso|being used by another process|cannot access the file|IOException')
}

function Write-Log([string]$Msg) {
    $line = "[$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')] $Msg"
    Write-Host $line
    if (-not $LogPath) { return }
    $dir = Split-Path $LogPath -Parent
    if ($dir -and -not (Test-Path $dir)) {
        try { New-Item -ItemType Directory -Path $dir -Force | Out-Null } catch {}
    }
    $utf8 = [System.Text.UTF8Encoding]::new($false)
    $maxTries = 8
    for ($i = 1; $i -le $maxTries; $i++) {
        $fs = $null
        $sw = $null
        try {
            $fs = [System.IO.File]::Open(
                $LogPath,
                [System.IO.FileMode]::Append,
                [System.IO.FileAccess]::Write,
                [System.IO.FileShare]::ReadWrite
            )
            $sw = New-Object System.IO.StreamWriter($fs, $utf8)
            $sw.WriteLine($line)
            $sw.Flush()
            return
        } catch {
            if ($i -ge $maxTries) {
                Write-Host "[WARN] Write-Log disk skip after ${maxTries}x: $($_.Exception.Message)"
                return
            }
            Start-Sleep -Milliseconds (40 * $i)
        } finally {
            if ($sw) { try { $sw.Dispose() } catch {} }
            elseif ($fs) { try { $fs.Dispose() } catch {} }
        }
    }
}

function Resolve-IngestExitAfterOrchNoise {
    # Tras excepcion de logging / volcado, recuperar rc real del scrape (metrics o err.log).
    $errFile = Join-Path $LogsDir "docker_ingest_${Day}.err.log"
    $fromLog = Test-IngestErrLogComplete $errFile
    if ($null -ne $fromLog) { return [int]$fromLog }
    $m = Read-IngestMetrics
    if ($m -and $m.PSObject.Properties['exit_code']) {
        try { return [int]$m.exit_code } catch {}
    }
    if (Test-RIngestCheckpointComplete) { return 0 }
    return $null
}

function Set-LastStepResultExit([string]$StepId, [int]$ExitCode) {
    if ($script:StepResults.Count -eq 0) { return }
    $last = $script:StepResults[-1]
    if ("$($last.id)" -ne $StepId) { return }
    $last['exit_code'] = $ExitCode
    $idx = Find-TimelineStep $StepId
    if ($idx -ge 0) {
        $script:TimelineSteps[$idx].exit_code = $ExitCode
        Write-TimelineOpenFile
    }
}

function Write-Status([hashtable]$Fields) {
    $base = @{
        started_at  = $script:StartedAt
        finished_at = $null
        last_step   = $script:LastStep
        exit_code   = $script:ExitCode
        task_name   = $script:PipelineTaskName
        mode        = $script:PipelineMode
        phase       = $script:PipelinePhase
        run_id      = $script:RunId
    }
    foreach ($k in $Fields.Keys) { $base[$k] = $Fields[$k] }
    if ($Fields.ContainsKey("phase") -and $Fields["phase"]) {
        $script:PipelinePhase = "$($Fields['phase'])"
    }
    [System.IO.File]::WriteAllText($StatusPath, ($base | ConvertTo-Json -Depth 4), [System.Text.UTF8Encoding]::new($false))
}

function Read-Ckpt {
    $h = @{}
    if (-not (Test-Path $CkptPath)) { return $h }
    Get-Content $CkptPath -Encoding UTF8 | ForEach-Object {
        $line = $_.Trim()
        if ($line -eq "" -or $line.StartsWith("#")) { return }
        $idx = $line.IndexOf("=")
        if ($idx -gt 0) {
            $h[$line.Substring(0, $idx)] = $line.Substring($idx + 1)
        }
    }
    return $h
}

function Write-CkptFields([hashtable]$Fields) {
    $cur = Read-Ckpt
    foreach ($k in $Fields.Keys) {
        if ($null -eq $Fields[$k] -or "$($Fields[$k])" -eq "") {
            $cur.Remove($k)
        } else {
            $cur[$k] = "$($Fields[$k])"
        }
    }
    $lines = $cur.GetEnumerator() | ForEach-Object { "$($_.Key)=$($_.Value)" }
    Set-Content -Path $CkptPath -Value $lines -Encoding UTF8
}

function Clear-Ckpt {
    if (Test-Path $CkptPath) { Remove-Item $CkptPath -Force }
}

function Get-PipelineDone {
    $cp = Read-Ckpt
    $raw = $cp["pipeline_done"]
    if (-not $raw) { return @() }
    return $raw.Split(",") | Where-Object { $_ -ne "" }
}

# Conservar incomplete_vars/completed_vars, pero no post-pasos de un run anterior.
# Keep vacÃƒÂ­o = borrar pipeline_done. Usar tras forzar re-ingest o al inicio de -PostOnly.
function Clear-PipelineDoneKeep([string[]]$Keep = @()) {
    $before = @(Get-PipelineDone)
    if ($before.Count -eq 0) { return }
    $keepSet = @{}
    foreach ($k in $Keep) { if ($k) { $keepSet["$k"] = $true } }
    $after = @($before | Where-Object { $keepSet.ContainsKey("$_") })
    if ($after.Count -eq $before.Count) { return }
    Write-CkptFields @{ pipeline_done = ($after -join ",") }
    Write-Log ("checkpoint: limpio pipeline_done stale keep={0} removed={1}" -f `
        ($(if ($after.Count) { $after -join "," } else { "(none)" })), `
        (($before | Where-Object { -not $keepSet.ContainsKey("$_") }) -join ","))
}

function Test-RIngestCheckpointComplete {
    $cp = Read-Ckpt
    if ($cp["pipeline_step"] -ne "ingest_done") { return $false }
    $today = (Get-Date).ToString("yyyy-MM-dd")
    $fin = $cp["fecha_fin"]
    # Checkpoint de otro dia no cuenta como "done" intradÃƒÂ­a (si no, el poll de docker
    # mata el ingest nuevo apenas arranca al ver ingest_done viejo).
    if (-not $fin -or $fin -lt $today) { return $false }
    $raw = $cp["completed_vars"]
    if (-not $raw) { return $false }
    $done = $raw.Split(",") | Where-Object { $_ -ne "" }
    foreach ($key in $script:IngestVarKeys) {
        if ($done -notcontains $key) { return $false }
    }
    return $true
}

function Test-PipelineStepDone([string]$StepId) {
    if ($StepId -eq "ingest") {
        $cp = Read-Ckpt
        $fin = $cp["fecha_fin"]
        $today = (Get-Date).ToString("yyyy-MM-dd")
        # Solo se considera "ingest done" intradÃƒÂ­a. fecha_fin de otro dÃƒÂ­a Ã¢â€ â€™ rescripe fresco
        # (R recalcula desde BD con cap; no se reanuda mega-rango viejo).
        if ($fin -and $fin -lt $today) {
            Write-Log "ingest: fecha_fin=$fin anterior a hoy=$today; forzando re-ejecucion (ventana nueva)"
            # ORCH_STALE_PIPELINE_DONE_SKIPS_POST: no dejar alertas/bias marcados done
            Clear-PipelineDoneKeep @("preflight")
            return $false
        }
        if (-not $fin -and (Test-Path $CkptPath)) {
            $ckptDay = (Get-Item $CkptPath).LastWriteTime.ToString("yyyy-MM-dd")
            if ($ckptDay -lt $today) {
                Write-Log "ingest: checkpoint sin fecha_fin de $ckptDay; forzando re-ejecucion"
                Clear-PipelineDoneKeep @("preflight")
                return $false
            }
        }
        $runId = $cp["run_id"]
        if ($runId -and $runId.Length -ge 8) {
            $runDay = $runId.Substring(0, 8)
            $todayCompact = (Get-Date).ToString("yyyyMMdd")
            if ($runDay -lt $todayCompact -and $fin -eq $today) {
                # fecha_fin ya hoy pero run de otro dÃƒÂ­a: no confiar en pipeline_done sucio
                Write-Log "ingest: run_id=$runId de otro dia con fecha_fin=$fin; forzando re-ejecucion"
                Clear-PipelineDoneKeep @("preflight")
                return $false
            }
        }
    }
    if ((Get-PipelineDone) -contains $StepId) { return $true }
    if ($StepId -eq "ingest") { return (Test-RIngestCheckpointComplete) }
    return $false
}

function Invoke-ObservacionDiariaSyncIfStale {
    Write-Log "sync_diaria: comprobando desfase jornada_cerrada"
    $dockerArgs = $ComposeArgs + @("exec", "-T", "worker", "Rscript", "scripts/sync_observacion_diaria_reciente.R")
    $rc = Invoke-DockerCommandSync $dockerArgs "sync_diaria" 45
    if ($rc -ne 0) {
        Write-Log "WARN: sync_diaria exit=$rc (no fatal)"
    }
    return 0
}

function Mark-PipelineStepDone([string]$StepId) {
    $done = @(Get-PipelineDone)
    if ($done -notcontains $StepId) { $done += $StepId }
    $fields = @{ pipeline_step = $StepId; pipeline_done = ($done -join ",") }
    if ($StepId -eq "ingest") {
        $cp = Read-Ckpt
        if (-not $cp["fecha_fin"]) {
            $fields["fecha_fin"] = (Get-Date).ToString("yyyy-MM-dd")
        }
    }
    Write-CkptFields $fields
}

function Get-LiveIngestRunIds {
    $psArgs = $ComposeArgs + @("--profile", "ingest", "ps", "-aq", "--filter", "status=running")
    $ps = Invoke-DockerNoThrow $psArgs
    $ids = @()
    foreach ($raw in @($ps.Output)) {
        if ($raw -isnot [string]) { continue }
        $id = $raw.Trim()
        if ($id -eq "" -or $id -match '^(Error|error|failed)') { continue }
        $insp = Invoke-DockerNoThrow @("inspect", "-f", "{{.Name}} {{.Created}}", $id)
        $line = [string]($insp.Output | Select-Object -First 1)
        if ($line -notmatch "ingest-run") { continue }
        $ids += $id
    }
    return $ids
}

function Test-CkptHasRecentProgress([int]$MaxStaleMin = 60) {
    if (-not (Test-Path $CkptPath)) { return $false }
    $ageMin = ((Get-Date) - (Get-Item $CkptPath).LastWriteTime).TotalMinutes
    if ($ageMin -le $MaxStaleMin) { return $true }
    $cp = Read-Ckpt
    $lp = $cp["last_progress_at"]
    if ($lp) {
        try {
            return (((Get-Date) - [datetime]$lp).TotalMinutes -le $MaxStaleMin)
        } catch { return $false }
    }
    return $false
}

function Test-LockStale {
    if (-not (Test-Path $LockPath)) { return $true }
    try {
        $lock = Get-Content $LockPath -Raw -Encoding UTF8 | ConvertFrom-Json
        $ageH = ((Get-Date) - [datetime]$lock.ts).TotalHours
        # task limit 24h; stale absoluto tras 22h
        if ($ageH -ge 22) { return $true }

        $pidAlive = $false
        if ($lock.pid) {
            $proc = Get-Process -Id $lock.pid -ErrorAction SilentlyContinue
            $pidAlive = [bool]$proc
        }
        $liveIds = @(Get-LiveIngestRunIds)
        $ownContainer = $false
        if ($lock.container_id) {
            $ownContainer = $liveIds -contains [string]$lock.container_id
        } elseif ($liveIds.Count -gt 0) {
            # Cualquier ingest-run vivo = dueÃƒÂ±o del scrape (orch puede haber muerto
            # sin dejar container_id en el lock). No usar ventana 5m: un GET lento
            # dejaria el lock "stale" y arrancaria un segundo ingest.
            $ownContainer = $true
        }

        # R ya terminÃƒÂ³ ingest y no hay dueÃƒÂ±o vivo Ã¢â€ â€™ lock huÃƒÂ©rfano
        if (-not $pidAlive -and -not $ownContainer -and (Test-RIngestCheckpointComplete)) {
            return $true
        }

        # PID muerto y sin contenedor propio Ã¢â€ â€™ stale (progreso ckpt no retiene el lock)
        if (-not $pidAlive -and -not $ownContainer) {
            return $true
        }
        if ($pidAlive -or $ownContainer) {
            return $false
        }
        return $false
    } catch {
        $fi = Get-Item $LockPath
        $liveIds = @(Get-LiveIngestRunIds)
        if ($liveIds.Count -gt 0 -or (Test-CkptHasRecentProgress 60)) { return $false }
        if (Test-RIngestCheckpointComplete) { return $true }
        return (((Get-Date) - $fi.LastWriteTime).TotalHours -ge 8)
    }
}

function New-Lock {
    $obj = @{
        pid          = $PID
        ts           = (Get-Date).ToString('o')
        run_id       = $script:RunId
        phase        = "orchestrator"
        container_id = $null
        task_name    = $script:PipelineTaskName
        mode         = $script:PipelineMode
    }
    [System.IO.File]::WriteAllText($LockPath, ($obj | ConvertTo-Json -Compress), [System.Text.UTF8Encoding]::new($false))
}

function Remove-Lock {
    if (Test-Path $LockPath) { Remove-Item $LockPath -Force -ErrorAction SilentlyContinue }
}

function Write-LockHeartbeat([hashtable]$Extra = @{}) {
    $phase = "orchestrator"
    if ($script:LastStep) { $phase = $script:LastStep }
    $obj = @{
        pid          = $PID
        ts           = (Get-Date).ToString('o')
        run_id       = $script:RunId
        phase        = $phase
        container_id = $null
        task_name    = $script:PipelineTaskName
        mode         = $script:PipelineMode
    }
    if (Test-Path $LockPath) {
        try {
            $cur = Get-Content $LockPath -Raw -Encoding UTF8 | ConvertFrom-Json
            if ($cur.container_id) { $obj.container_id = $cur.container_id }
            if ($cur.phase -and -not $script:LastStep -and -not $Extra.ContainsKey("phase")) {
                $obj.phase = $cur.phase
            }
            if ($cur.run_id) { $obj.run_id = $cur.run_id }
            # Conservar dueÃƒÂ±o del lock si otro proceso solo hace heartbeat auxiliar
            if ($cur.task_name -and -not $Extra.ContainsKey("task_name")) {
                $obj.task_name = "$($cur.task_name)"
            }
            if ($cur.mode -and -not $Extra.ContainsKey("mode")) {
                $obj.mode = "$($cur.mode)"
            }
        } catch {}
    }
    foreach ($k in $Extra.Keys) { $obj[$k] = $Extra[$k] }
    [System.IO.File]::WriteAllText($LockPath, ($obj | ConvertTo-Json -Compress), [System.Text.UTF8Encoding]::new($false))
    # Cola para el banner hub (tambiÃƒÂ©n cuando err.log dejÃƒÂ³ de actualizarse por adopt)
    try { Write-PipelineLiveTail } catch {}
}

# El paso sale del Diario solo cuando el best_match Y el hindcast por modelo
# (plan de senales: ECMWF, GFS, ICON, leads 1-3, temp y pp) estan cerrados.
# archivo_completo solo cubre el best_match. Con eso en true y modelos ausente,
# omitir el paso deja la descarga por modelo en null para siempre.
function Test-BiasHistoricoTerminado($st) {
    if (-not $st) { return $false }
    $best = $false
    if ($st.archivo_completo -eq $true) { $best = $true }
    if ($st.download -and ($st.download.archivo_completo -eq $true)) { $best = $true }
    if (-not $best) { return $false }
    $mod = ""
    if ($st.PSObject.Properties.Name -contains "modelos" -and $st.modelos) {
        $mod = "$($st.modelos.status)"
    }
    return ($mod -eq "modelos_complete")
}
# end Test-BiasHistoricoTerminado

function Get-StepTimeoutMin([string]$Label) {
    switch ($Label) {
        "ingest" { return $StepTimeoutMin }
        "alertas_top10" { return 90 }
        # El reloj de 70 min ya no corta este paso. Invoke-DockerCommandLong
        # mira si hechas+fallos sigue subiendo. Este numero no se usa como tope.
        "extremos_sombra" { return 0 }
        "refresh_pronostico" { return 90 }
        "alertas_pronostico" { return 180 }
        "verificar_concordancia" { return 60 }
        "evaluar_precision_alertas" { return 90 }
        "actualizar_skill_alertas" { return 90 }
        "senales_ensemble" { return 45 }
        "invalidate_pronostico_cache" { return 15 }
        "invalidate_warm_reporte_cache" { return 15 }
        "purge_cloudflare_reporte" { return 5 }
        "bias_historico" { return 360 }
        default { return 60 }
    }
}

# Deadline de pared para ingest (hoy a IngestDeadlineLocal). Si ya paso, retorna ahora.
function Get-IngestWallDeadline {
    $parts = ("$IngestDeadlineLocal").Split(':')
    $h = 7; $m = 0
    if ($parts.Count -ge 1) { [void][int]::TryParse($parts[0], [ref]$h) }
    if ($parts.Count -ge 2) { [void][int]::TryParse($parts[1], [ref]$m) }
    $now = Get-Date
    $dl = Get-Date -Year $now.Year -Month $now.Month -Day $now.Day -Hour $h -Minute $m -Second 0
    if ($now -ge $dl) { return $now }
    return $dl
}

function Test-BeforeIngestDeadline {
    return ((Get-Date) -lt (Get-IngestWallDeadline))
}

# Stop-Job / matar el CLI docker no basta: hay que parar el contenedor ingest-run.
function Stop-IngestRunContainers([string]$Reason = "deadline") {
    try {
        $ids = @(Get-LiveIngestRunIds)
        foreach ($id in $ids) {
            if (-not $id) { continue }
            Write-Log "ingest-stop ($Reason): docker stop $id"
            $null = Invoke-DockerNoThrow @("stop", "-t", "20", $id)
        }
    } catch {
        Write-Log "WARN stop ingest-run: $($_.Exception.Message)"
    }
}

function Test-IngestErrLogComplete([string]$ErrFile) {
    if (-not (Test-Path $ErrFile)) { return $null }
    $tail = Get-Content $ErrFile -Tail 40 -ErrorAction SilentlyContinue
    foreach ($line in $tail) {
        if ($line -match '=== Fin actualizar_diario \(exit=(\d+)\) ===') {
            return [int]$Matches[1]
        }
    }
    return $null
}

function Read-IngestMetrics {
    $jsonPath = Join-Path $LogsDir "ingest_metrics_$Day.json"
    if (Test-Path $jsonPath) {
        try {
            return Get-Content $jsonPath -Raw -Encoding UTF8 | ConvertFrom-Json
        } catch {
            Write-Log "WARN: ingest_metrics JSON invalido: $($_.Exception.Message)"
        }
    }

    $errFile = Join-Path $LogsDir "docker_ingest_${Day}.err.log"
    if (-not (Test-Path $errFile)) { return $null }

    $metrics = @{}
    $inResumen = $false
    Get-Content $errFile -Encoding UTF8 -ErrorAction SilentlyContinue | ForEach-Object {
        if ($_ -match '^=== RESUMEN actualizar_diario ===') { $inResumen = $true; return }
        if ($_ -match '^=== Fin actualizar_diario') { $inResumen = $false; return }
        if ($inResumen -and $_ -match '^\s+(\w+)=(.*)$') {
            $metrics[$Matches[1]] = $Matches[2]
        }
    }
    if ($metrics.Count -eq 0) { return $null }
    return [pscustomobject]$metrics
}

# Red de seguridad: ingest rc!=0 pero al menos una fuente aporto estaciones Ã¢â€ â€™ continuar post.
function Test-IngestPartialSuccess($Metrics) {
    if (-not $Metrics) { return $false }
    $dga = 0
    $dmc = 0
    try {
        if ($Metrics.PSObject.Properties['estaciones_dga']) {
            $dga = [int]("$($Metrics.estaciones_dga)" -replace '[^\d-]', '')
        }
    } catch { $dga = 0 }
    try {
        if ($Metrics.PSObject.Properties['estaciones_dmc']) {
            $dmc = [int]("$($Metrics.estaciones_dmc)" -replace '[^\d-]', '')
        }
    } catch { $dmc = 0 }
    if ($dga -lt 0) { $dga = 0 }
    if ($dmc -lt 0) { $dmc = 0 }
    return ($dga -gt 0 -or $dmc -gt 0)
}

function Get-CompletedVarsFromCkpt {
    $cp = Read-Ckpt
    $raw = $cp["completed_vars"]
    if (-not $raw) { return @() }
    return @($raw.Split(",") | Where-Object { $_ -ne "" })
}

function Write-PipelineDayHealthAndReport {
    $completed = @(Get-CompletedVarsFromCkpt)
    $incomplete = @($script:IngestVarKeys | Where-Object { $completed -notcontains $_ })
    $pasos = [ordered]@{}
    foreach ($s in $script:StepResults) {
        $ec = [int]$s.exit_code
        # Normalizar falso fallo orch (log lock) cuando scrape metrics OK y vars completas
        if ("$($s.id)" -eq "ingest" -and $ec -ne 0 -and $incomplete.Count -eq 0) {
            $mec = $null
            try {
                if ($script:IngestMetrics -and $script:IngestMetrics.PSObject.Properties['exit_code']) {
                    $mec = [int]$script:IngestMetrics.exit_code
                }
            } catch {}
            if ($null -ne $mec -and $mec -eq 0) { $ec = 0 }
        }
        $pasos[$s.id] = [ordered]@{
            exit_code    = $ec
            skipped      = [bool]$s.skipped
            duration_sec = $s.duration_sec
        }
    }
    $http = $null
    if ($script:IngestMetrics -and $script:IngestMetrics.PSObject.Properties['http_stats']) {
        $http = $script:IngestMetrics.http_stats
    }
    $respMsScrape = $null
    if ($script:IngestMetrics -and $script:IngestMetrics.PSObject.Properties['response_ms']) {
        $respMsScrape = $script:IngestMetrics.response_ms
    }
    $respMs = $null
    try {
        $fromProf = $null
        if ($script:ProfileRunId) { $fromProf = Get-HttpResponseMsFromProfile $script:ProfileRunId }
        $respMs = Merge-ResponseMs -FromScrape $respMsScrape -FromProfile $fromProf
    } catch { $respMs = $respMsScrape }
    $gapfill = $incomplete.Count -gt 0
    $needsWd = $gapfill -or ($script:ExitCode -ne 0)
    foreach ($p in $pasos.GetEnumerator()) {
        $info = $p.Value
        if ((-not [bool]$info.skipped) -and ([int]$info.exit_code -ne 0)) { $needsWd = $true }
    }

    $health = [ordered]@{
        run_id           = $script:RunId
        task_name        = $script:PipelineTaskName
        mode             = $script:PipelineMode
        exit_code        = $script:ExitCode
        finished_at      = (Get-Date).ToString('o')
        day              = (Get-Date).ToString('yyyy-MM-dd')
        pasos            = $pasos
        ingest           = [ordered]@{
            completed_vars   = @($completed)
            expected_vars    = @($script:IngestVarKeys)
            incomplete_vars  = @($incomplete)
            estaciones_dga   = $(if ($script:IngestMetrics) { $script:IngestMetrics.estaciones_dga } else { $null })
            estaciones_dmc   = $(if ($script:IngestMetrics) { $script:IngestMetrics.estaciones_dmc } else { $null })
            warnings         = $(if ($script:IngestMetrics -and $script:IngestMetrics.PSObject.Properties['warnings']) { "$($script:IngestMetrics.warnings)" } else { $null })
            http_stats       = $http
            response_ms      = $respMs
        }
        needs_watchdog   = [bool]$needsWd
        gapfill_eligible = [bool]$gapfill
        deadline_local   = "20:00"
        ingest_deadline_local = "$IngestDeadlineLocal"
    }
    $healthPath = Join-Path $LogsDir "pipeline_day_health_$Day.json"
    $json = $health | ConvertTo-Json -Depth 10
    [System.IO.File]::WriteAllText($healthPath, $json, [System.Text.UTF8Encoding]::new($false))
    Write-Log "day_health escrito $healthPath needs_watchdog=$needsWd gapfill=$gapfill incomplete=$($incomplete.Count)"

    # Seed error report via watchdog dry diagnose (sin repairs) Ã¢â‚¬â€ script dedicado
    try {
        $wd = Join-Path $PSScriptRoot "pipeline_watchdog.ps1"
        # Solo generar reporte inicial sin gapfill: deadline pasado force? Mejor inline minimo
        $reportPath = Join-Path $LogsDir "pipeline_error_report_$Day.json"
        $errors = @()
        if ($incomplete.Count -gt 0) {
            $errors += [ordered]@{
                code = "INGEST_INCOMPLETE_VARS"; step = "ingest"; count = $incomplete.Count
                severity = "error"; repair_hint = "gapfill"
                evidence = @{ sample_lines = @("missing: $($incomplete -join ', ')") }
            }
        }
        if ($http) {
            foreach ($fname in @("dga", "dmc")) {
                $st = $null
                try { $st = $http.$fname } catch {}
                if (-not $st) { continue }
                foreach ($pair in @(
                    @{ k = "status_403"; code = "INGEST_$($fname.ToUpper())_HTTP_403" },
                    @{ k = "timeouts"; code = "INGEST_$($fname.ToUpper())_TIMEOUT" },
                    @{ k = "status_429"; code = "INGEST_$($fname.ToUpper())_HTTP_429" },
                    @{ k = "status_500"; code = "INGEST_$($fname.ToUpper())_HTTP_500" },
                    @{ k = "status_503"; code = "INGEST_$($fname.ToUpper())_HTTP_503" }
                )) {
                    $n = 0
                    try { $n = [int]$st.($pair.k) } catch { $n = 0 }
                    if ($n -gt 0) {
                        $errors += [ordered]@{
                            code = $pair.code; step = "ingest"; fuente = $fname; count = $n
                            severity = "error"; repair_hint = "probe_then_gapfill_or_defer"
                        }
                    }
                }
                try {
                    if ($st.circuit_opened) {
                        $errors += [ordered]@{
                            code = "INGEST_$($fname.ToUpper())_CIRCUIT_OPEN"; step = "ingest"; fuente = $fname; count = 1
                            severity = "error"; repair_hint = "probe_then_gapfill_or_defer"
                        }
                    }
                } catch {}
            }
        }
        foreach ($s in $script:StepResults) {
            if ($s.skipped) { continue }
            $sec = 0
            try { $sec = [int]$pasos[$s.id].exit_code } catch { $sec = [int]$s.exit_code }
            if ($sec -eq 0) { continue }
            $errors += [ordered]@{
                code = ("STEP_{0}_FAIL" -f ($s.id.ToUpper() -replace '-', '_'))
                step = $s.id; count = 1; severity = "error"; repair_hint = "rerun_step"
            }
        }
        $byCode = [ordered]@{}
        foreach ($e in $errors) {
            $c = [string]$e.code
            if (-not $byCode.Contains($c)) { $byCode[$c] = 0 }
            $byCode[$c] = [int]$byCode[$c] + [int]$e.count
        }
        $report = [ordered]@{
            day          = (Get-Date).ToString('yyyy-MM-dd')
            generated_at = (Get-Date).ToString('o')
            source       = "actualizar_diario"
            run_id       = $script:RunId
            status       = $(if ($errors.Count -eq 0) { "healthy" } else { "issues" })
            errors       = @($errors)
            by_code      = $byCode
            incomplete_vars = @($incomplete)
            actions      = @()
            deadline_local = "20:00"
            ingest_deadline_local = "$IngestDeadlineLocal"
        }
        [System.IO.File]::WriteAllText($reportPath, ($report | ConvertTo-Json -Depth 10), [System.Text.UTF8Encoding]::new($false))
        Write-Log "error_report escrito $reportPath errors=$($errors.Count)"
        try {
            . (Join-Path $PSScriptRoot "pipeline_error_compendium.ps1")
            Update-PipelineErrorCompendium -Report $report -ProjectRoot $ProjectRoot.ToString() | Out-Null
            Write-Log "compendio errores actualizado"
        } catch {
            Write-Log "WARN: compendio: $($_.Exception.Message)"
        }
    } catch {
        Write-Log "WARN: error_report: $($_.Exception.Message)"
    }
}

function Write-OrchDiedAdoptEvent([string]$ContainerId, [object]$DeadLock) {
    # Documenta ORCH_DIED_WHILE_INGEST_ALIVE en el reporte del dÃƒÂ­a + compendio.
    try {
        $reportPath = Join-Path $LogsDir "pipeline_error_report_$Day.json"
        $errors = @([ordered]@{
            code         = "ORCH_DIED_WHILE_INGEST_ALIVE"
            step         = "ingest"
            count        = 1
            severity     = "error"
            repair_hint  = "adopt_ingest_then_resume_post"
            evidence     = @{
                sample_lines = @(
                    "dead_lock_pid=$($DeadLock.pid)",
                    "adopt_container=$ContainerId",
                    "live_tail/heartbeat frozen; ingest-run still up"
                )
            }
        })
        $report = [ordered]@{
            day            = (Get-Date).ToString('yyyy-MM-dd')
            generated_at   = (Get-Date).ToString('o')
            source         = "actualizar_diario_adopt"
            run_id         = $script:RunId
            status         = "issues"
            errors         = @($errors)
            by_code        = [ordered]@{ ORCH_DIED_WHILE_INGEST_ALIVE = 1 }
            incomplete_vars = @()
            actions        = @(@{ action = "adopt_ingest"; container_id = $ContainerId; at = (Get-Date).ToString('o') })
            deadline_local = "20:00"
            ingest_deadline_local = "$IngestDeadlineLocal"
        }
        if (Test-Path $reportPath) {
            try {
                $prev = Get-Content $reportPath -Raw -Encoding UTF8 | ConvertFrom-Json
                $merged = @()
                foreach ($e in @($prev.errors)) { if ($e) { $merged += $e } }
                $merged += $errors[0]
                $report.errors = @($merged)
                $bc = [ordered]@{}
                if ($prev.by_code) {
                    foreach ($p in $prev.by_code.PSObject.Properties) { $bc[$p.Name] = $p.Value }
                }
                $bc["ORCH_DIED_WHILE_INGEST_ALIVE"] = 1
                $report.by_code = $bc
                if ($prev.run_id) { $report.run_id = $prev.run_id }
            } catch {}
        }
        [System.IO.File]::WriteAllText($reportPath, ($report | ConvertTo-Json -Depth 10), [System.Text.UTF8Encoding]::new($false))
        try {
            . (Join-Path $PSScriptRoot "pipeline_error_compendium.ps1")
            Update-PipelineErrorCompendium -Report $report -ProjectRoot $ProjectRoot.ToString() | Out-Null
        } catch {}
        Write-Log "ORCH_DIED_WHILE_INGEST_ALIVE registrado (adopt $ContainerId)"
    } catch {
        Write-Log "WARN: no se pudo registrar ORCH_DIED: $($_.Exception.Message)"
    }
}

function Get-LockPidAlive {
    if (-not (Test-Path $LockPath)) { return $false }
    try {
        $lock = Get-Content $LockPath -Raw -Encoding UTF8 | ConvertFrom-Json
        if (-not $lock.pid) { return $false }
        return [bool](Get-Process -Id ([int]$lock.pid) -ErrorAction SilentlyContinue)
    } catch {
        return $false
    }
}

function Read-LockObject {
    if (-not (Test-Path $LockPath)) { return $null }
    try {
        return (Get-Content $LockPath -Raw -Encoding UTF8 | ConvertFrom-Json)
    } catch {
        return $null
    }
}

# Corrida de otro dia calendario (run_id yyyyMMdd… o ts) no debe bloquear el Diario de manana.
function Test-LockFromPriorCalendarDay {
    $lock = Read-LockObject
    if (-not $lock) { return $false }
    $today = (Get-Date).ToString("yyyyMMdd")
    $rid = [string]$lock.run_id
    if ($rid.Length -ge 8 -and $rid.Substring(0, 8) -match '^\d{8}$') {
        return ($rid.Substring(0, 8) -lt $today)
    }
    try {
        $ts = [datetime]$lock.ts
        return ($ts.Date -lt (Get-Date).Date)
    } catch {
        return $false
    }
}

# Libera lock + orch/ingest de un dia anterior para no omitir el Diario automatico.
function Clear-PriorDayPipelineLock {
    $lock = Read-LockObject
    if (-not $lock) { return }
    Write-Log ("Clear-PriorDayPipelineLock: liberando lock dia anterior run_id={0} pid={1} task={2}" -f `
        $lock.run_id, $lock.pid, $lock.task_name)
    try {
        if ($lock.pid) {
            $p = Get-Process -Id ([int]$lock.pid) -ErrorAction SilentlyContinue
            if ($p) {
                Write-Log "Clear-PriorDayPipelineLock: taskkill /T /F pid=$($lock.pid)"
                & taskkill.exe /T /F /PID ([int]$lock.pid) 2>&1 | ForEach-Object { Write-Log "$_" }
            }
        }
    } catch {
        Write-Log "WARN Clear-PriorDayPipelineLock kill pid: $($_.Exception.Message)"
    }
    try { Stop-IngestRunContainers "prior_day_lock" } catch {}
    try { Clear-DiarioWorkerExecOrphans -ComposeArgs $ComposeArgs } catch {
        Write-Log "WARN reap worker exec: $($_.Exception.Message)"
    }
    try { schtasks.exe /End /TN "Rodaja_Actualizar_Resume" 2>$null | Out-Null } catch {}
    Remove-Lock
}

function Start-VigilanciaAfterDiario {
    # Encadenar Vigilancia (watchdog) al terminar el Diario
    if ($Watchdog -or $script:PipelineMode -eq "watchdog") { return }
    if ($script:PipelineTaskName -eq "Rodaja_Actualizar_Resume") { return }
    if ($ScrapeOnly) { return }
    # PostOnly / FromGapfill: no disparar gapfill DGA (SNIA lento / decision humana)
    if ($PostOnly -or $FromGapfill -or $script:PipelineMode -in @("post", "gapfill_post")) { return }
    Write-Log "Encadenando Vigilancia: schtasks /Run Rodaja_Actualizar_Resume"
    try {
        $out = & schtasks.exe /Run /TN "Rodaja_Actualizar_Resume" 2>&1
        Write-Log "schtasks /Run Vigilancia: $out"
    } catch {
        Write-Log "WARN: no se pudo lanzar Vigilancia: $($_.Exception.Message)"
        # Fallback: Start-Process
        try {
            $wd = Join-Path $PSScriptRoot "pipeline_watchdog.ps1"
            Start-Process -FilePath "powershell.exe" -ArgumentList @(
                "-NoProfile", "-ExecutionPolicy", "Bypass", "-File", $wd,
                "-TaskName", "Rodaja_Actualizar_Resume",
                "-TriggerSource", "chained",
                "-DeadlineLocal", $DeadlineLocal
            ) -WindowStyle Hidden
            Write-Log "Fallback Start-Process pipeline_watchdog.ps1"
        } catch {
            Write-Log "WARN: fallback watchdog fallo: $($_.Exception.Message)"
        }
    }
}

function Start-BackfillBbddAfterDiario {
    # Encadenar cuota historica u_rafaga tras Diario sano. Separada: WaitIdle
    # no solapa DMC con ingest/gapfill. Nunca Start-Process.
    if ($DryRun) { return }
    if ($Watchdog -or $script:PipelineMode -eq "watchdog") { return }
    if ($script:PipelineTaskName -eq "Rodaja_Actualizar_Resume") { return }
    if ($ScrapeOnly) { return }
    if ($PostOnly -or $FromGapfill -or $script:PipelineMode -in @("post", "gapfill_post")) { return }
    if ($script:ExitCode -ne 0) {
        Write-Log "Pipeline no sano (exit=$script:ExitCode); no encadeno backfill BBDD"
        return
    }
    Write-Log "Encadenando backfill BBDD: schtasks /Run Rodaja_Backfill_BBDD"
    try {
        $out = & schtasks.exe /Run /TN "Rodaja_Backfill_BBDD" 2>&1
        Write-Log "schtasks /Run backfill BBDD: $out"
    } catch {
        Write-Log "WARN: no se pudo lanzar backfill BBDD: $($_.Exception.Message)"
    }
    Write-Log "Encadenando backfill rafaga horaria DGA: schtasks /Run Rodaja_Backfill_DGA_Rafaga"
    try {
        $outRaf = & schtasks.exe /Run /TN "Rodaja_Backfill_DGA_Rafaga" 2>&1
        Write-Log "schtasks /Run backfill rafaga DGA: $outRaf"
    } catch {
        Write-Log "WARN: no se pudo lanzar backfill rafaga DGA: $($_.Exception.Message)"
    }
}

function Start-BackfillDgaDireAfterIngest {
    # SNIA queda libre al cerrar el scrape. No espera al backfill DMC ni a Open-Meteo.
    if ($DryRun) { return }
    if ($Watchdog -or $script:PipelineMode -eq "watchdog") { return }
    if ($script:PipelineTaskName -eq "Rodaja_Actualizar_Resume") { return }
    if ($ScrapeOnly) { return }
    if ($PostOnly -or $FromGapfill -or $script:PipelineMode -in @("post", "gapfill_post")) { return }
    Write-Log "Encadenando backfill direccion DGA: schtasks /Run Rodaja_Backfill_DGA_Dire"
    try {
        $outDire = & schtasks.exe /Run /TN "Rodaja_Backfill_DGA_Dire" 2>&1
        Write-Log "schtasks /Run backfill direccion DGA: $outDire"
    } catch {
        Write-Log "WARN: no se pudo lanzar backfill direccion DGA: $($_.Exception.Message)"
    }
}

function Get-LogErrorLines {
    if (-not (Test-Path $LogPath)) { return "" }
    $patterns = @('\[FAIL\]', '\[ERROR\]', 'ERROR:', 'FAIL en paso', 'timedOut=true')
    $lines = Get-Content $LogPath -Encoding UTF8 -ErrorAction SilentlyContinue |
        Where-Object {
            $line = $_
            foreach ($p in $patterns) {
                if ($line -match $p) { return $true }
            }
            return $false
        } |
        Select-Object -Last 30
    ($lines -join "`n").Trim()
}

function Register-Actualizacion {
    $finishedAt = (Get-Date).ToString('o')
    $startedDt = [datetime]$script:StartedAt
    $finishedDt = [datetime]$finishedAt
    $durSec = [math]::Round(($finishedDt - $startedDt).TotalSeconds, 1)

    # ORCH_LOG_FILE_LOCKED: no persistir falso exit=1 de ingest si metrics dicen OK
    $pasosDetalle = @()
    foreach ($s in $script:StepResults) {
        $o = [ordered]@{}
        foreach ($k in @($s.Keys)) { $o[$k] = $s[$k] }
        if ("$($o.id)" -eq "ingest" -and [int]$o.exit_code -ne 0) {
            $mec = $null
            try {
                if ($script:IngestMetrics -and $script:IngestMetrics.PSObject.Properties['exit_code']) {
                    $mec = [int]$script:IngestMetrics.exit_code
                }
            } catch {}
            if ($null -ne $mec -and $mec -eq 0) {
                $o.exit_code = 0
                Write-Log "registrar: ingest exit normalizado a 0 (metrics exit=0; evita falso nodo Hub)"
            }
        }
        $pasosDetalle += $o
    }

    $pasosCompletados = @(
        $pasosDetalle |
            Where-Object { $_.exit_code -eq 0 -and -not $_.skipped } |
            ForEach-Object { $_.id }
    )

    $ingest = $script:IngestMetrics
    $filasDga = $null
    $filasDmc = $null
    $filasTotal = $null
    $estDga = $null
    $estDmc = $null
    $ultimaFecha = $null
    $metricsObj = @{}

    if ($ingest) {
        if ($ingest.PSObject.Properties['filas_afectadas_dga']) {
            $filasDga = [int64]$ingest.filas_afectadas_dga
        }
        if ($ingest.PSObject.Properties['filas_afectadas_dmc']) {
            $filasDmc = [int64]$ingest.filas_afectadas_dmc
        }
        if ($ingest.PSObject.Properties['filas_afectadas_total']) {
            $filasTotal = [int64]$ingest.filas_afectadas_total
        } elseif ($null -ne $filasDga -and $null -ne $filasDmc) {
            $filasTotal = $filasDga + $filasDmc
        }
        if ($ingest.PSObject.Properties['estaciones_dga']) {
            $estDga = [int]$ingest.estaciones_dga
        }
        if ($ingest.PSObject.Properties['estaciones_dmc']) {
            $estDmc = [int]$ingest.estaciones_dmc
        }
        if ($ingest.PSObject.Properties['ultima_fecha_observacion']) {
            $ultimaFecha = "$($ingest.ultima_fecha_observacion)"
            if ($ultimaFecha -eq "NA" -or $ultimaFecha -eq "") { $ultimaFecha = $null }
        }
        $ingest.PSObject.Properties | ForEach-Object {
            $metricsObj[$_.Name] = $_.Value
        }
    }
    $metricsObj["trigger_source"] = $script:TriggerSource
    # Documentar avg ms DGA/DMC/Open-Meteo por corrida (scrape + profile JSONL)
    try {
        $scrapeRm = $null
        if ($metricsObj.ContainsKey("response_ms")) { $scrapeRm = $metricsObj["response_ms"] }
        $fromProf = $null
        if ($script:ProfileRunId) { $fromProf = Get-HttpResponseMsFromProfile $script:ProfileRunId }
        $mergedRm = Merge-ResponseMs -FromScrape $scrapeRm -FromProfile $fromProf
        if ($mergedRm) { $metricsObj["response_ms"] = $mergedRm }
    } catch {}

    $report = [ordered]@{
        run_id                   = $script:RunId
        started_at               = $script:StartedAt
        finished_at              = $finishedAt
        exit_code                = $script:ExitCode
        skipped                  = [bool]$script:SkippedRun
        pasos_completados        = @($pasosCompletados)
        pasos_detalle            = @($pasosDetalle)
        estaciones_dga           = $estDga
        estaciones_dmc           = $estDmc
        filas_afectadas_dga      = $filasDga
        filas_afectadas_dmc      = $filasDmc
        filas_afectadas_total    = $filasTotal
        duracion_segundos        = $durSec
        ultima_fecha_observacion = $ultimaFecha
        errores                  = (Get-LogErrorLines)
        log_dia                  = "ingest/logs/actualizar_diario_$Day.log"
        job_bat                  = "actualizar_diario.bat"
        task_name                = $script:PipelineTaskName
        mode                     = $(if ($script:SkippedRun) { "skip" } else { $script:PipelineMode })
        phase                    = $script:PipelinePhase
        trigger_source           = $script:TriggerSource
        metrics                  = $metricsObj
    }

    $regOk = $false
    try {
        $json = $report | ConvertTo-Json -Depth 8 -Compress
        $regOut = @($json | docker @ComposeArgs exec -T api Rscript scripts/registrar_actualizacion_diaria.R 2>&1)
        $regRc = $LASTEXITCODE
        $regText = ($regOut | Out-String)
        $regOut | ForEach-Object { Write-Log "[registrar] $_" }
        if ($regText -match '\[registrar\] ERROR') {
            $regOk = $false
        } elseif ($regText -match '\[registrar\] OK' -or $regRc -eq 0) {
            $regOk = $true
        }
    } catch {
        Write-Log "WARN: no se pudo registrar parte_de_guardia: $($_.Exception.Message)"
    }
    # Skip de un segundo proceso no debe borrar el sidecar del dueño del lock.
    if ($regOk -and -not $script:SkippedRun -and (Test-Path $TimelineOpenPath)) {
        Remove-Item $TimelineOpenPath -Force -ErrorAction SilentlyContinue
    }
}

function Add-StepResult([string]$StepId, [string]$Label, [datetime]$Started, [datetime]$Finished, [int]$ExitCode, [bool]$Skipped) {
    $dur = [math]::Round(($Finished - $Started).TotalSeconds, 1)
    $script:StepResults += [ordered]@{
        id            = $StepId
        label         = $Label
        started_at    = $Started.ToString('o')
        finished_at   = $Finished.ToString('o')
        exit_code     = $ExitCode
        duration_sec  = $dur
        skipped       = $Skipped
    }
    Note-TimelineStepDone $StepId $Started.ToString('o') $Finished.ToString('o') $ExitCode $Skipped
}

function Find-TimelineStep([string]$StepId) {
    for ($i = 0; $i -lt $script:TimelineSteps.Count; $i++) {
        if ("$($script:TimelineSteps[$i].id)" -eq $StepId) { return $i }
    }
    return -1
}

function Write-TimelineOpenFile {
    $parts = New-Object System.Collections.Generic.List[string]
    foreach ($s in $script:TimelineSteps) {
        $fin = if ([string]::IsNullOrEmpty([string]$s.finished_at)) { "null" } else { '"' + $s.finished_at + '"' }
        $ec = if ($null -eq $s.exit_code -or "$($s.exit_code)" -eq "") { "null" } else { "$([int]$s.exit_code)" }
        $sk = if ($s.skipped) { "true" } else { "false" }
        [void]$parts.Add(('{{"id":"{0}","started_at":"{1}","finished_at":{2},"exit_code":{3},"skipped":{4}}}' -f `
            $s.id, $s.started_at, $fin, $ec, $sk))
    }
    $json = '{{"run_id":"{0}","task_name":"{1}","started_at":"{2}","steps":[{3}]}}' -f `
        $script:RunId, $script:PipelineTaskName, $script:StartedAt, ($parts -join ",")
    try {
        [System.IO.File]::WriteAllText($TimelineOpenPath, $json, [System.Text.UTF8Encoding]::new($false))
    } catch {
        Write-Log "WARN: timeline open: $($_.Exception.Message)"
    }
}

function Note-TimelineStepStart([string]$StepId) {
    $now = (Get-Date).ToString('o')
    $idx = Find-TimelineStep $StepId
    if ($idx -ge 0) {
        $script:TimelineSteps[$idx].started_at = $now
        $script:TimelineSteps[$idx].finished_at = $null
        $script:TimelineSteps[$idx].exit_code = $null
        $script:TimelineSteps[$idx].skipped = $false
    } else {
        [void]$script:TimelineSteps.Add(@{
            id          = $StepId
            started_at  = $now
            finished_at = $null
            exit_code   = $null
            skipped     = $false
        })
    }
    Write-TimelineOpenFile
}

function Note-TimelineStepDone([string]$StepId, [string]$StartedAt, [string]$FinishedAt, [int]$ExitCode, [bool]$Skipped) {
    $row = @{
        id          = $StepId
        started_at  = $StartedAt
        finished_at = $FinishedAt
        exit_code   = $ExitCode
        skipped     = [bool]$Skipped
    }
    $idx = Find-TimelineStep $StepId
    if ($idx -ge 0) { $script:TimelineSteps[$idx] = $row }
    else { [void]$script:TimelineSteps.Add($row) }
    Write-TimelineOpenFile
}

function Resolve-DockerExitCode([string]$Label, $Proc, [string]$ErrFile, [int]$DefaultExit) {
    $cli = $null
    if ($Proc) {
        try {
            if ($Proc.HasExited) {
                $Proc.Refresh()
                $raw = $Proc.ExitCode
                if ($null -ne $raw -and "$raw" -ne "") { $cli = [int]$raw }
            }
        } catch {}
    }
    if ($Label -ne "ingest") {
        if ($null -ne $cli) { return $cli }
        return $DefaultExit
    }
    # CLI Windows/docker a menudo -1 al cortar la espera tras ingest_done; el scrape
    # real esta en err.log / metrics / ckpt. No confiar en CLI != 0.
    if ($null -ne $cli -and $cli -eq 0) { return 0 }
    $fromLog = Test-IngestErrLogComplete $ErrFile
    if ($null -ne $fromLog) { return $fromLog }
    $m = $null
    try { $m = Read-IngestMetrics } catch {}
    if ($m -and $m.PSObject.Properties['exit_code']) {
        try {
            $mec = [int]$m.exit_code
            if ($mec -eq 0) { return 0 }
        } catch {}
    }
    if (Test-RIngestCheckpointComplete) { return 0 }
    if ($null -ne $cli) { return $cli }
    return $DefaultExit
}

# ponytail: PS 5.1 + $ErrorActionPreference=Stop convierte stderr de docker (p.ej.
# "No stopped containers") en excepcion terminante al redirigir 2>$null. Ceiling:
# pierdes el texto de error real; upgrade = PSNativeCommandUseErrorActionPreference
# selectivo (PS7+) o parsear exit code sin redirigir stderr.
function Invoke-DockerNoThrow {
    param([Parameter(Mandatory)][string[]]$DockerArgs)
    $prev = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    try {
        $out = & docker @DockerArgs 2>&1
        $code = $LASTEXITCODE
        return [pscustomobject]@{ ExitCode = $code; Output = $out }
    } finally {
        $ErrorActionPreference = $prev
    }
}

function Get-LastNonEmptyLogLine([string]$Path, [int]$Tail = 80) {
    $all = Get-LastNonEmptyLogLines $Path $Tail 1
    if ($all -and $all.Count -gt 0) { return $all[0] }
    return $null
}

function Get-LastNonEmptyLogLines([string]$Path, [int]$Tail = 120, [int]$Take = 5) {
    if (-not (Test-Path $Path)) { return @() }
    $lines = @(Get-Content $Path -Tail $Tail -Encoding UTF8 -ErrorAction SilentlyContinue |
        ForEach-Object { ([string]$_).Trim() } | Where-Object { $_ })
    if ($lines.Count -eq 0) { return @() }
    $n = [Math]::Min($Take, $lines.Count)
    return @($lines[($lines.Count - $n)..($lines.Count - 1)])
}

function Select-UsefulLogLine([object[]]$RawLines) {
    $picked = Select-UsefulLogLines $RawLines 1
    if ($picked -and $picked.Count -gt 0) { return $picked[0] }
    return $null
}

function Select-UsefulLogLines([object[]]$RawLines, [int]$Take = 5) {
    $lines = @($RawLines | ForEach-Object { ([string]$_).Trim() } | Where-Object { $_ })
    if ($lines.Count -eq 0) { return @() }
    $prefer = @($lines | Where-Object {
        $_ -match 'estacion|\[dga|\[dmc|PASO |ingest|alertas|pronostico|Open-Meteo'
    })
    $src = if ($prefer.Count -gt 0) { $prefer } else { $lines }
    $n = [Math]::Min($Take, $src.Count)
    return @($src[($src.Count - $n)..($src.Count - 1)])
}

function Clear-PipelineLiveTail {
    $obj = @{
        ts           = (Get-Date).ToString('o')
        line         = $null
        lines        = @()
        source       = "cleared"
        container_id = $null
        step         = $null
    }
    $obj | ConvertTo-Json -Compress | ForEach-Object {
        [System.IO.File]::WriteAllText($LiveTailPath, $_, [System.Text.UTF8Encoding]::new($false))
    }
}

# Cola del banner hub: log del paso en curso (no solo scrape DGA).
function Write-PipelineLiveTail {
    $step = $script:LastStep
    if (-not $step) {
        try {
            if (Test-Path $LockPath) {
                $lk = Get-Content $LockPath -Raw -Encoding UTF8 | ConvertFrom-Json
                if ($lk.phase -and "$($lk.phase)" -notin @("orchestrator", "running", "done")) {
                    $step = [string]$lk.phase
                }
            }
        } catch {}
    }
    if (-not $step) {
        $cp0 = Read-Ckpt
        if ($cp0["pipeline_step"]) { $step = [string]$cp0["pipeline_step"] }
    }

    $linesOut = @()
    $source = $null
    $cid = $null

    # 1) Log docker del paso actual (err luego out), si es fresco
    if ($step) {
        foreach ($kind in @("err", "out")) {
            $f = Join-Path $LogsDir "docker_${step}_${Day}.${kind}.log"
            if (-not (Test-Path $f)) { continue }
            $ageMin = ((Get-Date) - (Get-Item $f).LastWriteTime).TotalMinutes
            if ($ageMin -gt 30) { continue }
            $cand = @(Get-LastNonEmptyLogLines $f 120 5)
            if ($cand.Count -gt 0) {
                $linesOut = $cand
                $source = "docker_${step}_${kind}"
                break
            }
        }
    }

    # 2) Ingest vivo: err reciente o docker logs del container
    if ($linesOut.Count -eq 0 -and ($step -eq "ingest" -or -not $step)) {
        $errFile = Join-Path $LogsDir "docker_ingest_${Day}.err.log"
        if (Test-Path $errFile) {
            $ageMin = ((Get-Date) - (Get-Item $errFile).LastWriteTime).TotalMinutes
            if ($ageMin -le 5) {
                $cand = @(Get-LastNonEmptyLogLines $errFile 120 5)
                if ($cand.Count -gt 0) {
                    $linesOut = $cand
                    $source = "docker_ingest_err"
                }
            }
        }
        if ($linesOut.Count -eq 0) {
            try {
                if (Test-Path $LockPath) {
                    $lock = Get-Content $LockPath -Raw -Encoding UTF8 | ConvertFrom-Json
                    if ($lock.container_id) { $cid = [string]$lock.container_id }
                }
            } catch {}
            if (-not $cid) {
                $liveIds = @(Get-LiveIngestRunIds)
                if ($liveIds.Count -gt 0) { $cid = [string]$liveIds[0] }
            }
            if ($cid) {
                $job = Start-Job -ScriptBlock {
                    param($Id)
                    & docker logs --tail 40 $Id 2>&1
                } -ArgumentList $cid
                if (Wait-Job $job -Timeout 8) {
                    $picked = @(Select-UsefulLogLines @(Receive-Job $job) 5)
                    if ($picked.Count -gt 0) {
                        $linesOut = $picked
                        $source = "docker_logs"
                    }
                }
                Remove-Job $job -Force -ErrorAction SilentlyContinue
            }
        }
    }

    # 3) Checkpoint scrape (solo si el paso es ingest)
    if ($linesOut.Count -eq 0 -and ($step -eq "ingest" -or -not $step)) {
        $cp = Read-Ckpt
        $fu = $cp["fuente"]; $va = $cp["variable"]
        if ($fu -or $va) {
            $linesOut = @(("[{0}/{1}] estacion {2} {3}" -f $fu, $va, $cp["estacion_idx"], $cp["estacion_codigo"]).Trim())
            $source = "checkpoint"
        }
    }

    # 4) Log del orquestador (PASO Ã¢â‚¬Â¦ / exits de cualquier etapa)
    if ($linesOut.Count -eq 0) {
        $cand = @(Get-LastNonEmptyLogLines $LogPath 60 5)
        if ($cand.Count -gt 0) {
            $linesOut = $cand
            $source = "actualizar_diario"
        }
    }

    $line = if ($linesOut.Count -gt 0) { $linesOut[$linesOut.Count - 1] } else { $null }
    $obj = @{
        ts           = (Get-Date).ToString('o')
        line         = $line
        lines        = @($linesOut)
        source       = $source
        container_id = $cid
        step         = $step
    }
    $obj | ConvertTo-Json -Compress -Depth 4 | ForEach-Object {
        [System.IO.File]::WriteAllText($LiveTailPath, $_, [System.Text.UTF8Encoding]::new($false))
    }
}

function Set-PipelineStepRunning([string]$StepId) {
    $script:LastStep = $StepId
    Write-CkptFields @{ pipeline_step = $StepId }
    Write-LockHeartbeat @{ phase = $StepId }
    Note-TimelineStepStart $StepId
}

function Remove-OrphanIngestRuns {
    # Solo limpia exited leftovers. NUNCA stop de ingest-run vivo del dia (adopt).
    Write-Log "limpiando contenedores ingest-run detenidos (sin matar vivos)"
    $live = @(Get-LiveIngestRunIds)
    if ($live.Count -gt 0) {
        Write-Log "ingest-run vivo(s) detectado(s): $($live -join ', '); no se detienen"
    }
    # Exited leftover one-shots. "No stopped containers" = OK (nada que limpiar).
    $rmArgs = $ComposeArgs + @("--profile", "ingest", "rm", "-f")
    $rm = Invoke-DockerNoThrow $rmArgs
    $rmText = [string]($rm.Output -join " ")
    if ($rm.ExitCode -ne 0 -and $rmText -notmatch "No stopped containers") {
        Write-Log "WARN: compose rm exit=$($rm.ExitCode) ($rmText)"
    }
    return 0
}

function Wait-AdoptedIngest([string]$ContainerId, [int]$TimeoutMin) {
    $wallDl = Get-IngestWallDeadline
    Write-Log "adopt: esperando contenedor $ContainerId (timeout=${TimeoutMin}m wall_deadline=$($wallDl.ToString('HH:mm')))"
    Write-LockHeartbeat @{ phase = "adopt_ingest"; container_id = $ContainerId }
    $errFile = Join-Path $LogsDir "docker_ingest_${Day}.err.log"
    $t0 = Get-Date
    $relDeadline = $t0.AddMinutes($TimeoutMin)
    while ($true) {
        $insp = Invoke-DockerNoThrow @("inspect", "-f", "{{.State.Running}} {{.State.ExitCode}}", $ContainerId)
        $stateLine = [string]($insp.Output | Select-Object -First 1)
        $running = $stateLine -match '^\s*true\b'
        if (-not $running) {
            $exitMatch = [regex]::Match($stateLine, '(\d+)\s*$')
            $rc = if ($exitMatch.Success) { [int]$exitMatch.Groups[1].Value } else { 1 }
            $fromLog = Test-IngestErrLogComplete $errFile
            if ($null -ne $fromLog) { $rc = $fromLog }
            Write-Log "adopt: contenedor termino exit=$rc"
            return $rc
        }
        $now = Get-Date
        if ($now -ge $wallDl) {
            Write-Log "ERROR: wall deadline ingest $($IngestDeadlineLocal) en adopt; frenando contenedores"
            Stop-IngestRunContainers "wall_deadline_adopt"
            return 124
        }
        if ($now -gt $relDeadline) {
            Write-Log "ERROR: timeout adopt ingest (${TimeoutMin}m); frenando contenedores"
            Stop-IngestRunContainers "rel_timeout_adopt"
            return 124
        }
        Write-LockHeartbeat @{ phase = "adopt_ingest"; container_id = $ContainerId }
        if (Test-RIngestCheckpointComplete) {
            Write-Log "adopt: checkpoint ingest_done completo; cerrando adopt"
            return 0
        }
        $markerExit = Test-IngestErrLogComplete $errFile
        if ($null -ne $markerExit) {
            Write-Log "adopt: marcador Fin en err.log exit=$markerExit; grace 30s"
            Start-Sleep -Seconds 30
            $insp2 = Invoke-DockerNoThrow @("inspect", "-f", "{{.State.Running}}", $ContainerId)
            if ([string]($insp2.Output | Select-Object -First 1) -notmatch 'true') {
                return [int]$markerExit
            }
        }
        Start-Sleep -Seconds 60
    }
}

function Write-DockerLogsToMain([string]$OutFile, [string]$ErrFile, [string]$Label) {
    # ponytail: volcar 8k lineas de ingest via Write-Log → lock contention + ORCH_LOG_FILE_LOCKED.
    # Full log queda en docker_*_$Day.{out,err}.log; orch solo recibe cola.
    $tail = if ($Label -eq "ingest") { 80 } else { 200 }
    try {
        foreach ($f in @($OutFile, $ErrFile)) {
            if (-not (Test-Path $f)) { continue }
            Get-Content $f -Tail $tail -ErrorAction SilentlyContinue | ForEach-Object { Write-Log $_ }
        }
    } catch {
        # Write-Log ya no throw; esto es red de seguridad
        Write-Host "[WARN] Write-DockerLogsToMain ${Label}: $($_.Exception.Message)"
    }
}

function Stop-DockerProc($Proc) {
    if (-not $Proc) { return }
    try {
        if (-not $Proc.HasExited) { Stop-Process -Id $Proc.Id -Force -ErrorAction SilentlyContinue }
    } catch {}
}

function Initialize-DockerLogFiles([string]$OutFile, [string]$ErrFile) {
    foreach ($f in @($OutFile, $ErrFile)) {
        try {
            $dir = Split-Path $f -Parent
            if (-not (Test-Path $dir)) { New-Item -ItemType Directory -Path $dir -Force | Out-Null }
            [System.IO.File]::WriteAllText($f, "")
        } catch {
            Write-Log "WARN: no se pudo truncar ${f}: $($_.Exception.Message)"
        }
    }
}

function Invoke-DockerCommandSync([string[]]$DockerArgs, [string]$Label, [int]$TimeoutMin) {
    Write-Log "CMD docker $($DockerArgs -join ' ') (timeout=${TimeoutMin}m sync)"
    $outFile = Join-Path $LogsDir "docker_${Label}_${Day}.out.log"
    $errFile = Join-Path $LogsDir "docker_${Label}_${Day}.err.log"
    Initialize-DockerLogFiles $outFile $errFile
    $t0 = Get-Date
    Write-Log "[docker:$Label] start timeout=$($TimeoutMin * 60)s mode=sync"
    Write-LockHeartbeat @{ phase = $Label }

    $job = Start-Job -ScriptBlock {
        param($wd, $da, $out, $err)
        Set-Location $wd
        & docker @da 1>> $out 2>> $err
        return $LASTEXITCODE
    } -ArgumentList ($ProjectRoot.ToString()), $DockerArgs, $outFile, $errFile

    $deadline = $t0.AddMinutes($TimeoutMin)
    $timedOut = $false
    while ($true) {
        $completed = Wait-Job -Job $job -Timeout 60
        if ($completed) { break }
        Write-LockHeartbeat @{ phase = $Label }
        if ((Get-Date) -gt $deadline) {
            $timedOut = $true
            break
        }
    }
    if ($timedOut) {
        Stop-Job -Job $job | Out-Null
        Remove-Job -Job $job -Force | Out-Null
        $elapsed = ((Get-Date) - $t0).TotalMinutes
        Write-Log "[docker:$Label] end elapsed=$([math]::Round($elapsed,1))m timedOut=true"
        Write-Log "ERROR: timeout en $Label (${TimeoutMin}m)"
        Write-DockerLogsToMain $outFile $errFile $Label
        return 124
    }

    $rc = Receive-Job -Job $job
    Remove-Job -Job $job -Force | Out-Null
    if ($null -eq $rc) { $rc = 1 }
    $elapsed = ((Get-Date) - $t0).TotalMinutes
    Write-Log "[docker:$Label] end elapsed=$([math]::Round($elapsed,1))m exit=$rc timedOut=false"
    Write-DockerLogsToMain $outFile $errFile $Label
    Write-Log "$Label exit=$rc"
    return [int]$rc
}

# Ultimo hechas+fallos del log de la sombra. $null si todavia no hay linea.
function Get-ExtremosSombraDone([string]$ErrFile) {
    if (-not (Test-Path -LiteralPath $ErrFile)) { return $null }
    $best = $null
    foreach ($line in @(Get-Content -LiteralPath $ErrFile -Tail 30 -ErrorAction SilentlyContinue)) {
        if ("$line" -match 'hechas=(\d+) fallos=(\d+)') {
            $best = [int]$Matches[1] + [int]$Matches[2]
        }
    }
    return $best
}

function Invoke-DockerCommandLong([string[]]$DockerArgs, [string]$Label, [int]$TimeoutMin) {
    $wallDl = if ($Label -eq "ingest") { Get-IngestWallDeadline } else { $null }
    $wallNote = if ($wallDl) { " wall_deadline=$($wallDl.ToString('HH:mm'))" } else { "" }
    Write-Log "CMD docker $($DockerArgs -join ' ') (timeout=${TimeoutMin}m poll$wallNote)"
    $outFile = Join-Path $LogsDir "docker_${Label}_${Day}.out.log"
    $errFile = Join-Path $LogsDir "docker_${Label}_${Day}.err.log"
    Initialize-DockerLogFiles $outFile $errFile
    $t0 = Get-Date
    $timeoutSec = $TimeoutMin * 60
    Write-Log "[docker:$Label] start timeout=${timeoutSec}s pid=pending mode=poll$wallNote"
    Write-LockHeartbeat @{ phase = $Label }

    $proc = Start-Process -FilePath "docker" -ArgumentList $DockerArgs `
        -WorkingDirectory $ProjectRoot -PassThru -NoNewWindow `
        -RedirectStandardOutput $outFile -RedirectStandardError $errFile
    # ponytail: Start-Process -PassThru NO retiene ExitCode salvo que se toque .Handle
    # temprano (quirk .NET/PowerShell). Sin esto, $proc.ExitCode queda null tras salir y
    # Resolve-DockerExitCode cae al default=1, marcando FALSO FALLO en pasos no-ingest
    # (p.ej. alertas_top10) aunque el script R termine OK, deteniendo el pipeline.
    $null = $proc.Handle
    Write-Log "[docker:$Label] pid=$($proc.Id)"

    $deadline = $t0.AddSeconds($timeoutSec)
    $timedOut = $false
    $wallHit = $false
    $markerExit = $null
    # ponytail: 10 min sin una estacion que vuelva. Una estacion sana de esta red
    # tarda segundos; 3939 en frio fue 39s. Si hechas+fallos sigue subiendo, no hay reloj.
    $sombraStallMin = 10
    $sombraStall = ($Label -eq "extremos_sombra")
    $sombraProgressAt = $t0
    $sombraDone = -1
    if ($sombraStall) {
        Write-Log "extremos_sombra: sin reloj fijo; corte si hechas+fallos no sube en ${sombraStallMin}m (mata el R del contenedor)"
    }

    while (-not $proc.HasExited) {
        $now = Get-Date
        if ($wallDl -and $now -ge $wallDl) {
            $wallHit = $true
            $timedOut = $true
            Write-Log "ERROR: wall deadline ingest $($IngestDeadlineLocal); frenando CLI+contenedores"
            break
        }
        if ($sombraStall) {
            $done = Get-ExtremosSombraDone $errFile
            if ($null -ne $done -and $done -gt $sombraDone) {
                $sombraDone = $done
                $sombraProgressAt = $now
            }
            $quietMin = ($now - $sombraProgressAt).TotalMinutes
            if ($quietMin -ge $sombraStallMin) {
                $timedOut = $true
                Write-Log "ERROR: extremos_sombra sin avance hechas+fallos=$sombraDone por $([int]$quietMin)m"
                break
            }
        } elseif ($now -gt $deadline) {
            $timedOut = $true
            break
        }
        Write-LockHeartbeat
        # R ya escribiÃƒÂ³ ingest_done completo: no esperar eternamente al CLI docker colgado
        if ($Label -eq "ingest" -and (Test-RIngestCheckpointComplete)) {
            Write-Log "[docker:$Label] checkpoint ingest_done completo; cerrando espera docker"
            $null = Wait-Process -Id $proc.Id -Timeout 120 -ErrorAction SilentlyContinue
            if (-not $proc.HasExited) { Stop-DockerProc $proc }
            break
        }
        $markerExit = Test-IngestErrLogComplete $errFile
        if ($null -ne $markerExit -and $markerExit -eq 0) {
            Write-Log "[docker:$Label] marcador exit=0 en err.log; grace 120s"
            $null = Wait-Process -Id $proc.Id -Timeout 120 -ErrorAction SilentlyContinue
            if (-not $proc.HasExited) { Stop-DockerProc $proc }
            break
        }
        # Si el docker CLI muriÃƒÂ³ pero queda ingest-run vivo, adoptar
        if ($Label -eq "ingest" -and $proc.HasExited) {
            $live = @(Get-LiveIngestRunIds)
            if ($live.Count -gt 0) {
                Write-Log "WARN: docker CLI salio pero ingest-run vivo; adoptando $($live[0])"
                return (Wait-AdoptedIngest $live[0] $TimeoutMin)
            }
        }
        $null = Wait-Process -Id $proc.Id -Timeout 60 -ErrorAction SilentlyContinue
    }

    if ($timedOut -and -not $proc.HasExited) { Stop-DockerProc $proc }
    if ($timedOut) {
        $joinedArgs = ($DockerArgs -join " ")
        $rscript = $null
        if ($joinedArgs -match 'exec' -and $joinedArgs -match '([A-Za-z0-9_]+\.R)') {
            $rscript = $Matches[1]
        }
        if ($rscript) {
            try { Stop-WorkerExecByName -ComposeArgs $ComposeArgs -ScriptName $rscript } catch {
                Write-Log "WARN worker-exec-stop: $($_.Exception.Message)"
            }
        }
    }
    if ($timedOut -and $Label -eq "ingest") {
        Stop-IngestRunContainers $(if ($wallHit) { "wall_deadline" } else { "rel_timeout" })
    }

    $elapsed = ((Get-Date) - $t0).TotalMinutes
    $defaultExit = if ($timedOut) { 124 } else { 1 }
    $rc = Resolve-DockerExitCode $Label $proc $errFile $defaultExit

    if ($timedOut -and $rc -ne 0) {
        Write-Log "ERROR: timeout en $Label (${TimeoutMin}m wallHit=$wallHit)"
        $rc = Resolve-DockerExitCode $Label $proc $errFile 124
        if ($rc -eq 0) {
            Write-Log "WARN: timeout docker pero ingesta OK (log/checkpoint); continuando"
        } else {
            $rc = 124
        }
    } elseif ($rc -ne 0) {
        $resolved = Resolve-DockerExitCode $Label $proc $errFile $rc
        if ($resolved -eq 0 -and $rc -ne 0) {
            Write-Log "WARN: docker exit=$rc pero ingesta OK (log/checkpoint); continuando"
            $rc = 0
        }
    }

    Write-Log "[docker:$Label] end elapsed=$([math]::Round($elapsed,1))m exitResolved=$rc timedOut=$timedOut wallHit=$wallHit hasExited=$($proc.HasExited)"
    Write-DockerLogsToMain $outFile $errFile $Label
    Write-Log "$Label exit=$rc"
    return [int]$rc
}

function Invoke-DockerCommand([string[]]$DockerArgs, [string]$Label) {
    $timeoutMin = Get-StepTimeoutMin $Label
    # Start-Job no mata docker exec al timeout. Long mata el CLI y el R dentro del contenedor.
    return Invoke-DockerCommandLong $DockerArgs $Label $timeoutMin
}

function Ensure-WorkerPackage([string]$Pkg, [string]$InstallScript) {
    $check = $ComposeArgs + @("exec", "-T", "worker", "Rscript", "-e", "quit(status=ifelse(requireNamespace('$Pkg', quietly=TRUE), 0L, 1L))")
    $chk = Invoke-DockerNoThrow $check
    if ($chk.ExitCode -ne 0) {
        Write-Log "instalando $Pkg en worker ($InstallScript)"
        $installArgs = $ComposeArgs + @("exec", "-T", "worker", "bash", $InstallScript)
        $ins = Invoke-DockerNoThrow $installArgs
        foreach ($line in @($ins.Output)) { Write-Log $line }
        if ($ins.ExitCode -ne 0) { throw "install $Pkg fallo (exit=$($ins.ExitCode))" }
    }
}

# Lite, reporte y borde. Corre al cerrar refresh_pronostico, no al terminar
# alertas: si ese paso falla el reporte seguia con la emision anterior.
function Invoke-PronosticoReporteCacheRefresh([string]$StepId) {
    $invSpan = $null
    try {
        $invSpan = Start-ProfileSpan -Name "phase.invalidate_cache" -Cat "phase" -Parent $script:ProfileStepSpanId -Attrs @{ step = $StepId }
    } catch {}
    $invArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId $StepId -ProfileDir "/ingest_logs/profiles") + @("api", "Rscript", "scripts/invalidate_pronostico_cache.R")
    $invRc = Invoke-DockerCommand $invArgs "invalidate_pronostico_cache"
    try { Stop-ProfileSpan -Id $invSpan -AttrsExtra @{ exit_code = $invRc } | Out-Null } catch {}
    if ($invRc -ne 0) {
        Write-Log "WARN: invalidate_pronostico_cache exit=$invRc (no fatal)"
    }
    $warmSpan = $null
    try {
        $warmSpan = Start-ProfileSpan -Name "phase.warm_reporte_cache" -Cat "phase" -Parent $script:ProfileStepSpanId -Attrs @{ step = $StepId }
    } catch {}
    $warmArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId $StepId -ProfileDir "/ingest_logs/profiles") + @("api", "Rscript", "scripts/invalidate_warm_reporte_cache.R")
    $warmRc = Invoke-DockerCommand $warmArgs "invalidate_warm_reporte_cache"
    try { Stop-ProfileSpan -Id $warmSpan -AttrsExtra @{ exit_code = $warmRc } | Out-Null } catch {}
    if ($warmRc -ne 0) {
        Write-Log "WARN: invalidate_warm_reporte_cache exit=$warmRc (no fatal)"
    }
    $cfSpan = $null
    try {
        $cfSpan = Start-ProfileSpan -Name "phase.purge_cloudflare_reporte" -Cat "phase" -Parent $script:ProfileStepSpanId -Attrs @{ step = $StepId }
    } catch {}
    $purgePs1 = Join-Path $PSScriptRoot "purge_cloudflare_reporte.ps1"
    $cfRc = 0
    if (Test-Path $purgePs1) {
        $prevEap = $ErrorActionPreference
        $ErrorActionPreference = "Continue"
        try {
            Write-Log "CMD powershell -File purge_cloudflare_reporte.ps1"
            # ponytail: exit 0 de un -File hijo en la consola schtasks/bat
            # puede SetShouldExit al conhost y abortar el Diario
            # (2026-08-27: CMD purge -> FIN 2s, sin catch, sin Add-StepResult).
            # cmd /c absorbe el exit; 2>&1 deja OK/WARN en el log.
            $cfOut = & cmd.exe /c "powershell.exe -NoProfile -ExecutionPolicy Bypass -File `"$purgePs1`"" 2>&1
            foreach ($line in @($cfOut)) {
                if ("$line") { Write-Log "[cf-purge] $line" }
            }
            $cfRc = if ($null -eq $LASTEXITCODE) { 0 } else { [int]$LASTEXITCODE }
        } catch {
            Write-Log "WARN: purge_cloudflare_reporte exception: $($_.Exception.Message)"
            $cfRc = 1
        } finally {
            $ErrorActionPreference = $prevEap
        }
    } else {
        Write-Log "WARN: no existe $purgePs1"
        $cfRc = 1
    }
    try { Stop-ProfileSpan -Id $cfSpan -AttrsExtra @{ exit_code = $cfRc } | Out-Null } catch {}
    Write-Log "purge_cloudflare_reporte exit=$cfRc (no fatal)"
}

function Invoke-PipelineStep([string]$StepId) {
    switch ($StepId) {
        "preflight" {
            $ensure = Join-Path $PSScriptRoot "ensure_docker.ps1"
            & powershell -NoProfile -ExecutionPolicy Bypass -File $ensure -TimeoutSec 600 2>&1 | ForEach-Object { Write-Log $_ }
            return $LASTEXITCODE
        }
        "ingest" {
            # DGA knobs: cap 12 + adapt. DMC: pacing a presupuesto 5-15s / 6h (mismo set en watchdog).
            # -e gana sobre environment del compose por si queda un override viejo.
            $envArgs = @(
                "INGEST_MODO=completo",
                "TZ=America/Santiago",
                "INGEST_DGA_TIMEOUT_SEC=210",
                "INGEST_DGA_CONCURRENCY=12",
                "INGEST_DGA_RAMP_SEC=45",
                "INGEST_DGA_MAX_RETRIES=3",
                "INGEST_DGA_ADAPT=1",
                "INGEST_DGA_TARGET_SEC=20",
                "INGEST_DGA_MIN_INFLIGHT=2",
                "INGEST_DGA_BATCH=1",
                "INGEST_DGA_BATCH_INFLIGHT=2",
                "INGEST_SLEEP_MIN_DGA=0.4",
                "INGEST_SLEEP_MIN_DMC=5",
                "INGEST_SLEEP_MAX_DMC=15",
                "INGEST_DMC_BUDGET_SEC=21600",
                "INGEST_DMC_DEADLINE_LOCAL=09:00",
                "INGEST_DMC_DEADLINE_SLACK_SEC=2700",
                "INGEST_DMC_URLS_EST=2875",
                "INGEST_RETRY_DEAD=1"
            )
            $profArgs = Get-ProfileDockerEnvArgs -StepId "ingest" -ProfileDir "/app/ingest/logs/profiles"
            if ($DryRun) { $envArgs += "INGEST_DRY_RUN=1" }
            if ($ResetCheckpoint) { $envArgs += "INGEST_RESET_CHECKPOINT=1" }
            # Adoptar corrida viva del dia antes de lanzar otra
            $live = @(Get-LiveIngestRunIds)
            if ($live.Count -gt 0 -and -not $ResetCheckpoint) {
                Write-Log "ingest: adoptando contenedor vivo $($live[0])"
                return (Wait-AdoptedIngest $live[0] (Get-StepTimeoutMin "ingest"))
            }
            Remove-OrphanIngestRuns | Out-Null
            $dockerArgs = $ComposeArgs + @("--profile", "ingest", "run", "--rm")
            foreach ($e in $envArgs) { $dockerArgs += @("-e", $e) }
            $dockerArgs += $profArgs
            $dockerArgs += @("ingest", "Rscript", "ingest/run/actualizar_diario.R")
            return (Invoke-DockerCommand $dockerArgs "ingest")
        }
        "alertas_top10" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "alertas_top10" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/evaluar_alertas_top10.R")
            return (Invoke-DockerCommand $dockerArgs "alertas_top10")
        }
        "extremos_sombra" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "extremos_sombra" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/evaluar_extremo_sombra_dia.R")
            return (Invoke-DockerCommand $dockerArgs "extremos_sombra")
        }
        "refresh_pronostico" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "refresh_pronostico" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/refresh_pronostico.R")
            $rc = Invoke-DockerCommand $dockerArgs "refresh_pronostico"
            if ($rc -eq 0) {
                Invoke-PronosticoReporteCacheRefresh "refresh_pronostico"
            }
            return $rc
        }
        "alertas_pronostico" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "alertas_pronostico" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/evaluar_alertas_pronostico.R")
            $rc = Invoke-DockerCommand $dockerArgs "alertas_pronostico"
            if ($rc -eq 0) {
                # Rankings del pronostico completo. El reporte ya se invalido al cerrar el refresh.
                $invSpan = $null
                try {
                    $invSpan = Start-ProfileSpan -Name "phase.invalidate_cache" -Cat "phase" -Parent $script:ProfileStepSpanId -Attrs @{ step = "alertas_pronostico" }
                } catch {}
                $invArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "alertas_pronostico" -ProfileDir "/ingest_logs/profiles") + @("api", "Rscript", "scripts/invalidate_pronostico_cache.R")
                $invRc = Invoke-DockerCommand $invArgs "invalidate_pronostico_cache"
                try { Stop-ProfileSpan -Id $invSpan -AttrsExtra @{ exit_code = $invRc } | Out-Null } catch {}
                if ($invRc -ne 0) {
                    Write-Log "WARN: invalidate_pronostico_cache exit=$invRc (no fatal)"
                }
            }
            return $rc
        }
        "verificar_concordancia" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "verificar_concordancia" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/verificar_concordancia.R")
            return (Invoke-DockerCommand $dockerArgs "verificar_concordancia")
        }
        "evaluar_precision_alertas" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "evaluar_precision_alertas" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/evaluar_precision_alertas.R")
            return (Invoke-DockerCommand $dockerArgs "evaluar_precision_alertas")
        }
        "actualizar_skill_alertas" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "actualizar_skill_alertas" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/actualizar_skill_alertas.R")
            return (Invoke-DockerCommand $dockerArgs "actualizar_skill_alertas")
        }
        "senales_ensemble" {
            $dockerArgs = $ComposeArgs + @("exec", "-T") + (Get-WorkerStepEnvArgs -StepId "senales_ensemble" -ProfileDir "/ingest_logs/profiles") + @("worker", "Rscript", "run/senales_ensemble_dia.R")
            return (Invoke-DockerCommand $dockerArgs "senales_ensemble")
        }
        "bias_historico" {
            # Encadenado: tras refresh_pronostico y el ensemble la cuota Open-Meteo ya esta contabilizada.
            # El mismo script sigue con el hindcast por modelo cuando el best_match cerro.
            # Sale del Diario solo con best_match completo Y modelos_complete.
            $biasStatusPath = Join-Path $LogsDir "bias_historico.status.json"
            if (Test-Path $biasStatusPath) {
                try {
                    $biasSt = Get-Content $biasStatusPath -Raw -Encoding UTF8 | ConvertFrom-Json
                    if (Test-BiasHistoricoTerminado $biasSt) {
                        Write-Log "bias_historico: best_match y hindcast por modelo completos; paso omitido"
                        return 0
                    }
                } catch {
                    Write-Log "WARN: no se pudo leer bias_historico.status.json ($($_.Exception.Message))"
                }
            }
            $biasPs1 = Join-Path $PSScriptRoot "descargar_openmeteo_historico.ps1"
            if (-not (Test-Path $biasPs1)) {
                Write-Log "ERROR: no existe $biasPs1"
                return 1
            }
            $biasArgs = @()
            if ($DryRun) { $biasArgs += "-DryRun" }
            # Propagar perfil orch Ã¢â€ â€™ bias (mismo JSONL)
            $env:INGEST_PROFILE_RUN_ID = "$($script:ProfileRunId)"
            $env:INGEST_PROFILE_TASK = "$($script:PipelineTaskName)"
            $env:INGEST_PROFILE_MODE = "$($script:PipelineMode)"
            $env:INGEST_PROFILE_TRIGGER = "$($script:TriggerSource)"
            $env:INGEST_PROFILE_STEP = "bias_historico"
            if ($script:ProfileT0Ms) { $env:INGEST_PROFILE_T0_MS = "$($script:ProfileT0Ms)" }
            if ($script:ProfileStepSpanId) { $env:INGEST_PROFILE_PARENT = "$($script:ProfileStepSpanId)" }
            $timeoutMin = Get-StepTimeoutMin "bias_historico"
            $biasOut = Join-Path $LogsDir "docker_bias_historico_${Day}.out.log"
            $biasErr = Join-Path $LogsDir "docker_bias_historico_${Day}.err.log"
            [System.IO.File]::WriteAllText($biasOut, "")
            [System.IO.File]::WriteAllText($biasErr, "")
            $argList = @("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", $biasPs1) + @($biasArgs)
            Write-Log "CMD powershell -File descargar_openmeteo_historico.ps1 $($biasArgs -join ' ') (timeout=${timeoutMin}m)"
            $prevEap = $ErrorActionPreference
            $ErrorActionPreference = "Continue"
            $biasProc = $null
            $rc = 1
            try {
                $biasProc = Start-Process -FilePath "powershell" -ArgumentList $argList `
                    -WorkingDirectory $ProjectRoot -PassThru -NoNewWindow `
                    -RedirectStandardOutput $biasOut -RedirectStandardError $biasErr
                $null = $biasProc.Handle
                $biasDeadline = (Get-Date).AddMinutes($timeoutMin)
                $biasTimedOut = $false
                $biasOutPos = [long]0
                $biasErrPos = [long]0
                while (-not $biasProc.HasExited) {
                    $biasOutPos = Read-BoundedLogNew $biasOut $biasOutPos "bias"
                    $biasErrPos = Read-BoundedLogNew $biasErr $biasErrPos "bias"
                    if ((Get-Date) -gt $biasDeadline) {
                        $biasTimedOut = $true
                        break
                    }
                    Write-LockHeartbeat @{ phase = "bias_historico" }
                    $null = Wait-Process -Id $biasProc.Id -Timeout 60 -ErrorAction SilentlyContinue
                }
                $biasOutPos = Read-BoundedLogNew $biasOut $biasOutPos "bias"
                $biasErrPos = Read-BoundedLogNew $biasErr $biasErrPos "bias"
                if ($biasTimedOut -and -not $biasProc.HasExited) {
                    Write-Log "ERROR: timeout en bias_historico (${timeoutMin}m)"
                    & taskkill.exe /T /F /PID $biasProc.Id 2>&1 | ForEach-Object { Write-Log "$_" }
                    try { Clear-DiarioWorkerExecOrphans -ComposeArgs $ComposeArgs } catch {
                        Write-Log "WARN reap worker exec: $($_.Exception.Message)"
                    }
                    try { Stop-IngestRunContainers "bias_timeout" } catch {}
                    $rc = 124
                } else {
                    try { $rc = [int]$biasProc.ExitCode } catch { $rc = 1 }
                }
            } finally {
                $ErrorActionPreference = $prevEap
                Remove-Item Env:INGEST_PROFILE_RUN_ID -ErrorAction SilentlyContinue
                Remove-Item Env:INGEST_PROFILE_TASK -ErrorAction SilentlyContinue
                Remove-Item Env:INGEST_PROFILE_MODE -ErrorAction SilentlyContinue
                Remove-Item Env:INGEST_PROFILE_TRIGGER -ErrorAction SilentlyContinue
                Remove-Item Env:INGEST_PROFILE_STEP -ErrorAction SilentlyContinue
                Remove-Item Env:INGEST_PROFILE_T0_MS -ErrorAction SilentlyContinue
                Remove-Item Env:INGEST_PROFILE_PARENT -ErrorAction SilentlyContinue
            }
            if ($null -eq $rc) { $rc = 1 }
            return [int]$rc
        }
        default { return 1 }
    }
}

# --- main ---
Add-Content -Path $LogPath -Value "" -Encoding UTF8 -ErrorAction SilentlyContinue
Write-Log "=================================================="
Write-Log "INICIO actualizar_diario.ps1 Resume=$Resume ResetCheckpoint=$ResetCheckpoint DryRun=$DryRun ScrapeOnly=$ScrapeOnly PostOnly=$PostOnly FromGapfill=$FromGapfill TaskName=$($script:PipelineTaskName) mode=$($script:PipelineMode) trigger=$($script:TriggerSource) ingest_deadline=$IngestDeadlineLocal"
# Lock vivo de hoy: no matar su hindcast. Lock muerto o de ayer: reap.
$reapOrphans = (-not (Get-LockPidAlive)) -or (Test-LockFromPriorCalendarDay)
if ($reapOrphans) {
    try { Clear-DiarioWorkerExecOrphans -ComposeArgs $ComposeArgs } catch {
        Write-Log "WARN reap worker exec: $($_.Exception.Message)"
    }
}

if ($ScrapeOnly -and $PostOnly) {
    Write-Log "ERROR: -ScrapeOnly y -PostOnly son mutuamente excluyentes"
    exit 2
}
if ($FromGapfill -and ($ScrapeOnly -or $PostOnly)) {
    Write-Log "ERROR: -FromGapfill es mutuamente excluyente con -ScrapeOnly/-PostOnly"
    exit 2
}

$stepsToRun = $PipelineSteps
if ($ScrapeOnly) {
    $stepsToRun = @($PipelineSteps | Where-Object { $_.Id -in @("preflight", "ingest") })
} elseif ($FromGapfill) {
    # Solo alertas + concordancia; skill/bias/refresh quedan del soft-continue matutino.
    $stepsToRun = @($PipelineSteps | Where-Object { $_.Id -in @("alertas_top10", "extremos_sombra", "alertas_pronostico", "verificar_concordancia") })
} elseif ($PostOnly) {
    $stepsToRun = @($PipelineSteps | Where-Object { $_.Id -ne "preflight" })
}

# Soft-fail: estos pasos no abortan el pipeline
$NonFatalSteps = @("extremos_sombra", "verificar_concordancia", "evaluar_precision_alertas", "actualizar_skill_alertas", "senales_ensemble", "bias_historico")

if ($ResetCheckpoint) {
    Write-Log "ResetCheckpoint: borrando checkpoint"
    Clear-Ckpt
}

# ORCH_DIED_WHILE_INGEST_ALIVE: pid del lock muerto + ingest-run vivo Ã¢â€ â€™ adoptar
# (no solo con -Resume). Evita scrapes huÃƒÂ©rfanos sin post-pasos.
$liveAtStart = @(Get-LiveIngestRunIds)
$lockPidAlive = Get-LockPidAlive
$deadLock = Read-LockObject
if ($liveAtStart.Count -gt 0 -and -not $lockPidAlive -and -not $PostOnly -and -not $FromGapfill) {
    Write-Log "ORCH_DIED_WHILE_INGEST_ALIVE: lock.pid muerto con ingest-run vivo; adoptando $($liveAtStart[0])"
    if ($deadLock) { Write-OrchDiedAdoptEvent $liveAtStart[0] $deadLock }
    Remove-Lock
    New-Lock
    $rc = Wait-AdoptedIngest $liveAtStart[0] (Get-StepTimeoutMin "ingest")
    $script:IngestMetrics = Read-IngestMetrics
    $cpAdopt = Read-Ckpt
    $todayIso = (Get-Date).ToString("yyyy-MM-dd")
    $fechaFinAdopt = "$($cpAdopt['fecha_fin'])"
    $staleWindow = ($fechaFinAdopt -and $fechaFinAdopt -lt $todayIso)

    if ($staleWindow -and (Test-BeforeIngestDeadline)) {
        # Contenedor era de dia anterior: no marcar ingest done; scrape fresco hasta IngestDeadlineLocal
        Write-Log "adopt: fecha_fin=$fechaFinAdopt < hoy=$todayIso; no marcar ingest done; re-scrape fresco (deadline $($IngestDeadlineLocal))"
        Clear-PipelineDoneKeep @("preflight")
        # caer al loop normal con ingest pendiente
    } elseif ($rc -eq 0 -or $rc -eq 124 -or (Test-IngestPartialSuccess $script:IngestMetrics)) {
        if ($rc -eq 124) {
            Write-Log "adopt: wall/rel deadline (124); soft-continue post-pasos"
        } elseif ($rc -ne 0) {
            Write-Log "adopt: exit=$rc pero parcial OK; soft-continue post-pasos"
        }
        Mark-PipelineStepDone "ingest"
        $stepsToRun = @($PipelineSteps | Where-Object { $_.Id -notin @("preflight", "ingest") })
        if ($ScrapeOnly) { $stepsToRun = @() }
    } else {
        # Sin datos utiles: igual intentar post (productos con lo que haya) + vigilancia gapfill
        Write-Log "WARN: adopt exit=$rc sin partial OK; soft-continue post-pasos (Vigilancia gapfill)"
        Mark-PipelineStepDone "ingest"
        $stepsToRun = @($PipelineSteps | Where-Object { $_.Id -notin @("preflight", "ingest") })
        if ($ScrapeOnly) { $stepsToRun = @() }
        $script:ExitCode = 0
    }
} elseif (-not (Test-LockStale)) {
    if (Test-LockFromPriorCalendarDay) {
        # Manual de tarde / Vigilancia cruzando medianoche no debe omitir el Diario 00:10
        Clear-PriorDayPipelineLock
        New-Lock
    } else {
        Write-Log "Lock activo (PID vivo o container/progreso reciente); omitiendo."
        $script:SkippedRun = $true
        # No pisar status.json del dueÃƒÂ±o del lock (p.ej. Post skip mientras Scrape corre).
        Register-Actualizacion
        exit 0
    }
} else {
    Remove-Lock
    New-Lock
}

try {
    Write-Status @{ phase = "running" }

    # PostOnly: si ingest no done pero hay datos frescos / ckpt parcial, igual intentar post
    if ($PostOnly -and -not (Test-PipelineStepDone "ingest")) {
        Write-Log "PostOnly: ingest no marcado done; corriendo post-pasos si hay datos (soft)"
    }
    if ($PostOnly) {
        # Aunque Resume+PostOnly, re-ejecutar post (ckpt puede tenerlos "done" tras skip falso)
        Clear-PipelineDoneKeep @("preflight", "ingest")
        Write-Log "PostOnly: sync jornada_cerrada antes de post-pasos"
        Invoke-ObservacionDiariaSyncIfStale | Out-Null
    }
    if ($FromGapfill) {
        # Re-correr solo alertas/concordancia; conservar refresh/skill/bias ya hechos a la manana.
        Clear-PipelineDoneKeep @(
            "preflight", "ingest",
            "refresh_pronostico", "actualizar_skill_alertas", "bias_historico"
        )
        Write-Log "FromGapfill: sync + alertas/concordancia (sin skill/bias/refresh)"
        Invoke-ObservacionDiariaSyncIfStale | Out-Null
        $script:DeferDigests = $false
    } else {
        Update-DeferDigestsFlag
    }

    foreach ($step in $stepsToRun) {
        $script:LastStep = $step.Id
        $stepStarted = Get-Date
        if ($Resume -and (Test-PipelineStepDone $step.Id)) {
            if ($step.Id -eq "ingest") {
                Invoke-ObservacionDiariaSyncIfStale | Out-Null
            }
            Write-Log "PASO $($step.Id) ya completado (checkpoint); omitiendo"
            Add-StepResult $step.Id $step.Label $stepStarted (Get-Date) 0 $true
            continue
        }
        # PostOnly: no re-ejecutar scrape; contar como OK (datos de ingest previo del dia)
        if ($PostOnly -and $step.Id -eq "ingest") {
            Write-Log "PASO ingest omitido (-PostOnly); marcando OK con metrics previas"
            $script:IngestMetrics = Read-IngestMetrics
            Mark-PipelineStepDone $step.Id
            Add-StepResult $step.Id $step.Label $stepStarted (Get-Date) 0 $false
            continue
        }
        Write-Log "PASO $($step.Id) - $($step.Label)"
        Set-PipelineStepRunning $step.Id
        $rc = 1
        $spanId = $null
        $script:ProfileStepSpanId = $null
        try { $spanId = Start-ProfileSpan -Name ("step.{0}" -f $step.Id) -Cat "step" -Attrs @{ step = $step.Id } } catch {}
        $script:ProfileStepSpanId = $spanId
        try {
            $rc = Invoke-PipelineStep $step.Id
        } catch {
            $exMsg = "$($_.Exception.Message)"
            Write-Log "ERROR en paso $($step.Id): $exMsg"
            $rc = 1
            # ORCH_LOG_FILE_LOCKED: excepcion de I/O tras scrape OK no debe tumbar el paso
            if ($step.Id -eq "ingest" -and (Test-OrchLogFileLockMessage $exMsg)) {
                $recovered = Resolve-IngestExitAfterOrchNoise
                if ($null -ne $recovered) {
                    Write-Log "WARN: ORCH_LOG_FILE_LOCKED; recuperando ingest rc=$recovered desde metrics/err.log"
                    $rc = [int]$recovered
                }
            }
        }
        try { Stop-ProfileSpan -Id $spanId -AttrsExtra @{ exit_code = $rc } | Out-Null } catch {}
        $script:ProfileStepSpanId = $null
        Add-StepResult $step.Id $step.Label $stepStarted (Get-Date) $rc $false
        if ($step.Id -eq "ingest") {
            $script:IngestMetrics = Read-IngestMetrics
            if ($rc -ne 0) {
                $recovered = Resolve-IngestExitAfterOrchNoise
                if ($null -ne $recovered -and [int]$recovered -eq 0) {
                    Write-Log "WARN: ingest rc=$rc pero scrape OK (log/metrics/ckpt); normalizando rc=0"
                    $rc = 0
                    Set-LastStepResultExit "ingest" 0
                }
            }
        }
        if ($rc -ne 0) {
            if ($NonFatalSteps -contains $step.Id) {
                Write-Log "WARN: paso $($step.Id) exit=$rc (non-fatal); continuando"
                Mark-PipelineStepDone $step.Id
                continue
            }
            # Deadline de pared (124): cortar ingest y seguir con productos
            if ($step.Id -eq "ingest" -and $rc -eq 124) {
                Write-Log "WARN: ingest wall/rel deadline (124); continuando post-pasos"
                Mark-PipelineStepDone $step.Id
                Update-DeferDigestsFlag
                try { Start-BackfillDgaDireAfterIngest } catch { Write-Log "WARN: backfill direccion DGA: $($_.Exception.Message)" }
                continue
            }
            # Soft-fail parcial: una fuente OK basta para no tumbar alertas/pronostico
            if ($step.Id -eq "ingest" -and (Test-IngestPartialSuccess $script:IngestMetrics)) {
                $ed = 0; $em = 0
                try { $ed = [int]$script:IngestMetrics.estaciones_dga } catch {}
                try { $em = [int]$script:IngestMetrics.estaciones_dmc } catch {}
                Write-Log "WARN: ingest exit=$rc pero parcial OK (dga_est=$ed dmc_est=$em); continuando post-pasos"
                # Si metrics dicen exit=0 (scrape OK), no dejar STEP_INGEST_FAIL en day_health
                $mec = $null
                try {
                    if ($script:IngestMetrics -and $script:IngestMetrics.PSObject.Properties['exit_code']) {
                        $mec = [int]$script:IngestMetrics.exit_code
                    }
                } catch {}
                if ($null -ne $mec -and $mec -eq 0) {
                    Set-LastStepResultExit "ingest" 0
                    Write-Log "ingest: soft-continue con metrics exit=0; step_result=0 (evita falso STEP_INGEST_FAIL)"
                }
                Mark-PipelineStepDone $step.Id
                Update-DeferDigestsFlag
                try { Start-BackfillDgaDireAfterIngest } catch { Write-Log "WARN: backfill direccion DGA: $($_.Exception.Message)" }
                continue
            }
            $script:ExitCode = $rc
            Write-Log "FAIL en paso $($step.Id) (exit=$rc)"
            break
        }
        Mark-PipelineStepDone $step.Id
        if ($step.Id -eq "ingest") {
            Update-DeferDigestsFlag
            try { Start-BackfillDgaDireAfterIngest } catch { Write-Log "WARN: backfill direccion DGA: $($_.Exception.Message)" }
        }
    }

    if ($script:ExitCode -eq 0 -and -not $ScrapeOnly) {
        try { Write-PipelineDayHealthAndReport } catch { Write-Log "WARN: day_health: $($_.Exception.Message)" }
        $stillIncomplete = @($script:IngestVarKeys | Where-Object { (Get-CompletedVarsFromCkpt) -notcontains $_ })
        if ($stillIncomplete.Count -gt 0) {
            Write-Log "Pipeline exit=0 pero incomplete_vars=$($stillIncomplete -join ','); conservando checkpoint para Vigilancia"
            $script:ProfileOutcome = "parcial"
        } else {
            Write-Log "Pipeline completo; limpiando checkpoint"
            Clear-Ckpt
            $script:ProfileOutcome = "ok"
        }
    } elseif ($script:ExitCode -eq 0 -and $ScrapeOnly) {
        try { Write-PipelineDayHealthAndReport } catch { Write-Log "WARN: day_health: $($_.Exception.Message)" }
        Write-Log "ScrapeOnly OK; checkpoint conservado para post"
        $script:ProfileOutcome = "ok"
    } else {
        try { Write-PipelineDayHealthAndReport } catch { Write-Log "WARN: day_health: $($_.Exception.Message)" }
        Write-Log "Pipeline incompleto; checkpoint conservado para resume"
        $script:ProfileOutcome = if ($script:ExitCode -ne 0) { "error" } else { "parcial" }
    }
} finally {
    Remove-Lock
    try { Clear-PipelineLiveTail } catch {}
    Write-Log "RESUMEN final=$script:ExitCode last_step=$script:LastStep"
    Write-Log "FIN actualizar_diario"
    Write-Status @{
        phase       = "done"
        finished_at = (Get-Date).ToString('o')
        exit_code   = $script:ExitCode
        last_step   = $script:LastStep
    }
    try {
        $stepSummaries = @($script:StepResults | ForEach-Object {
            [ordered]@{
                id           = $_.id
                label        = $_.label
                duration_sec = $_.duration_sec
                exit_code    = $_.exit_code
                skipped      = $_.skipped
            }
        })
        $po = if ($script:ProfileOutcome) { "$($script:ProfileOutcome)" } else {
            if ($script:ExitCode -eq 0) { "ok" } else { "error" }
        }
        Complete-OrchProfileSummary -Status $po -Steps $stepSummaries
    } catch {}
    Register-Actualizacion
    try { Start-VigilanciaAfterDiario } catch { Write-Log "WARN: vigilancia: $($_.Exception.Message)" }
    try { Start-BackfillBbddAfterDiario } catch { Write-Log "WARN: backfill BBDD: $($_.Exception.Message)" }
}

exit $script:ExitCode
