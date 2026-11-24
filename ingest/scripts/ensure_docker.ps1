# Levanta Docker y el stack minimo para ingesta diaria.
param(
    [int]$TimeoutSec = 300
)

$ErrorActionPreference = "Stop"
$ProjectRoot = Resolve-Path (Join-Path $PSScriptRoot "..\..")
Set-Location $ProjectRoot

# docker compose escribe progreso en stderr; no abortar por eso
$script:ErrorActionPreference = "Continue"
$script:StartedAt = (Get-Date).ToString('o')
$script:RunId = (Get-Date).ToString('yyyyMMddTHHmmss')
$script:ExitCode = 0
$ComposeArgs = @("compose", "-f", "docker-compose.yml", "-f", "docker-compose.prod.yml")
$LogsDir = Join-Path $ProjectRoot "ingest\logs"
if (-not (Test-Path $LogsDir)) { New-Item -ItemType Directory -Path $LogsDir -Force | Out-Null }

function Write-Log([string]$Msg) {
    $line = "[$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')] $Msg"
    Write-Host $line
}

function Register-WarmupActualizacion {
    $finishedAt = (Get-Date).ToString('o')
    $durSec = [math]::Round(([datetime]$finishedAt - [datetime]$script:StartedAt).TotalSeconds, 1)
    $report = [ordered]@{
        run_id            = $script:RunId
        started_at        = $script:StartedAt
        finished_at       = $finishedAt
        exit_code         = $script:ExitCode
        skipped           = $false
        pasos_completados = @(if ($script:ExitCode -eq 0) { "warmup" } else { @() })
        pasos_detalle     = @(
            [ordered]@{
                id           = "warmup"
                label        = "ensure_docker"
                started_at   = $script:StartedAt
                finished_at  = $finishedAt
                exit_code    = $script:ExitCode
                duration_sec = $durSec
                skipped      = $false
            }
        )
        duracion_segundos = $durSec
        errores           = $(if ($script:ExitCode -ne 0) { "ensure_docker exit=$($script:ExitCode)" } else { "" })
        log_dia           = "ingest/logs/ensure_docker_warmup.log"
        job_bat           = "ensure_docker.ps1"
        task_name         = "Rodaja_Docker_Warmup"
        mode              = "warmup"
        phase             = "done"
        metrics           = @{ timeout_sec = $TimeoutSec }
    }
    try {
        $json = $report | ConvertTo-Json -Depth 6 -Compress
        $prev = $ErrorActionPreference
        $ErrorActionPreference = "Continue"
        try {
            $json | docker @ComposeArgs exec -T api Rscript scripts/registrar_actualizacion_diaria.R 2>&1 |
                ForEach-Object { Write-Log "[registrar] $_" }
        } finally {
            $ErrorActionPreference = $prev
        }
    } catch {
        Write-Log "WARN: no se pudo registrar warmup: $($_.Exception.Message)"
    }
}

function Test-DockerDaemon {
    try {
        docker info 2>$null | Out-Null
        return $LASTEXITCODE -eq 0
    } catch {
        return $false
    }
}

function Wait-DockerDaemon([int]$MaxSec) {
    $deadline = (Get-Date).AddSeconds($MaxSec)
    while ((Get-Date) -lt $deadline) {
        if (Test-DockerDaemon) { return $true }
        Start-Sleep -Seconds 10
    }
    return $false
}

Write-Log "ensure_docker: inicio (timeout=${TimeoutSec}s)"

& (Join-Path $ProjectRoot "scripts\fix-docker-credentials.ps1") | Out-Null
if ($LASTEXITCODE -ne 0) {
    Write-Log "ERROR: fix-docker-credentials fallo"
    $script:ExitCode = 1
    Register-WarmupActualizacion
    exit 1
}

$dockerSvc = Get-Service -Name "com.docker.service" -ErrorAction SilentlyContinue
if ($dockerSvc) {
    try {
        Set-Service -Name "com.docker.service" -StartupType Automatic -ErrorAction SilentlyContinue
    } catch {}
    if ($dockerSvc.Status -ne "Running") {
        Write-Log "Iniciando com.docker.service..."
        try { Start-Service -Name "com.docker.service" -ErrorAction Stop } catch {
            Write-Log "WARN: no se pudo iniciar com.docker.service: $($_.Exception.Message)"
        }
    }
}

if (-not (Test-DockerDaemon)) {
    $taskName = "Start_Docker_Desktop_56984"
    Write-Log "Lanzando Docker Desktop como 56984 (tarea $taskName)..."
    try {
        Start-ScheduledTask -TaskName $taskName -ErrorAction Stop
    } catch {
        Write-Log "ERROR: no se pudo iniciar tarea ${taskName}: $($_.Exception.Message)"
        $script:ExitCode = 1
        Register-WarmupActualizacion
        exit 1
    }
}

if (-not (Wait-DockerDaemon $TimeoutSec)) {
    Write-Log "ERROR: Docker daemon no respondio en ${TimeoutSec}s"
    $script:ExitCode = 1
    Register-WarmupActualizacion
    exit 1
}
Write-Log "Docker daemon OK"

$compose = @(
    "compose", "-f", "docker-compose.yml", "-f", "docker-compose.prod.yml"
)

Write-Log "docker compose up -d --scale api=2 (timescaledb redis worker api)..."
$upOut = & docker @compose up -d --scale api=2 timescaledb redis worker api 2>&1
foreach ($line in $upOut) { Write-Log $line }
if ($LASTEXITCODE -ne 0) {
    Write-Log "ERROR: docker compose up fallo (exit=$LASTEXITCODE)"
    $script:ExitCode = $LASTEXITCODE
    Register-WarmupActualizacion
    exit $script:ExitCode
}
# nginx debe recrearse para refrescar IPs de api tras scale
Write-Log "docker compose up -d --force-recreate nginx..."
$ngOut = & docker @compose up -d --force-recreate nginx 2>&1
foreach ($line in $ngOut) { Write-Log $line }

$deadline = (Get-Date).AddSeconds($TimeoutSec)
$dbOk = $false
$workerOk = $false
while ((Get-Date) -lt $deadline) {
    $dbHealth = (& docker @compose ps timescaledb --format "{{.Health}}" 2>$null | Select-Object -First 1)
    $workerState = (& docker @compose ps worker --format "{{.State}}" 2>$null | Select-Object -First 1)
    $dbOk = ($dbHealth -eq "healthy")
    $workerOk = ($workerState -eq "running")
    Write-Log "health: timescaledb=$dbHealth worker=$workerState"
    if ($dbOk -and $workerOk) { break }
    Start-Sleep -Seconds 10
}

if (-not $dbOk -or -not $workerOk) {
    Write-Log "ERROR: stack no healthy (db=$dbOk worker=$workerOk)"
    $script:ExitCode = 1
    Register-WarmupActualizacion
    exit 1
}

Write-Log "ensure_docker: OK"
$script:ExitCode = 0
Register-WarmupActualizacion
exit 0
