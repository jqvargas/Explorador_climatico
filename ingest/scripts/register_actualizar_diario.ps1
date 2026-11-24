# Registrar tareas: Warmup, Rodaja_Actualizar_Diario (pipeline 00:10), Resume, Reportes.
# Deshabilita el split Scrape/Post si existen.
# Consola (SSH-friendly; no usa Get-Credential / popup GUI).
#
#   Set-Location C:\Users\rodaja\Explorador_climatico
#   .\ingest\scripts\register_actualizar_diario.ps1
#
# Opcional sin prompts:
#   .\ingest\scripts\register_actualizar_diario.ps1 -UserName "Javier_Zenbook\rodaja" -Password "tu_clave"

param(
    [string]$UserName = "",
    [string]$Password = ""
)

$ErrorActionPreference = "Stop"
$ProjectRoot = Resolve-Path (Join-Path $PSScriptRoot "..\..")
$EnsureScript = Join-Path $PSScriptRoot "ensure_docker.ps1"
$MainScript = Join-Path $PSScriptRoot "actualizar_diario.ps1"
$ReportesMensualScript = Join-Path $PSScriptRoot "actualizar_reportes_mensual.ps1"

function ConvertFrom-SecureStringPlain([Security.SecureString]$Secure) {
    $bstr = [Runtime.InteropServices.Marshal]::SecureStringToBSTR($Secure)
    try { [Runtime.InteropServices.Marshal]::PtrToStringAuto($bstr) }
    finally { [Runtime.InteropServices.Marshal]::ZeroFreeBSTR($bstr) }
}

function Get-DefaultTaskUser {
    # SSH suele dejar USERDOMAIN=WORKGROUP (invalido). whoami devuelve PC\usuario real.
    $who = (& whoami.exe 2>$null)
    if ($who) { return $who.Trim() }
    [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
}

function Resolve-TaskUser([string]$Candidate) {
    $try = @(
        $Candidate,
        (Get-DefaultTaskUser),
        "$env:COMPUTERNAME\$env:USERNAME",
        ".\$env:USERNAME"
    ) | Where-Object { $_ -and $_.Trim() -ne "" } | Select-Object -Unique

    foreach ($name in $try) {
        try {
            $acct = New-Object System.Security.Principal.NTAccount($name)
            $null = $acct.Translate([System.Security.Principal.SecurityIdentifier])
            return $acct.Value
        } catch {}
    }
    throw "No se pudo resolver cuenta Windows valida. Pruebe -UserName 'NombrePC\rodaja'"
}

function Read-ConsoleCredential {
    param([string]$DefaultUser)

    if (-not $script:UserName) {
        Write-Host "Cuenta detectada: $DefaultUser"
        $prompt = "Usuario Windows [Enter=aceptar]"
        $typed = Read-Host $prompt
        $script:UserName = if ($typed) { $typed.Trim() } else { $DefaultUser }
    }
    $script:UserName = Resolve-TaskUser $script:UserName
    Write-Host "Usando cuenta: $script:UserName"

    if (-not $script:Password) {
        Write-Host "Contrasena de Windows (no se muestra mientras escribes):"
        $secure = Read-Host -AsSecureString
        if (-not $secure -or $secure.Length -eq 0) {
            throw "Contrasena vacia; cancelado."
        }
        $script:Password = ConvertFrom-SecureStringPlain $secure
    }
}

function Harden-ScheduledTaskCom {
    param(
        [string]$TaskName,
        [int]$ExecutionMinutes,
        [switch]$ClearTriggers
    )
    $svc = New-Object -ComObject Schedule.Service
    $svc.Connect()
    $folder = $svc.GetFolder("\")
    $def = $folder.GetTask($TaskName).Definition
    $def.Settings.IdleSettings.StopOnIdleEnd = $false
    $def.Settings.StartWhenAvailable = $true
    $def.Settings.DisallowStartIfOnBatteries = $false
    $def.Settings.StopIfGoingOnBatteries = $false
    # 1 = Queue (no StopExisting): no matar ingest largo a las 01:05
    $def.Settings.MultipleInstances = 1
    $def.Settings.ExecutionTimeLimit = "PT${ExecutionMinutes}M"
    if ($ClearTriggers) {
        # On-demand: schtasks /Run y encadenado desde Diario; sin horario diario.
        while ($def.Triggers.Count -gt 0) { $def.Triggers.Remove(1) }
    }
    # 4=update, 1=TASK_LOGON_PASSWORD
    $folder.RegisterTaskDefinition(
        $TaskName, $def, 4,
        $UserName, $Password, 1
    ) | Out-Null
}

function Register-TaskViaSchtasks {
    param(
        [string]$TaskName,
        [string]$ScriptPath,
        [string]$ExtraArgs,
        [string]$StartTime,
        [int]$ExecutionMinutes = 1440,
        [string]$Schedule = "DAILY",
        [int]$DayOfMonth = 1
    )
    $tr = "powershell.exe -NoProfile -ExecutionPolicy Bypass -File `"$ScriptPath`" $ExtraArgs"
    if ($Schedule -eq "ONDEMAND") {
        # schtasks exige un schedule al crear; Harden limpia triggers -> solo /Run.
        $out = & schtasks.exe /Create /TN $TaskName /TR $tr /SC ONCE /ST 00:00 /SD 01/01/2099 `
            /RU $UserName /RP $Password /F 2>&1
    } elseif ($Schedule -eq "MONTHLY") {
        $out = & schtasks.exe /Create /TN $TaskName /TR $tr /SC MONTHLY /D $DayOfMonth /ST $StartTime `
            /RU $UserName /RP $Password /F 2>&1
    } else {
        $out = & schtasks.exe /Create /TN $TaskName /TR $tr /SC DAILY /ST $StartTime `
            /RU $UserName /RP $Password /F 2>&1
    }
    $out | ForEach-Object { Write-Host $_ }
    if ($LASTEXITCODE -ne 0) {
        throw "schtasks /Create fallo para $TaskName (exit=$LASTEXITCODE)"
    }
    Harden-ScheduledTaskCom -TaskName $TaskName -ExecutionMinutes $ExecutionMinutes `
        -ClearTriggers:($Schedule -eq "ONDEMAND")
}

function Show-TaskSummary([string]$TaskName) {
    if ($LASTEXITCODE -ne 0) { return }
    schtasks /Query /TN $TaskName /V /FO LIST 2>$null |
        Select-String "Nombre de tarea|Hora pr|Estado|Tarea que se ejecutar|ltimo resultado"
}

$defaultUser = Get-DefaultTaskUser

Write-Host "Proyecto: $ProjectRoot"
Write-Host "Registrando tareas programadas (credenciales en consola)..."
Read-ConsoleCredential -DefaultUser $defaultUser

try {
    Set-Service -Name "com.docker.service" -StartupType Automatic -ErrorAction SilentlyContinue
} catch {
    Write-Host "WARN: no se pudo configurar com.docker.service: $($_.Exception.Message)"
}

Register-TaskViaSchtasks `
    -TaskName "Rodaja_Docker_Warmup" `
    -ScriptPath $EnsureScript `
    -ExtraArgs "-TimeoutSec 600" `
    -StartTime "00:00" `
    -ExecutionMinutes 60
Write-Host "OK: Rodaja_Docker_Warmup (diaria 00:00, 10 min antes del Diario)"

# Pipeline diario unico 00:10. Vigilancia se encadena al terminar el Diario
# (schtasks /Run); sin horario diario propio.
Register-TaskViaSchtasks `
    -TaskName "Rodaja_Actualizar_Diario" `
    -ScriptPath $MainScript `
    -ExtraArgs "-Resume -TaskName Rodaja_Actualizar_Diario -TriggerSource scheduler" `
    -StartTime "00:10" `
    -ExecutionMinutes 1440
Write-Host "OK: Rodaja_Actualizar_Diario (diaria 00:10, pipeline completo)"

Register-TaskViaSchtasks `
    -TaskName "Rodaja_Actualizar_Resume" `
    -ScriptPath $MainScript `
    -ExtraArgs "-Watchdog -TaskName Rodaja_Actualizar_Resume -TriggerSource scheduler -DeadlineLocal 20:00" `
    -StartTime "00:00" `
    -Schedule "ONDEMAND" `
    -ExecutionMinutes 900
Write-Host "OK: Rodaja_Actualizar_Resume (Vigilancia: solo encadenada al Diario /Run, gapfill hasta 20:00)"

Register-TaskViaSchtasks `
    -TaskName "Rodaja_Actualizar_Reportes_Mensual" `
    -ScriptPath $ReportesMensualScript `
    -ExtraArgs "" `
    -StartTime "02:00" `
    -ExecutionMinutes 1440 `
    -Schedule "MONTHLY" `
    -DayOfMonth 1
Write-Host "OK: Rodaja_Actualizar_Reportes_Mensual (dia 1, 02:00)"

# Split Scrape/Post: desactivar (no borrar; health detecta drift si se re-habilitan).
foreach ($splitTn in @("Rodaja_Ingest_Scrape", "Rodaja_Ingest_Post")) {
    try {
        $t = Get-ScheduledTask -TaskName $splitTn -ErrorAction SilentlyContinue
        if ($t) {
            Disable-ScheduledTask -TaskName $splitTn -ErrorAction Stop | Out-Null
            Write-Host "DISABLED: $splitTn (split; usar Rodaja_Actualizar_Diario)"
        } else {
            Write-Host "SKIP: $splitTn no existe (OK)"
        }
    } catch {
        Write-Host "WARN: no se pudo desactivar ${splitTn}: $($_.Exception.Message)"
    }
}

try {
    & (Join-Path $PSScriptRoot "export_schtasks_health.ps1")
} catch {
    Write-Host "WARN: export_schtasks_health: $($_.Exception.Message)"
}

Write-Host ""
Write-Host "=== Tareas registradas ==="
foreach ($tn in @(
    "Rodaja_Docker_Warmup", "Rodaja_Actualizar_Diario",
    "Rodaja_Actualizar_Resume", "Rodaja_Actualizar_Reportes_Mensual"
)) {
    Show-TaskSummary $tn
}

Write-Host ""
Write-Host "Probar pipeline: schtasks /Run /TN `"Rodaja_Actualizar_Diario`""
Write-Host "Probar vigilancia: schtasks /Run /TN `"Rodaja_Actualizar_Resume`""
Write-Host "  (solo encadenada al terminar el Diario; sin horario diario)"
Write-Host "Probar mensual: schtasks /Run /TN `"Rodaja_Actualizar_Reportes_Mensual`""
Write-Host "Backfill BBDD (cuota u_rafaga): ingest\scripts\register_backfill_bbdd.ps1 -CurrentUser"

$Password = $null
