# Endurece tareas ya registradas (StopOnIdleEnd=false, StartWhenAvailable, limite tiempo).
# Requiere la misma contraseña usada al registrar.
#
#   .\ingest\scripts\harden_scheduled_tasks.ps1
#   .\ingest\scripts\harden_scheduled_tasks.ps1 -UserName "javier_zenbook\rodaja" -Password "tu_clave"

param(
    [string]$UserName = "",
    [string]$Password = ""
)

$ErrorActionPreference = "Stop"

function ConvertFrom-SecureStringPlain([Security.SecureString]$Secure) {
    $bstr = [Runtime.InteropServices.Marshal]::SecureStringToBSTR($Secure)
    try { [Runtime.InteropServices.Marshal]::PtrToStringAuto($bstr) }
    finally { [Runtime.InteropServices.Marshal]::ZeroFreeBSTR($bstr) }
}

if (-not $UserName) {
    $UserName = (& whoami.exe).Trim()
    Write-Host "Usuario: $UserName"
}
if (-not $Password) {
    Write-Host "Contrasena de Windows:"
    $Password = ConvertFrom-SecureStringPlain (Read-Host -AsSecureString)
}

# Minutes; MultipleInstances 1 = Queue. Scrape/Post se endurecen si existen (quedan disabled).
$limits = @{
    "Rodaja_Docker_Warmup"               = 60
    "Rodaja_Actualizar_Diario"           = 1440
    "Rodaja_Actualizar_Resume"           = 900
    "Rodaja_Actualizar_Reportes_Mensual" = 1440
    "Rodaja_Generar_SEO"                 = 60
    "Rodaja_Ingest_Scrape"               = 1440
    "Rodaja_Ingest_Post"                 = 240
}

$svc = New-Object -ComObject Schedule.Service
$svc.Connect()
$folder = $svc.GetFolder("\")

foreach ($tn in $limits.Keys) {
    try {
        $task = $folder.GetTask($tn)
    } catch [System.IO.FileNotFoundException] {
        Write-Host "SKIP: tarea no existe: $tn"
        continue
    }
    $def = $task.Definition
    $def.Settings.IdleSettings.StopOnIdleEnd = $false
    $def.Settings.StartWhenAvailable = $true
    $def.Settings.DisallowStartIfOnBatteries = $false
    $def.Settings.StopIfGoingOnBatteries = $false
    $def.Settings.MultipleInstances = 1
    $mins = $limits[$tn]
    $def.Settings.ExecutionTimeLimit = "PT${mins}M"
    $folder.RegisterTaskDefinition($tn, $def, 4, $UserName, $Password, 1) | Out-Null
    Write-Host "OK: $tn (limit=${mins}m Queue)"
}

try {
    Enable-ScheduledTask -TaskName "Rodaja_Actualizar_Diario" -ErrorAction SilentlyContinue | Out-Null
    Write-Host "ENABLED: Rodaja_Actualizar_Diario"
} catch {}

foreach ($splitTn in @("Rodaja_Ingest_Scrape", "Rodaja_Ingest_Post")) {
    try {
        Disable-ScheduledTask -TaskName $splitTn -ErrorAction SilentlyContinue | Out-Null
        Write-Host "DISABLED: $splitTn (split)"
    } catch {}
}

try {
    & (Join-Path $PSScriptRoot "export_schtasks_health.ps1")
} catch {
    Write-Host "WARN: export_schtasks_health: $($_.Exception.Message)"
}

Write-Host ""
schtasks /Query /TN "Rodaja_Actualizar_Diario" /XML 2>$null | Select-String "StopOnIdleEnd|StartWhenAvailable|ExecutionTimeLimit|MultipleInstances"

$Password = $null
