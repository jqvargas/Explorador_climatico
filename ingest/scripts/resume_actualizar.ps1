# Utilidades de recuperacion para actualizacion diaria.
param(
    [switch]$Status,
    [switch]$Resume,
    [switch]$Reset,
    [switch]$ClearLock
)

$ProjectRoot = Resolve-Path (Join-Path $PSScriptRoot "..\..")
$LogsDir = Join-Path $ProjectRoot "ingest\logs"
$StatusPath = Join-Path $LogsDir "actualizar_diario.status.json"
$CkptPath = Join-Path $LogsDir "actualizar_diario.checkpoint"
$LockPath = Join-Path $LogsDir "actualizar_diario.lock"
$MainScript = Join-Path $PSScriptRoot "actualizar_diario.ps1"

if ($Status) {
    Write-Host "=== status.json ==="
    if (Test-Path $StatusPath) { Get-Content $StatusPath -Raw } else { Write-Host "(no existe)" }
    Write-Host "=== checkpoint ==="
    if (Test-Path $CkptPath) { Get-Content $CkptPath -Raw } else { Write-Host "(no existe)" }
    Write-Host "=== lock ==="
    if (Test-Path $LockPath) { Get-Content $LockPath -Raw } else { Write-Host "(no existe)" }
    exit 0
}

if ($ClearLock) {
    if (Test-Path $LockPath) { Remove-Item $LockPath -Force; Write-Host "Lock eliminado." }
    else { Write-Host "Sin lock." }
}

$args = @()
if ($Resume) { $args += "-Resume" }
if ($Reset) { $args += "-ResetCheckpoint" }

if ($args.Count -eq 0 -and -not $ClearLock) {
    Write-Host @"
Uso:
  .\ingest\scripts\resume_actualizar.ps1 -Status
  .\ingest\scripts\resume_actualizar.ps1 -Resume
  .\ingest\scripts\resume_actualizar.ps1 -Reset
  .\ingest\scripts\resume_actualizar.ps1 -ClearLock
"@
    exit 0
}

if ($args.Count -gt 0) {
    & powershell -NoProfile -ExecutionPolicy Bypass -File $MainScript @args
    exit $LASTEXITCODE
}
