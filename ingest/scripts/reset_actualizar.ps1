# Cancela corrida en curso y deja listo para empezar desde cero.
# Uso: .\ingest\scripts\reset_actualizar.ps1

$ErrorActionPreference = "Continue"
$ProjectRoot = Resolve-Path (Join-Path $PSScriptRoot "..\..")
$LogsDir = Join-Path $ProjectRoot "ingest\logs"

Write-Host "1. Detener tarea programada (si corre)..."
schtasks /End /TN "Rodaja_Actualizar_Diario" 2>$null | Out-Null

Write-Host "2. Detener contenedores ingest..."
docker ps -q --filter "name=ingest-run" 2>$null | ForEach-Object {
    docker stop $_ 2>$null | Out-Null
}

Write-Host "3. Limpiar lock, checkpoint y status..."
Remove-Item (Join-Path $LogsDir "actualizar_diario.lock") -Force -ErrorAction SilentlyContinue
Remove-Item (Join-Path $LogsDir "actualizar_diario.checkpoint") -Force -ErrorAction SilentlyContinue
$statusObj = [ordered]@{
    phase       = "idle"
    message     = "reset manual; listo para corrida limpia"
    reset_at    = (Get-Date).ToString("o")
    exit_code   = $null
    last_step   = $null
    started_at  = $null
    finished_at = $null
}
[System.IO.File]::WriteAllText(
    (Join-Path $LogsDir "actualizar_diario.status.json"),
    ($statusObj | ConvertTo-Json),
    [System.Text.UTF8Encoding]::new($false)
)

Write-Host ""
Write-Host "Listo. Ejecuta actualizar_diario.bat (o la tarea) para una corrida limpia."
