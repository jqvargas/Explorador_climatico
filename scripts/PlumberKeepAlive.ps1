# PlumberKeepAlive.ps1 - Mantiene caliente la API Plumber cada 5 minutos
# Ejecutar como Administrador para registrar la tarea programada:
#
#   Register-ScheduledTask -TaskName "PlumberKeepAlive" -Action (New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-File `"$PSScriptRoot\PlumberKeepAlive.ps1`"") -Trigger (New-ScheduledTaskTrigger -Once -At (Get-Date) -RepetitionInterval (New-TimeSpan -Minutes 5)) -Force
#
# O ejecutar manualmente para probar:
#   .\PlumberKeepAlive.ps1

$uri = if ($env:API_URL) { $env:API_URL -replace "/$", "" } else { "http://localhost:8000" }
try {
    Invoke-WebRequest -Uri "$uri/ping" -UseBasicParsing -TimeoutSec 5 | Out-Null
} catch { }
