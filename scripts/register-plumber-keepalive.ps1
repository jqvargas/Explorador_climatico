# Registrar tarea programada para mantener Plumber caliente
# Ejecutar como Administrador desde PowerShell:
#   Set-Location "C:\Users\rodaja\Explorador_climatico\scripts"
#   .\register-plumber-keepalive.ps1
#
# O desde CMD (como Admin): scripts\register-plumber-keepalive.bat

$uri = "http://localhost:8000"
$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument "-NoProfile -Command `"try { Invoke-WebRequest -Uri '$uri/ping' -UseBasicParsing -TimeoutSec 5 } catch {}`""
$trigger = New-ScheduledTaskTrigger -Once -At (Get-Date) -RepetitionInterval (New-TimeSpan -Minutes 5)
Register-ScheduledTask -TaskName "PlumberKeepAlive" -Action $action -Trigger $trigger -Force
Write-Host "Tarea PlumberKeepAlive registrada. Se ejecutara cada 5 minutos."
