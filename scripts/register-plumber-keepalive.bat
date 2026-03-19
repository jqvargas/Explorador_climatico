@echo off
REM Registrar tarea keepalive - Ejecutar como Administrador
cd /d "%~dp0.."
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0register-plumber-keepalive.ps1"
pause
