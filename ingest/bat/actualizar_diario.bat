@echo off
REM Wrapper: delega al orquestador PowerShell robusto.
setlocal
cd /d "%~dp0..\.."
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0..\scripts\actualizar_diario.ps1" %*
exit /b %ERRORLEVEL%
