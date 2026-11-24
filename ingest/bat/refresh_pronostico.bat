@echo off
setlocal
cd /d "%~dp0..\.."
if not exist "ingest\logs" mkdir "ingest\logs"
for /f "tokens=1-3 delims=/ " %%a in ('date /t') do set D=%%c%%b%%a
set LOG=ingest\logs\pronostico_%D%.log
where Rscript >nul 2>&1
if errorlevel 1 (
  echo Rscript no encontrado en PATH. >> "%LOG%"
  exit /b 1
)
echo [%date% %time%] Inicio refresh pronostico >> "%LOG%"
Rscript worker\run\refresh_pronostico.R %* >> "%LOG%" 2>&1
set RC=%ERRORLEVEL%
echo [%date% %time%] Fin refresh exit=%RC% >> "%LOG%"
exit /b %RC%
