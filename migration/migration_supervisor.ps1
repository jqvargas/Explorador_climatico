# Migration Supervisor Script
# Runs every 1 hour, checks if migration stopped, resumes up to 3 times on failure, then stops.
# Usage: .\migration_supervisor.ps1
# Run in background: Start-Process powershell -ArgumentList "-File migration_supervisor.ps1" -WindowStyle Hidden
# Quick one-time run: .\run_migration_now.ps1

$MigDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Rscript = "C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
$MigScript = Join-Path $MigDir "migration.R"
$CheckpointFile = Join-Path $MigDir "migration_checkpoint.txt"
$StateFile = Join-Path $MigDir "migration_supervisor_state.txt"
$LogFile = Join-Path $MigDir "migration_supervisor.log"
$IntervalMinutes = 60
$MaxResumeAttempts = 3
$StabilityWaitSeconds = 120

function Write-SupervisorLog { param([string]$Message); $ts = Get-Date -Format "yyyy-MM-dd HH:mm:ss"; Add-Content -Path $LogFile -Value "$ts $Message"; Write-Host "$ts $Message" }
function Get-FailedAttempts { if (-not (Test-Path $StateFile)) { return 0 }; $c = Get-Content $StateFile -Raw; if ($c -match "failed_attempts=(\d+)") { return [int]$Matches[1] }; return 0 }
function Set-FailedAttempts { param([int]$Count); Set-Content -Path $StateFile -Value "failed_attempts=$Count" }
function Test-MigrationComplete { if (-not (Test-Path $CheckpointFile)) { return $false }; (Get-Content $CheckpointFile) | ForEach-Object { if ($_ -match "observacion_modelo:(\d+)") { if ([int]$Matches[1] -ge 1559) { return $true } } }; return $false }
function Start-Migration { Start-Process -FilePath $Rscript -ArgumentList $MigScript -NoNewWindow -Wait -RedirectStandardOutput (Join-Path $MigDir "migration_log.txt") -RedirectStandardError (Join-Path $MigDir "migration_err.txt") }
function Get-MigrationProcess { Get-Process -Name "Rscript" -ErrorAction SilentlyContinue }

Set-Location $MigDir
Write-SupervisorLog "Supervisor started. Interval: $IntervalMinutes min. Max attempts: $MaxResumeAttempts"

while ($true) {
    Start-Sleep -Seconds ($IntervalMinutes * 60)
    if (Test-MigrationComplete) { Write-SupervisorLog "Migration complete. Exiting."; exit 0 }
    $proc = Get-MigrationProcess
    if ($proc) { Write-SupervisorLog "Migration running (PID $($proc.Id))."; Set-FailedAttempts 0; continue }
    $attempts = Get-FailedAttempts
    if ($attempts -ge $MaxResumeAttempts) { Write-SupervisorLog "FINAL STOP: $MaxResumeAttempts failed attempts."; exit 1 }
    $attempts++; Set-FailedAttempts $attempts
    Write-SupervisorLog "Resume attempt $attempts of $MaxResumeAttempts..."
    Start-Migration; Start-Sleep -Seconds $StabilityWaitSeconds
    if (Get-MigrationProcess) { Write-SupervisorLog "Resumed OK."; Set-FailedAttempts 0 } else { Write-SupervisorLog "Attempt $attempts failed." }
}
