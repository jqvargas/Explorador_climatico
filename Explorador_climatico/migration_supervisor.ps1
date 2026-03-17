# Migration Supervisor Script
# Runs every 1 hour, checks if migration stopped, resumes up to 3 times on failure, then stops.
# Usage: .\migration_supervisor.ps1
# Run in background: Start-Process powershell -ArgumentList "-File migration_supervisor.ps1" -WindowStyle Hidden

$MigDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$Rscript = "C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
$MigScript = Join-Path $MigDir "migration.R"
$CheckpointFile = Join-Path $MigDir "migration_checkpoint.txt"
$StateFile = Join-Path $MigDir "migration_supervisor_state.txt"
$LogFile = Join-Path $MigDir "migration_supervisor.log"
$IntervalMinutes = 60
$MaxResumeAttempts = 3
$StabilityWaitSeconds = 120  # Wait this long to consider migration "stable"

function Write-SupervisorLog {
    param([string]$Message)
    $ts = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $line = "$ts $Message"
    Add-Content -Path $LogFile -Value $line
    Write-Host $line
}

function Get-FailedAttempts {
    if (-not (Test-Path $StateFile)) { return 0 }
    $content = Get-Content $StateFile -Raw
    if ($content -match "failed_attempts=(\d+)") { return [int]$Matches[1] }
    return 0
}

function Set-FailedAttempts {
    param([int]$Count)
    Set-Content -Path $StateFile -Value "failed_attempts=$Count`nlast_update=$(Get-Date -Format 'o')"
}

function Test-MigrationComplete {
    if (-not (Test-Path $CheckpointFile)) { return $false }
    $cp = Get-Content $CheckpointFile
    # Migration complete when observacion_modelo has 1559 (total chunks)
    foreach ($line in $cp) {
        if ($line -match "observacion_modelo:(\d+)") {
            $chunk = [int]$Matches[1]
            if ($chunk -ge 1559) { return $true }
        }
    }
    return $false
}

function Start-Migration {
    $logPath = Join-Path $MigDir "migration_log.txt"
    $errPath = Join-Path $MigDir "migration_err.txt"
    Start-Process -FilePath $Rscript -ArgumentList $MigScript -NoNewWindow -RedirectStandardOutput $logPath -RedirectStandardError $errPath -PassThru
}

function Get-MigrationProcess {
    Get-Process -Name "Rscript" -ErrorAction SilentlyContinue | Where-Object {
        try { $_.Path -like "*R*" } catch { $true }
    }
}

# Main loop
Set-Location $MigDir
Write-SupervisorLog "Migration supervisor started. Checking every $IntervalMinutes minutes. Max resume attempts: $MaxResumeAttempts"

while ($true) {
    Start-Sleep -Seconds ($IntervalMinutes * 60)

    if (Test-MigrationComplete) {
        Write-SupervisorLog "Migration appears complete (observacion_modelo done). Supervisor exiting."
        exit 0
    }

    $proc = Get-MigrationProcess
    if ($proc) {
        Write-SupervisorLog "Migration is running (PID $($proc.Id)). Resetting failed attempts counter."
        Set-FailedAttempts 0
        continue
    }

    # Migration not running
    $attempts = Get-FailedAttempts
    if ($attempts -ge $MaxResumeAttempts) {
        Write-SupervisorLog "FINAL STOP: $MaxResumeAttempts failed resume attempts. Supervisor exiting."
        exit 1
    }

    $attempts++
    Set-FailedAttempts $attempts
    Write-SupervisorLog "Migration not running. Resume attempt $attempts of $MaxResumeAttempts. Starting migration..."

    $p = Start-Migration
    Start-Sleep -Seconds $StabilityWaitSeconds

    $stillRunning = Get-MigrationProcess
    if ($stillRunning) {
        Write-SupervisorLog "Migration started (PID $($stillRunning.Id)). Reset failed attempts."
        Set-FailedAttempts 0
    } else {
        Write-SupervisorLog "Resume attempt $attempts failed - migration exited within $StabilityWaitSeconds seconds."
    }
}
