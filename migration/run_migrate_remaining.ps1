# Run migration for remaining tables only
$Rscript = "C:\Program Files\R\R-4.3.0\bin\x64\Rscript.exe"
Set-Location $PSScriptRoot
& $Rscript migration.R --remaining 2>&1 | Tee-Object -FilePath migrate_remaining_output.txt
if ($LASTEXITCODE -eq 0) { Write-Host "Done. Run compare_row_counts.R to verify." -ForegroundColor Green }
else { Write-Host "Failed. Check migrate_remaining_output.txt" -ForegroundColor Red }
exit $LASTEXITCODE
