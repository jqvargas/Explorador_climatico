# Migration Scripts

All migration-related scripts for moving data from the source PostgreSQL database to TimescaleDB (clima).

## How to run

From the project root or migration folder:

  cd migration
  Rscript migration.R

Or: .\migration\run_migrate_remaining.ps1

Supervisor (auto-resume): .\migration\migration_supervisor.ps1

## Prerequisites

- SSH tunnel to source database active
- TimescaleDB running (docker-compose up)
- Env: sql-backend/ATT15580.env
