-- Tabla para trabajos de descarga (API /solicitar-descarga)
-- Ejecutar: psql -U postgres -d clima -f migration/trabajos_descarga.sql

CREATE TABLE IF NOT EXISTS trabajos_descarga (
  job_id TEXT PRIMARY KEY,
  estado TEXT NOT NULL DEFAULT 'pendiente',
  link_descarga TEXT,
  creado_en TIMESTAMPTZ NOT NULL,
  completado_en TIMESTAMPTZ,
  expira_en TIMESTAMPTZ NOT NULL,
  estacion_ids JSONB NOT NULL,
  variable_ids JSONB NOT NULL,
  fecha_inicio DATE NOT NULL,
  fecha_fin DATE NOT NULL,
  email TEXT,
  formato TEXT NOT NULL CHECK (formato IN ('csv', 'xlsx'))
);

CREATE INDEX IF NOT EXISTS idx_trabajos_descarga_estado ON trabajos_descarga(estado);
CREATE INDEX IF NOT EXISTS idx_trabajos_descarga_creado ON trabajos_descarga(creado_en);
