-- Indices para acelerar consultas de estaciones y observaciones
CREATE INDEX IF NOT EXISTS idx_estacion_bbox ON estacion(lat, lon);
CREATE INDEX IF NOT EXISTS idx_observacion_estacion ON observacion_final(id_estacion);
