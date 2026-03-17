-- Ãndices para acelerar la consulta de estaciones (endpoint /estaciones)
-- Ejecutar una vez contra la base de datos: psql -U postgres -d clima -f add_indexes_estaciones.sql

-- Para el subquery cuando se filtra por id_variable: SELECT DISTINCT id_estacion FROM observacion_final WHERE id_variable = ?
CREATE INDEX IF NOT EXISTS idx_observacion_final_var_est 
ON observacion_final (id_variable, id_estacion);

-- Para EXISTS (id_estacion, id_variable) cuando se filtran operador+variable
CREATE INDEX IF NOT EXISTS idx_observacion_final_est_var 
ON observacion_final (id_estacion, id_variable);

-- Para filtrar por operador/fuente: WHERE e.id_fuente = ?
CREATE INDEX IF NOT EXISTS idx_estacion_id_fuente 
ON estacion (id_fuente);

