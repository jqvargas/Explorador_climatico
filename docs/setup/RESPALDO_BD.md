# Respaldo de la base de datos (pendiente)

**Estado:** sin respaldo. Pendiente hasta tener dónde guardarlo (otro disco o la nube).
**Anotado:** 2026-11-24.

## Situación actual

- La base `clima` (TimescaleDB + PostGIS, contenedor `clima_db`) pesa unos **37 GB** y vive solo en `./pgdata`, en el disco `C:` de este equipo.
- No hay ninguna tarea programada que haga `pg_dump`, ni una copia en otra máquina, disco o nube.
- Si el disco falla, se roban el equipo o `pgdata` se corrompe, **se pierde todo**: observaciones, usuarios, suscripciones, pronósticos archivados y métricas de alertas. El repo no sirve para reconstruirla: no guarda el esquema ni los datos.

## Por qué no está hecho

No hay otro disco ni almacenamiento en la nube disponible. Un respaldo en el mismo disco `C:` no protege de una falla del disco, que es el riesgo principal.

## Qué hacer cuando haya recursos

1. **Destino.** Un disco externo o NAS, o un bucket en la nube (S3, Backblaze B2, Google Cloud Storage). Calcular unos 15–25 GB por copia comprimida, más espacio para varias copias.
2. **Copia.** Un `pg_dump` en formato custom, comprimido, desde el contenedor:
   ```powershell
   docker exec clima_db pg_dump -U postgres -d clima -Fc -Z 6 -f /tmp/clima.dump
   docker cp clima_db:/tmp/clima.dump D:\respaldos\clima_$(Get-Date -Format yyyyMMdd).dump
   docker exec clima_db rm /tmp/clima.dump
   ```
   TimescaleDB pide restaurar en una base con la misma versión de la extensión y usar `timescaledb_pre_restore()` y `timescaledb_post_restore()`. Ver la documentación de TimescaleDB, "Backup and restore".
3. **Tarea programada.** Registrar `Rodaja_Respaldo_BD`, semanal o diaria fuera del horario de `Rodaja_Actualizar_Diario`, que haga la copia y borre las más viejas (por ejemplo, guardar 4 semanales y 3 mensuales).
4. **Fuera del equipo.** Al menos una copia tiene que quedar fuera de este PC.
5. **Probar la restauración.** Restaurar una copia en una base aparte y comprobar los conteos de `bitacora_oficial`, `jornada_cerrada`, `mirador` y `tripulacion`. Un respaldo que nunca se restauró no está probado.
6. **Cerrar.** Marcar el ítem en [`LAUNCH_BACKLOG.md`](../producto/LAUNCH_BACKLOG.md) §8 y actualizar esta nota con el destino, la frecuencia y la fecha de la última prueba.
