# Handoff etapa 5 — `guia_privacidad`

**Fecha:** 2026-11-15
**Status en cola:** `hecho_con_gaps`
**Plan:** `docs/producto/widget_pronostico/etapa_5_plan.md`

## Qué se hizo

- Guía de emulador y de Samsung: `docs/producto/widget_pronostico/guia_prueba.md`. `adb emu geo fix` con longitud primero. Casos de permiso concedido, permiso denegado, sin conexión, cambio de ubicación, estación fijada, modo oscuro y tap a la TWA.
- Data safety y el párrafo para pegar al subir el AAB: `docs/producto/widget_pronostico/data_safety.md`. Ubicación aproximada, recopilada, no compartida, funcionalidad de la app, solo en primer plano. El id no es ubicación. La página legal live no se editó.
- C15 pasó a `ok`, con la ejecución humana en la columna de gap. La etapa queda `hecho_con_gaps` porque esas casillas siguen abiertas.

## Qué se verificó

- [x] Los dos markdown cubren el DoD. Búsqueda: `adb emu geo fix`, permiso concedido y denegado, sin conexión, Valparaíso y el punto en el Pacífico, estación fijada `S.e=3428`, `cmd uimode night`, el tap a Mi reporte, y el párrafo de privacidad.
- [x] `git diff -- nginx/html/legal/privacidad/index.html` no trae cambios de esta etapa (el archivo no se escribió).
- [x] C15 ya no está `pendiente`. C01–C14 y C16 no se reabrieron.
- [x] No se tocó la API, el manifest, `minSdk`, assetlinks, el splash ni `start_url`. No hubo restart. No se publicó un AAB.

## Gaps / caveats / qué puede fallar después (obligatorio)

Nadie recorrió la guía. El texto de Play y el de la web están escritos y sin aplicar.

| Riesgo | Señal | Impacto | Mitigación o etapa dueña |
|--------|-------|---------|--------------------------|
| APK debug sobre la app de Play | `INSTALL_FAILED_UPDATE_INCOMPATIBLE` | Hay que desinstalar la de la tienda | Emulador vacío, o el AAB de internal testing. La guía lo dice |
| Barra de Chrome dentro de rodaja en el debug | El tap abre la app pero se ve la barra | Digital Asset Links no verifica la firma de debug | No editar `assetlinks.json`. Lo confirma el AAB firmado |
| Modo avión y esperar | La tarjeta no dice «Sin conexión» | El periódico no corre sin red | Quitar y volver a poner el widget, como la guía |
| «Usar la más cercana» tras 10 km | La estación cambia | Ese botón recalcula siempre. Los 25 km no se ven ahí | No tratarlo como bug. El umbral quedó en las pruebas JVM de la etapa 2 |
| Formulario de Play | «Recopilada» exige decir que el dato sale del teléfono | La coordenada no sale | No inventar un envío. La ficha dice qué responder |
| `bubblewrap build` | Borra los bloques `RODAJA_WIDGET_*` | Se pierde el widget en el AAB | Reponerlos a mano. Tarea humana, no de la etapa 6 |
| Privacidad publicada antes del AAB | La web habla de un permiso que la app de Play todavía no pide | El texto y el binario quedan desfasados | Pegar el párrafo en el mismo momento de subir el AAB |

## TAREAS HUMANAS

- [ ] Recorrer `guia_prueba.md` en un emulador con Google Play (API 31 o más): permiso concedido, permiso denegado, sin conexión, cambio de ubicación, estación fijada, modo oscuro, tap.
- [ ] Los mismos casos en un Samsung con One UI.
- [ ] Subir versionCode a 8 y correr `bubblewrap build`. Si regenera, reponer a mano los bloques `RODAJA_WIDGET_*`.
- [ ] Internal testing en Play Console. Data safety como `data_safety.md`.
- [ ] Al subir el AAB, pegar en la privacidad live el párrafo y el ítem de finalidad de `data_safety.md`.

## Pendiente / no es de esta etapa

- La etapa 6 relee los handoffs, repite el curl de los dos GET y `assembleDebug`, y escribe `CIERRE.md`. No marca estas casillas como hechas.
- No hay código nuevo.

## Para la etapa siguiente

- No reabrir: minSdk 21, Glance 1.1.1, sin foreground service, `updatePeriodMillis` 0, sin lat/lon en ninguna URL, sin ubicación precisa ni en segundo plano, default 3939, el theme de `<application>`, `twa_boot`, el splash, assetlinks.
- El HTML de Mi reporte ya está en producción con el bypass `view=pronostico` + `e`. Sigue fuera del commit de la etapa 4: al commitear esa página, conservar `_viewPron`.
- Archivos tocados: `guia_prueba.md`, `data_safety.md`, `etapa_5_plan.md`, este handoff, `criterios.md` (C15), `widget_pronostico_cola.json`.
- Comandos útiles para el cierre, no corridos aquí:
  - `curl.exe -sD - https://app.rodaja.cl/api/widget/estaciones/3939/hoy`
  - `cd mobile/twa` y `.\gradlew.bat assembleDebug --console=plain`

## Decisiones de producto tomadas aquí

Ninguna. El párrafo repite el plan maestro. 3428 (Punta Arenas) y 1098 (Valparaíso) son estaciones del asset de la etapa 1, elegidas para que la prueba tenga un id real. No reemplazan a la 3939.
