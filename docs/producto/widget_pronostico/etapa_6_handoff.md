# Handoff etapa 6 — `cierre_qa`

**Fecha:** 2026-11-16
**Status en cola:** `hecho`
**Plan:** `docs/producto/widget_pronostico/etapa_6_plan.md`

## Qué se hizo

- Se reclamó la 6. La 5 está `hecho_con_gaps`. No había otra etapa `en_curso`.
- Se releyeron los handoffs 1–5, `criterios.md` y el §1 del master. No se tocó código, la API, el manifest ni la privacidad live.
- Se re-midieron los dos GET, el 400 y el 404, y `assembleDebug`. El resultado está en `docs/producto/widget_pronostico/CIERRE.md`.
- C16 pasó a `ok`. C01–C15 no se reescribieron: la re-medición las sostiene.
- Las cinco casillas humanas de la etapa 5 se copiaron al cierre sin tildar.

## Qué se verificó

- [x] `curl.exe` a `https://app.rodaja.cl/api/widget/estaciones` → 200, `Cache-Control: public, max-age=3600`, 1975 objetos `id/name/comuna/lat/lon`, sin `rankings`, incluye 3939, 188103 bytes. `X-Cache: MISS` en 15.0 s y después `HIT` en 0.99 s. Nginx local, ya en HIT: 0.25 s.
- [x] `curl.exe` a `https://app.rodaja.cl/api/widget/estaciones/3939/hoy` → 200, `max-age=600`. `station_id` 3939, `current_temp` 13.2, `tmin` 9.8, `tmax` 21.7, `precip_prob` 67, `precip_mm` 0.3, `condition_code` 3, `is_day` 1, `updated_at` `2026-11-16T07:32:20-03:00`. Sin `lat`, `lon` ni `rankings`. MISS 2.69 s, HIT 0.83 s. Nginx local, HIT: 0.21 s.
- [x] `…/abc/hoy` → 400 `{"error":"id inválido"}`, `Cache-Control: no-store`.
- [x] `…/999999999/hoy` dos veces → 404 `{"error":"Sin pronóstico para esta estación"}`, `no-store`, `X-Cache: MISS` las dos.
- [x] Desde `mobile/twa`: `.\gradlew.bat assembleDebug --console=plain` → **BUILD SUCCESSFUL** in 1m. 35 tasks UP-TO-DATE. Gradle 8.11.1.
- [x] `minSdkVersion 21` y `versionCode 7` en `mobile/twa/app/build.gradle` líneas 76 y 78. Manifest empaquetado debug: `minSdkVersion="21"`, `ACCESS_COARSE_LOCATION`, sin `ACCESS_FINE_LOCATION`, sin `ACCESS_BACKGROUND_LOCATION` y sin `SystemForegroundService`. Los tres `autoVerify` https siguen dentro de `LauncherActivity`. `rodaja`/`pin` no tiene `autoVerify`. `updatePeriodMillis="0"`.
- [x] C16 ya no está `pendiente`. No hubo restart de api ni de shiny.

## Gaps / caveats / qué puede fallar después (obligatorio)

El DoD de esta etapa está cumplido. Lo que sigue ya estaba en los handoffs anteriores y se deja medido o listado.

| Riesgo | Señal | Impacto | Mitigación o etapa dueña |
|--------|-------|---------|--------------------------|
| Catálogo frío | MISS público 15.0 s el 2026-11-16. El 2026-11-15, en nginx, fueron 6.3 s. Hoy el MISS local no se repitió: la segunda lectura ya era HIT | Quien pegue el GET del catálogo en un navegador espera. El widget no lo hace: usa el asset | No abrir otro índice en esta cola. El HIT de la hora baja de 1 s |
| Sin teléfono | Las casillas de `guia_prueba.md` siguen vacías | El deep link, el pin y el modo oscuro del widget no se vieron en un Samsung | Operador. No marcarlas hechas desde aquí |
| `bubblewrap build` | Borra los bloques `RODAJA_WIDGET_*` | El AAB puede salir sin widget | Reponerlos a mano al subir versionCode a 8 |
| `index.html` de Mi reporte | El bypass `_viewPron` está servido y fuera del commit de la etapa 4 | Un commit posterior de esa página puede perder la condición | Conservar `_viewPron` cuando se commitee Mi reporte |
| Pruebas JVM | `assembleDebug` no las corre | Un cambio futuro puede romper el parser sin que este cierre lo vea | La última verde es la etapa 4, 2026-11-15 |

## TAREAS HUMANAS

Heredadas de la etapa 5. Siguen abiertas.

- [ ] Recorrer `guia_prueba.md` en un emulador con Google Play (API 31 o más): permiso concedido, permiso denegado, sin conexión, cambio de ubicación, estación fijada, modo oscuro, tap.
- [ ] Los mismos casos en un Samsung con One UI.
- [ ] Subir versionCode a 8 y correr `bubblewrap build`. Si regenera, reponer a mano los bloques `RODAJA_WIDGET_*`.
- [ ] Internal testing en Play Console. Data safety como `data_safety.md`.
- [ ] Al subir el AAB, pegar en la privacidad live el párrafo y el ítem de finalidad de `data_safety.md`.

## Pendiente / no es de esta etapa

- No hay etapa 7. La cola termina en este cierre.
- Publicar el AAB, Play Console, keystore y la página legal live.

## Para la etapa siguiente

- No reabrir: minSdk 21, default 3939, Glance 1.1.1, sin foreground service, `updatePeriodMillis` 0, sin lat/lon en `/hoy`, sin ubicación precisa ni en segundo plano, un solo `autoVerify` https (el de `LauncherActivity`), theme de `<application>`, splash, `start_url`, `twa_boot`, assetlinks.
- Archivos tocados: `etapa_6_plan.md`, `etapa_6_handoff.md`, `CIERRE.md`, `criterios.md` (C16), `widget_pronostico_cola.json`.
- Comandos útiles:
  - `curl.exe -sD - https://app.rodaja.cl/api/widget/estaciones/3939/hoy`
  - `cd mobile/twa` y `.\gradlew.bat assembleDebug --console=plain`

## Decisiones de producto tomadas aquí

Ninguna. 3939 sigue siendo la DMC Quinta Normal que resolvió la etapa 1. Los números del 2026-11-16 (13.2 °C ahora, 9.8 / 21.7) reemplazan al ejemplo del 2026-11-15 en el cierre; el contrato no cambia.
