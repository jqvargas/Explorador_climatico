# Cierre — widget de pronóstico

**Fecha:** 2026-11-16, ~09:04 hora de Chile.
**Cola:** etapa 6 `hecho`. Etapas 4 y 5 siguen `hecho_con_gaps`.
**Plan:** [etapa_6_plan.md](etapa_6_plan.md).
**Vara:** [PLAN_WIDGET_PRONOSTICO.md](../PLAN_WIDGET_PRONOSTICO.md) §1. Baseline 2026-11-14. Esta etapa no cambió código.

## Perfectas

Cumplen la vara con la medición de esta mañana.

1. **Los dos GET públicos siguen el contrato.** `https://app.rodaja.cl/api/widget/estaciones` → 200, `Cache-Control: public, max-age=3600`, 1975 objetos con claves `id`, `name`, `comuna`, `lat`, `lon`, sin `rankings`, incluye 3939. Cuerpo 188103 bytes. `X-Cache: MISS` en 15.0 s y el siguiente `HIT` en 0.99 s. `https://app.rodaja.cl/api/widget/estaciones/3939/hoy` → 200, `max-age=600`, sin `lat`, `lon` ni `rankings`:

```json
{"station_id":3939,"name":"Quinta Normal Santiago","comuna":"Estación Central","current_temp":13.2,"tmin":9.8,"tmax":21.7,"precip_prob":67,"precip_mm":0.3,"condition_code":3,"is_day":1,"updated_at":"2026-11-16T07:32:20-03:00"}
```

   MISS 2.69 s, HIT 0.83 s. En nginx local, ya en HIT: catálogo 0.25 s, hoy 0.21 s.

2. **El error de negocio no queda cacheado como 200.** `…/abc/hoy` → 400 `{"error":"id inválido"}`, `Cache-Control: no-store`. `…/999999999/hoy` dos veces → 404 `{"error":"Sin pronóstico para esta estación"}`, `no-store`, `X-Cache: MISS` las dos.

3. **`assembleDebug` compila y minSdk sigue 21.** Desde `mobile/twa`: `.\gradlew.bat assembleDebug --console=plain` → **BUILD SUCCESSFUL** in 1m (35 tasks, todas UP-TO-DATE). Gradle 8.11.1. `app/build.gradle` línea 76 `minSdkVersion 21`, línea 78 `versionCode 7`. El manifest empaquetado de debug dice `android:minSdkVersion="21"`, permiso `ACCESS_COARSE_LOCATION`, y los tres `autoVerify` https están dentro de `cl.rodaja.app.LauncherActivity` (host de la TWA, `accounts.rodaja.cl`, `clerk.rodaja.cl`). `WidgetOfferActivity` tiene `scheme=rodaja` `host=pin` sin `autoVerify`. `pronostico_widget_info.xml` sigue con `updatePeriodMillis="0"`.

4. **La guía y Data safety están escritas.** Commit `cb6d8ce`: `guia_prueba.md` y `data_safety.md`. `nginx/html/legal/` no se editó en esta cola. El AAB no se publicó.

## Críticas

Ninguna contra la vara del §1. El catálogo y el pronóstico de hoy responden con el contrato, y el APK de debug se arma con minSdk 21.

## Aceptadas

Quedan abiertas. No tumban este cierre. El detalle está en los handoffs 1, 3, 4 y 5.

| Gap | Qué se midió el 2026-11-16 | Por qué se acepta |
|-----|----------------------------|-------------------|
| Nadie colocó el widget ni concedió el permiso | No hay corrida de `guia_prueba.md`. versionCode sigue 7 | La cola no publica el AAB. Las cinco casillas de la etapa 5 siguen sin tildar |
| Catálogo frío lento | MISS público 15.0 s (188103 bytes). HIT 0.99 s. El MISS de nginx del 2026-11-15 fue 6.3 s; hoy no se repitió ese MISS local porque Redis ya tenía la hora | El teléfono usa `estaciones_pronostico.json`. El worker pide `/hoy` del id, no el catálogo. No se tocó el SQL |
| Pruebas JVM no repetidas hoy | `assembleDebug` no corre `test`. La última verde es la etapa 4, 2026-11-15: 0 failures en debug y release | No hubo diff de Java/Kotlin en esta etapa |
| El bypass de Mi reporte no está en el commit de la etapa 4 | No se volvió a abrir el navegador. El HTML servido ya se midió el 2026-11-15 | Al commitear `nginx/html/mi-reporte/index.html`, conservar `_viewPron`. El archivo sigue mezclado con otro diff |
| `bubblewrap build` puede borrar `RODAJA_WIDGET_*` | No se corrió bubblewrap | Lo repone el operador al armar el AAB. versionCode 8 es de esa tarea |
| Firma de debug y Digital Asset Links | No se instaló un APK | La barra de Chrome en un debug no se “arregla” editando `assetlinks.json`. Lo ve el AAB firmado |

## La vara del §1, en una línea

| # | Listo | Estado |
|---|--------|--------|
| 1 | `assembleDebug` con minSdk 21 | Perfecta (1m, UP-TO-DATE, manifest 21) |
| 2 | Pruebas JVM de haversine, selección y parseo | Aceptada: verdes el 2026-11-15; no se relanzaron |
| 3 | Los dos GET en `app.rodaja.cl`, sin login, con Cache-Control, sin rankings | Perfecta |
| 4 | Guía de emulador y texto de Data safety | Perfecta (commit `cb6d8ce`; el operador no la recorrió) |
| 5 | El AAB no se publica en esta cola | Perfecta: versionCode 7, sin `bubblewrap build` |

## TAREAS HUMANAS

Copiadas de la etapa 5. Esta etapa no las tilda.

- [ ] Recorrer `guia_prueba.md` en un emulador con Google Play (API 31 o más): permiso concedido, permiso denegado, sin conexión, cambio de ubicación, estación fijada, modo oscuro, tap.
- [ ] Los mismos casos en un Samsung con One UI.
- [ ] Subir versionCode a 8 y correr `bubblewrap build`. Si regenera, reponer a mano los bloques `RODAJA_WIDGET_*`.
- [ ] Internal testing en Play Console. Data safety como `data_safety.md`.
- [ ] Al subir el AAB, pegar en la privacidad live el párrafo y el ítem de finalidad de `data_safety.md`.

## Qué no se reabre

Default 3939. `current_temp` es el `t_2m` de la hora que contiene «ahora». Sin lat/lon en `/hoy` ni en ninguna URL del widget. Sin rankings. minSdk 24 (Play rechazó el 21). Glance 1.1.1. `updatePeriodMillis` 0. Sin foreground service. Sin `ACCESS_FINE_LOCATION` ni `ACCESS_BACKGROUND_LOCATION`. Sin segundo `autoVerify` https. Theme de `<application>`, splash, `start_url`, `twa_boot` y `assetlinks.json` quedan como estaban. El pin sigue una sola vez, con guarda de API 26.
