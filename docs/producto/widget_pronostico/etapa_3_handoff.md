# Handoff etapa 3 — `widget_ui`

**Fecha:** 2026-11-15
**Status en cola:** `hecho`
**Plan:** `docs/producto/widget_pronostico/etapa_3_plan.md`

## Qué se hizo

- Widget Glance 4×2 (250×110 dp) y 2×2 (110×110 dp), `SizeMode.Responsive`. Textos en español: nombre, comuna, temperatura o «—», mín/máx, probabilidad y/o mm, hora `HH:mm` del `updated_at` (el offset ya es Chile).
- Estados: cargando, contenido, sin conexión (caché + hora + «Sin conexión»), error genérico, sin ubicación («Toca para activar ubicación»). El de sin ubicación no tiene click: no hay actividad de permiso todavía.
- La estación es la del asset (`default_station_id` leído de los primeros 256 bytes). Si DataStore tiene `station_id` > 0, el worker usa ese. Esta etapa no lo escribe.
- Worker periódico 45 min, `NetworkType.CONNECTED`, backoff exponencial de 30 s, y una pasada inmediata al agregar o actualizar el widget, con la misma restricción de red. `updatePeriodMillis` es 0. El último JSON bueno se reescribe sin `rankings`, `lat` ni `lon`.
- Glance 1.1.1 (la 1.2.0 sube el minSdk de la librería a 23). Tema `GlanceTheme` por defecto: color dinámico en API 31 y baseline en el resto, de día y de noche. Kotlin 2.0.21 y el plugin de Compose, solo dentro de `RODAJA_WIDGET_*`. minSdk sigue 21.
- Íconos de día: PNG de 64×64 extraídos del `<image>` de cada SVG (no tenían paths). Los cinco geométricos (`luna`, `luna-parcial`, `luna-nubes`, `bruma`, `lluvia-y-luna`) son VectorDrawable. El mordisco de la luna es un path: VectorDrawable no tiene máscara.
- WorkManager 2.9.1 trae `SystemForegroundService` prendido. El bloque del manifest lo saca con `tools:node="remove"`, y también el permiso `FOREGROUND_SERVICE`. El worker no llama `setForeground` ni `setExpedited`.

## Qué se verificó

- [x] Desde `mobile/twa`: `.\gradlew.bat test assembleDebug --console=plain` → **BUILD SUCCESSFUL** in 34s. JDK 21, AGP 8.9.1, Gradle 8.11.1.
- [x] `testDebugUnitTest` y `testReleaseUnitTest`: HaversineTest 3, StationSelectorTest 12, HoyParserTest 5, WidgetLogicTest 8. 0 failures, 0 errors.
- [x] Tras quitar el servicio de primer plano del merge: `.\gradlew.bat :app:assembleDebug --console=plain` → **BUILD SUCCESSFUL** in 12s.
- [x] `minSdkVersion 21` en `app/build.gradle`. El manifest fusionado de debug dice `android:minSdkVersion="21"`.
- [x] `res/xml/pronostico_widget_info.xml`: `updatePeriodMillis="0"` y `android:previewLayout="@layout/pronostico_widget_preview"`.
- [x] Manifest fusionado: el receiver solo tiene `APPWIDGET_UPDATE`. Los tres `autoVerify` https siguen en `LauncherActivity` (app.rodaja.cl, accounts, clerk). No hay `ACCESS_FINE_LOCATION`, `ACCESS_COARSE_LOCATION` ni `ACCESS_BACKGROUND_LOCATION`. No queda `SystemForegroundService` ni `FOREGROUND_SERVICE`.
- [x] C01–C08 no se reabrieron. C09 y C10 quedaron en `ok`.
- [x] No se tocó la API. No hubo restart.

## Gaps / caveats / qué puede fallar después (obligatorio)

El DoD de esta etapa está cumplido. Lo que sigue es para no romperlo.

| Riesgo | Señal | Impacto | Mitigación o etapa dueña |
|--------|-------|---------|--------------------------|
| `bubblewrap update` | Borra los bloques `RODAJA_WIDGET_*` de gradle y del manifest | Se pierden Kotlin, Glance, el receiver y el `tools:node="remove"`. Los archivos nuevos (Kotlin, drawables, xml) no los regenera, pero el receiver deja de estar declarado | Reponer los bloques a mano. No correr bubblewrap en esta cola |
| Glance en API 21 y 22 | Log de `GlanceAppWidgetReceiver`: «Using Glance in devices with API<23 is untested» | En esos dos niveles el widget puede verse mal. No se subió minSdk | Dejarlo. Glance 1.2 no entra: su minSdk es 23 |
| Íconos de día | Un SVG de día es un PNG embebido | No son vectores. Escalarlos por encima de ~40 dp se ve blando | No redibujarlos. Los de noche sí son vector |
| Sin emulador | Nadie colocó el widget | El layout 4×2 / 2×2 y el claro/oscuro no se vieron | Etapa 5, guía de prueba. No marcar un teléfono como hecho |
| Tap mudo | Tocar la tarjeta o «Toca para activar ubicación» no abre nada | El deep link y el permiso son de la etapa 4. Un Intent a una clase que no existe se caería | No poner el PendingIntent hasta que la actividad exista |
| `no_location` | El worker escribe `content`, `offline` o `error` en cada pasada | Si la etapa 4 guarda `no_location` en la misma clave, el próximo fetch lo pisa | Enseñar al worker a respetar ese estado, o usar otra señal, antes de mostrarlo |
| Trabajo periódico `KEEP` | Cambiar el intervalo no reemplaza el trabajo ya encolado | El teléfono sigue con el request viejo hasta borrar datos o cancelar `rodaja_pronostico_periodic` | `ExistingPeriodicWorkPolicy.UPDATE` o `cancel` si la etapa 4 cambia el request |
| Sin red y el widget ya puesto | El periódico no corre mientras no hay red | El cartel «Sin conexión» se pone al agregar/actualizar el widget o cuando un fetch que ya empezó tira `IOException`. No hay un listener permanente | No agregar un servicio para escucharlo |
| `runBlocking` en el receiver | DataStore lento en el primer arranque | ANR si la escritura pasa de unos segundos. Hoy es una preferencia | No llamar `goAsync`: el receiver de Glance ya lo usa en `onUpdate` |
| `doFirst` del test JVM | Alguien lo saca al tocar el bloque de Gradle | `HoyParserTest` vuelve a `Method ... not mocked` | No quitar el reorder de `android.jar` |

## TAREAS HUMANAS

- [x] ninguna

## Pendiente / no es de esta etapa

- Ubicación, pin, `OnboardingActivity`, `WidgetConfigureActivity`, `widgetFeatures`, el deep link del tap, el early-return de `LauncherActivity`, el bypass de `index.html`, privacidad, AAB.
- El catálogo del asset no se parsea. Solo se lee `default_station_id`.
- El haversine y el modo `nearest` / `fixed` no los llama el widget todavía.

## Para la etapa siguiente

- No reabrir: minSdk 21; Glance 1.1.1 (no 1.2); sin `FOREGROUND_SERVICE`; `updatePeriodMillis` 0; sin lat/lon en la URL; `HoyParser` sigue mirando `has` antes de `isNull`; el `doFirst` de `android.jar` se queda; no aplicar `pop ≥ 50`.
- DataStore `pronostico_widget`: claves `state` (`loading` `content` `offline` `error` `no_location`), `json` (el canónico), `station_id` (long, opcional). Vacío → id del asset.
- Nombres de trabajo: `rodaja_pronostico_periodic`, `rodaja_pronostico_now`.
- El tap del pronóstico, cuando exista: Intent explícito a `cl.rodaja.app.LauncherActivity` con data `https://app.rodaja.cl/mi-reporte/?view=pronostico&e={id}`, el mismo patrón que `shortcuts.xml`. Sin `autoVerify` nuevo.
- El tap de «Toca para activar ubicación»: la actividad de la etapa 4. Hoy el cartel se pinta y no es clickable.
- Archivos nuevos bajo `mobile/twa/app/` (Java/Kotlin del widget, drawables `w_*`, `pronostico_widget_info.xml`, `pronostico_widget_preview.xml`, `widget_strings.xml`) y los bloques `RODAJA_WIDGET_*` de `app/build.gradle` y `AndroidManifest.xml`. `git add -f`: `app/` está en `mobile/twa/.gitignore`.
- Comando: `cd mobile/twa` y `.\gradlew.bat test assembleDebug --console=plain`.

## Decisiones de producto tomadas aquí

Ninguna. El tema es el de Glance (dinámico en API 31, baseline antes, claro y oscuro). Los PNG de día son el dibujo que ya venía dentro del SVG, no un ícono nuevo. La estación de prueba es la del asset, no otra ciudad.
