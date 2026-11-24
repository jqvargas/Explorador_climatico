# Handoff etapa 2 — `android_logica`

**Fecha:** 2026-11-15
**Status en cola:** `hecho`
**Plan:** `docs/producto/widget_pronostico/etapa_2_plan.md`

## Qué se hizo

- Haversine en km (`R = 6371`), elección de estación y parseo del JSON de hoy, en Java 8, sin `android.*` y sin Kotlin.
- `StationSelector.choose` aplica el §4.3–4.5: `fixed` no mira el fix; `nearest` solo recalcula si no hay fix previo, no hay id guardado o el desplazamiento es ≥ 25 km; a más de 200 km, catálogo vacío o sin fix → `defaultStationId` (lo pasa el llamador; esta etapa no hardcodea 3939).
- `HoyParser` ignora `rankings`, `lat` y `lon`. `condition_code` sale `Integer`. Sin `station_id`, o JSON roto, lanza `IllegalArgumentException`.
- En `app/build.gradle`, entre `RODAJA_WIDGET_BEGIN` / `END`: JUnit 4.13.2, `org.json:json:20240303` solo en test, `mavenCentral()`, y el stub de `android.jar` al final del classpath de test. minSdk sigue 21.

## Qué se verificó

- [x] Desde `mobile/twa`: `.\gradlew.bat test assembleDebug --console=plain` → **BUILD SUCCESSFUL** in 3m 17s (56 tasks). JDK 21, AGP 8.9.1, Gradle 8.11.1.
- [x] `testDebugUnitTest` y `testReleaseUnitTest`: HaversineTest 3, StationSelectorTest 12, HoyParserTest 5. 0 failures, 0 errors. Reportes en `app/build/test-results/`.
- [x] `assembleDebug` en el mismo run, task `:app:assembleDebug` ejecutada.
- [x] `minSdkVersion 21` sigue en `mobile/twa/app/build.gradle` línea 58.
- [x] C01–C05 no se tocaron. C06–C08 quedaron en `ok`.
- [x] No hay cliente HTTP. No se reinició la API.

## Gaps / caveats / qué puede fallar después (obligatorio)

El DoD de esta etapa está cumplido. Lo que sigue es para no romperlo.

| Riesgo | Señal | Impacto | Mitigación o etapa dueña |
|--------|-------|---------|--------------------------|
| `bubblewrap update` | Borra los tres bloques `RODAJA_WIDGET_*` de `app/build.gradle` | Se pierden JUnit, `org.json` de test y `mavenCentral()`. El Java nuevo no es de esos bloques, pero un update sigue prohibido | Reponer el bloque a mano si alguien regenera. No correr bubblewrap en esta cola |
| Stub de `org.json` | `HoyParserTest` tira `Method ... not mocked` | El reorder deja `android.jar` al final solo en el test JVM. En el teléfono se usa el `org.json` del sistema | No quitar el `doFirst` al agregar Glance. Si un test necesita una API real de Android, Robolectric; no subir minSdk |
| JDK 21 y source 8 | javac avisa que source/target 8 es obsolete | El build igual sale 0. Un JDK futuro puede negarse | No se subió el lenguaje ni minSdk. El warning ya estaba en el compile de Bubblewrap |
| `isNull` distinto en Android y en el jar de Maven | Una clave ausente | En Android `isNull` es true si falta la clave; en Maven, no. El parser mira `has` primero | No está corrido en un teléfono. La etapa 3 no debe “simplificarlo” a solo `isNull` |
| Empate de distancia | Dos estaciones al mismo km | Gana el menor `id`. No está en el master; no sustituye a la 3939 | Dejarlo. No es una ciudad de respaldo |

## TAREAS HUMANAS

- [x] ninguna

## Pendiente / no es de esta etapa

- Glance, worker, DataStore, drawables, manifest, permiso, pin, `LauncherActivity`, el bypass de `index.html`, privacidad, AAB.
- No hay parser del catálogo. El asset y el GET siguen siendo JSON crudo.
- Los `reason` (`cercana`, `lejos`, `vacio`, `sin_fix`, `sin_movimiento`, `fixed`) no son texto de la tarjeta.

## Para la etapa siguiente

- No reabrir: minSdk 21; sin Kotlin en esta etapa; sin coordenadas en ninguna URL; default lo pasa el llamador (3939 está en el asset, no en estas clases); `current_temp` null se queda null; no aplicar el refuerzo `pop ≥ 50`.
- Operadores: se recalcula si el desplazamiento es **≥ 25** (`moved < 25` conserva). La más cercana pasa a default si el km es **> 200** (199 se queda, 201 es `lejos`). Probado en 10/30 y 199/201, no en el flotante exacto del borde.
- API para la etapa 3:
  - `Haversine.km(lat1, lon1, lat2, lon2)`
  - `StationSelector.choose(mode, heldStationId, prevLat, prevLon, newLat, newLon, catalog, defaultStationId)` → `Result.stationId` / `Result.reason`
  - `StationSelector.Point(id, lat, lon)` — la etapa 3 arma la lista desde el asset o el GET. Esta etapa no parsea ese array.
  - `HoyParser.parse(String)` → `Hoy`. Campos públicos. `conditionCode` es `Integer`.
- Modo distinto de `fixed` / `nearest` lanza. `fixed` con id null devuelve el default y reason `fixed`.
- Archivos tocados: `Haversine.java`, `StationSelector.java`, `Hoy.java`, `HoyParser.java`, los tres `*Test.java`, `mobile/twa/app/build.gradle` (solo bloques marcados), plan, handoff, criterios, cola.
- Git: `mobile/twa/.gitignore` ignora `app/`. Hay que `git add -f` de esos paths. `app/build.gradle` queda tracked entero (es el gradle de Bubblewrap más el bloque); el resto de `app/` que no sea de esta etapa no se agrega. `LauncherActivity`, el manifest y el asset de la etapa 1 no se modificaron.
- Comando: `cd mobile/twa` y `.\gradlew.bat test assembleDebug --console=plain`.

## Decisiones de producto tomadas aquí

Ninguna. Los `reason` y el desempate por menor `id` son etiquetas de código para las pruebas y para la etapa 3. No eligen otra ciudad si falta Quinta Normal: el id por defecto lo sigue pasando quien llama.
