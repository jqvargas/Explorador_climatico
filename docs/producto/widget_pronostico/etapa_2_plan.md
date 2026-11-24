# Plan de implementación — etapa 2 `android_logica`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

Lógica Java testeable en JVM — haversine, elección de estación y parseo del JSON de hoy — con `assembleDebug` en minSdk 21 y sin UI.

## 2. Auditoría: qué se leyó

Solo la lectura mínima de la etapa. No reauditar el repo.

| Ruta | Qué se confirmó |
|------|-----------------|
| Handoff etapa 1 | Default `3939` vive en el asset, no en el GET del catálogo. `/hoy` no trae lat/lon ni rankings. `current_temp` es el `t_2m` de la hora, y puede ser null. `mobile/twa/.gitignore` ignora `app/`: los `.java` nuevos entran con `git add -f`. No reabrir C01–C05. |
| `contrato_api.md` | Hoy: `station_id` entero, números o null, `condition_code` entero, `is_day` 0/1/null, `precip_prob` entero. El catálogo trae `id/name/comuna/lat/lon` para el haversine local. |
| `mobile/twa/app/build.gradle` | `com.android.application`, minSdk 21, Java 1.8, una dependencia (`androidbrowserhelper:2.6.2`). No hay Kotlin, JUnit, `testOptions` ni comentarios `RODAJA_WIDGET_*`. |
| `mobile/twa/build.gradle` | AGP 8.9.1. Repos del root: `google()` y `jcenter()`. No hace falta tocarlo: el módulo `app` puede declarar `mavenCentral()` solo para JUnit y `org.json`. |
| Master §4.3–4.5 | Sin permiso o sin fix, o la más cercana a más de 200 km → `default_station_id`. Recalcular en modo `nearest` solo si no hay fix previo o el desplazamiento es ≥ 25 km. Modo `fixed` no se toca. Las coordenadas no salen del teléfono: esta etapa no abre ninguna URL. |

Estado de build, fuera de esa lista y necesario para el comando del DoD: ya estamos en la rama `widget-pronostico`. Hay `gradlew.bat`, wrapper Gradle 8.11.1, `sdk.dir` en `local.properties`, JDK 21 en el PATH. El target de compile sigue en 1.8.

## 3. Estado actual (evidencia)

No hay clases bajo `cl.rodaja.app.widget` ni `src/test`. `org.json` no está en el `build.gradle`; en el teléfono lo trae el runtime de Android. En un test JVM, `android.jar` solo tiene el stub (lanza al usarlo). El parser no importa `android.*`, así que el test puede correr con el `org.json` de Maven y el stub al final del classpath.

No se agrega el plugin Kotlin. El riesgo nombrado en el master es que ese plugin rompa el Java de Bubblewrap, y aquí no hace falta.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| Haversine esférico, R = 6371 km | `app/src/main/java/cl/rodaja/app/widget/Haversine.java` | 1° de latitud = `6371 * π/180` km; el mismo punto da 0 | Java 8. Sin `android.*`. |
| Elección de estación | `.../widget/StationSelector.java` | los casos de §4.3–4.5 | Ver motivos abajo. Empate de distancia: menor `id` (desempate de código, no elige otra ciudad). |
| Parseo del JSON de hoy | `.../widget/Hoy.java`, `HoyParser.java` | nulls, `condition_code` entero, clave `rankings` ignorada | Sin `station_id` o JSON roto → `IllegalArgumentException`. No declara `JSONException`. |
| JUnit 4 + `org.json` solo en test; `mavenCentral()`; el stub de `android.jar` queda al final del classpath de test | `app/build.gradle`, entre `RODAJA_WIDGET_BEGIN` / `END` | `gradlew.bat test` | minSdk 21 no se toca. Sin plugin Kotlin. Sin dependencia de Glance. |
| Pruebas | `app/src/test/java/cl/rodaja/app/widget/*Test.java` | el task verde | Umbrales lejos del borde (10 vs 30 km, 199 vs 201 km) para que el redondeo no los cruce. |

Motivos que devuelve `StationSelector` (no son texto de UI; la etapa 3 no los muestra):

| Caso | `reason` | id |
|------|----------|----|
| modo `fixed` | `fixed` | el id guardado; si es null, el default |
| `nearest` sin fix nuevo | `sin_fix` | default |
| hay fix previo, desplazamiento &lt; 25 km y hay id guardado | `sin_movimiento` | ese id, sin mirar el catálogo |
| no hay fix previo, o el desplazamiento es ≥ 25 km, o no hay id guardado | se recalcula | |
| catálogo vacío o sin coordenadas finitas | `vacio` | default |
| la más cercana queda a más de 200 km | `lejos` | default |
| la más cercana queda a 200 km o menos | `cercana` | ese id |

Modo distinto de `fixed` / `nearest` lanza `IllegalArgumentException`. Un punto no finito del catálogo se salta. `fixed` no lee el fix nuevo ni el catálogo.

`HoyParser` usa `org.json.JSONObject`. No lee `rankings`, `lat` ni `lon`. `condition_code`, `precip_prob` e `is_day` salen `Integer`. El resto de los números salen `Double`. Clave ausente o JSON null → null.

## 5. Tareas HUMANAS

ninguna

## 6. Dependencias

- Handoff anterior: etapa 1 `hecho`. Default 3939. Esta etapa no llama al API.
- Restart api: no.
- assembleDebug: sí, al final.

## 7. Orden de implementación

1. Este plan.
2. Las cuatro clases Java.
3. El bloque marcado en `app/build.gradle`.
4. Los tests.
5. `gradlew.bat test` y `gradlew.bat assembleDebug` desde `mobile/twa`.
6. Criterios C06–C08, handoff, cola.
7. Un commit en `widget-pronostico`, con `git add -f` solo de los `.java` nuevos y de `app/build.gradle`. Sin push.

## 8. Verificación

- `gradlew.bat test` → exit 0. Cubre haversine, más cercana, 25 km, 200 km, `fixed`, catálogo vacío, nulls y `rankings`.
- `gradlew.bat assembleDebug` → exit 0.
- `minSdkVersion 21` sigue en `app/build.gradle`.
- C01–C05 no se editan.

## 9. Fuera de alcance

Glance, worker, DataStore, manifest, permiso, pin, `LauncherActivity`, drawables, el HTML de mi-reporte, la API, el asset, privacidad, AAB, minSdk, viento.

## 10. Riesgos

- El stub de `org.json` en `android.jar` hace fallar el test JVM si gana en el classpath. El bloque marcado lo deja al final. En el teléfono se usa el `org.json` del sistema; el jar de Maven es `testImplementation`.
- Si `assembleDebug` falla por el bloque, el handoff lleva el error completo. No se sube minSdk ni se agrega Kotlin para “arreglarlo”.
- `bubblewrap update` borra los comentarios `RODAJA_WIDGET_*`. Sigue prohibido.
- `jcenter()` del root puede no resolver JUnit. Por eso `mavenCentral()` va en el módulo `app`, dentro del bloque marcado.
- El desempate por menor `id` no está en el master. Solo ordena dos distancias iguales. No sustituye a la 3939.

## 11. Self-check

- [x] Filas `etapa_owner = 2` de `criterios.md` cubiertas por el plan (C06–C08)
- [x] Prohibidos globales del prompt fijo respetados
- [x] Humanos no fingidos (Play, keystore, teléfono, AAB)
- [x] No se sube minSdk, no se toca assetlinks ni el splash
- [x] Lat/lon no se envían al servidor (no hay cliente HTTP en esta etapa)
- [x] C01–C05 no se reabren
