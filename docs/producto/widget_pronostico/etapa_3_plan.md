# Plan de implementación — etapa 3 `widget_ui`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

Widget Glance 4×2 y 2×2 que muestra el pronóstico de hoy de la estación por defecto del asset, lo refresca con WorkManager y guarda el último JSON en DataStore.

## 2. Auditoría: qué se leyó

Solo la lectura mínima del master. No reauditar el repo.

| Ruta | Qué se confirmó |
|------|-----------------|
| `etapa_2_handoff.md` | Java 8 sin Kotlin: `Haversine`, `StationSelector`, `Hoy`, `HoyParser`. `HoyParser` mira `has` antes de `isNull`. El `doFirst` de `android.jar` no se quita. minSdk 21. Default 3939 está en el asset, no en estas clases. Sin cliente HTTP. `app/` está en `.gitignore`: `git add -f`. |
| `contrato_api.md` | `GET /api/widget/estaciones/<id>/hoy` en `https://app.rodaja.cl`, sin token. `default_station_id` 3939 en el asset. Tabla WMO día/noche. `is_day` null usa el ícono de día. No aplicar `pop ≥ 50`. |
| `AndroidManifest.xml` | `MAIN/LAUNCHER` y los `autoVerify` https están solo en `LauncherActivity`. Ya hay un `<service>` de la TWA (`DelegationService`), no es foreground. No hay receiver de widget. Permiso actual: solo `POST_NOTIFICATIONS`. |
| Master §4.13–4.15 | Worker 45 min, `NetworkType.CONNECTED`, backoff exponencial, pasada inmediata al agregar, `updatePeriodMillis = 0`, sin foreground. Último JSON en DataStore. Sin red: caché + hora + «sin conexión». Glance `SizeMode.Responsive`, 4×2 y 2×2, colores dinámicos con fallback &lt; 31, claro/oscuro, `previewLayout`, español. Íconos desde los SVG. Configuración del widget es la etapa 4. |
| `reporte_pronostico.js` `ecPronosticoWmoCondition` y `ecPronosticoNightComposite` | Día: composite por código. Noche solo cambia 0, 1, 2, 45, 48 y 80 (`luna`, `luna-parcial`, `luna-nubes`, `bruma`, `lluvia-y-luna`). El resto igual. |

## 3. Estado actual (evidencia)

- `mobile/twa/app/build.gradle`: AGP 8.9.1, `compileSdkVersion 36`, `minSdkVersion 21`, `targetSdkVersion 36`, Java 1.8. Bloques `RODAJA_WIDGET_*` con JUnit, `org.json` de test y `mavenCentral()`. Sin Kotlin, sin Glance, sin WorkManager, sin DataStore.
- `settings.gradle` es solo `include ':app'`. No hay `pluginManagement`. El plugin de Kotlin no puede llevar versión dentro del `plugins {}` del módulo: el classpath va en un `buildscript` del mismo `app/build.gradle`, dentro de marcadores.
- Glance **1.2.0** sube el minSdk de la librería a 23 (release notes, 1.2.0-beta01). Esta etapa usa **1.1.1**, que sigue en 21. No se sube el minSdk de la app para «hacer entrar» 1.2.
- Los SVG de día (`soleado`, `lluvia`, etc.) no tienen paths: son un PNG de 64×64 en un `<image>`. Los de noche que el mapa cambia (`luna`, `luna-parcial`, `luna-nubes`, `bruma`, `lluvia-y-luna`) sí son geometría. VectorDrawable no tiene `<mask>`: el mordisco de la luna pasa a un path.
- Rama `widget-pronostico`. El working tree de producción tiene otros cambios. El commit de esta etapa no los incluye.
- Esta etapa no toca la API. No hay restart.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| Lógica pura: mapa WMO, textos °C/mm/hora, estados, URL sin lat/lon, id por defecto leído del encabezado del asset, JSON de caché sin claves extra | `WmoIcon.java`, `WidgetText.java`, `WidgetFrame.java`, `WidgetLogicTest.java` | `gradlew test` | `current_temp` null → «—». Código desconocido → sin ícono. `is_day` null → día. JSON con `rankings`/`lat` no se reescribe a la caché. |
| PNG extraídos de los SVG raster; VectorDrawable de los cinco geométricos | `res/drawable-mdpi/w_*.png`, `res/drawable/w_*.xml` | `assembleDebug` | Máscara de luna: el path es la media luna, no un círculo entero. |
| Glance 4×2 / 2×2, tema dinámico ≥ 31 y fallback, estados en español | `PronosticoWidget.kt`, `PronosticoUi.kt` | `assembleDebug` | 2×2 es el ancho &lt; 180 dp. Sin JSON: solo el cartel. Con JSON y error u offline: números + cartel. |
| DataStore `pronostico_widget` (state, json, `station_id` opcional) | `WidgetStore.kt` | lo usa el worker; la prueba JVM no abre DataStore | Si no hay `station_id`, se usa el del asset. Esta etapa no escribe otro id. |
| Worker 45 min, red, backoff 30 s exponencial, GET solo del id | `PronosticoWorker.kt`, `PronosticoRefresh.kt`, `WidgetNet.kt` | revisión del request + test de la URL | 4xx no reintenta. 5xx e IO sí. IO marca `offline` y no borra el JSON. Sin red al agregar: estado `offline` ya, y el trabajo queda esperando `CONNECTED`. |
| Receiver, `updatePeriodMillis=0`, `previewLayout`, sin filtro https y sin `<service>` | `AndroidManifest.xml` (bloques marcados), `res/xml/pronostico_widget_info.xml`, `res/layout/pronostico_widget_preview.xml`, `res/values/widget_strings.xml` | grep del manifest y del xml | El tap de «Toca para activar ubicación» no apunta a una actividad que no existe (no-op). El tap del pronóstico lo pone la etapa 4. |
| Kotlin 2.0.21, plugin Compose, Glance 1.1.1, WorkManager, DataStore. Se conserva el `doFirst` de tests. minSdk 21 | `app/build.gradle` solo bloques `RODAJA_WIDGET_*` | `assembleDebug` y `test` | Si el plugin rompe el Java de Bubblewrap, el error va al handoff. No se sube minSdk. |

Textos de la tarjeta (fuente de verdad en `WidgetFrame`, el XML del preview repite el de carga):

- Cargando: «Cargando pronóstico»
- Sin conexión: «Sin conexión» + hora de `updated_at` (ya viene con offset de Chile; se muestran `HH:mm`)
- Sin ubicación: «Toca para activar ubicación»
- Error: «No se pudo cargar el pronóstico»
- Temperatura nula: «—». Si hay número: un decimal con coma y « °C». Rango «mín / máx». Precipitación «57 %» y/o «1,2 mm».

## 5. Tareas HUMANAS

ninguna

## 6. Dependencias

- Handoff anterior: etapa 2 `hecho`. `HoyParser.parse` es el único parseo del JSON de hoy.
- Restart api: no.
- assembleDebug: sí, desde `mobile/twa`, junto con `test`. JDK y SDK los que ya usó la etapa 2 (`local.properties`).

## 7. Orden de implementación

1. Clases Java de texto, ícono, estados y URL, con la prueba JVM.
2. Drawables (PNG extraídos y vectores de noche).
3. Bloques Gradle (Kotlin, Compose, dependencias) sin tocar minSdk ni el `doFirst`.
4. DataStore, worker, receiver, UI Glance.
5. Manifest y xml del widget.
6. `gradlew.bat test assembleDebug`.

## 8. Verificación

```
cd mobile/twa
.\gradlew.bat test assembleDebug --console=plain
```

Esperado: BUILD SUCCESSFUL, pruebas viejas más `WidgetLogicTest` en verde, `minSdkVersion 21`, el xml del provider con `updatePeriodMillis="0"` y `previewLayout`, el bloque nuevo del manifest sin `<service>` y sin `http`.

## 9. Fuera de alcance

Ubicación, pin, `OnboardingActivity`, `WidgetConfigureActivity`, `widgetFeatures`, deep link del tap, `LauncherActivity`, `index.html`, permiso de ubicación, catálogo haversine en el teléfono, rankings, viento, AAB, bubblewrap, assetlinks, splash, `start_url`, API.

## 10. Riesgos

- El plugin de Kotlin o Compose rompe `assembleDebug`. No se sube minSdk ni se pasa a Glance 1.2 (minSdk 23).
- `gradle.properties` tiene `-Xmx1536m`. Si el compile de Compose se queda sin heap, se sube solo esa línea y se dice en el handoff.
- Catorce íconos de día son PNG dentro del SVG. Un VectorDrawable inventado no sería el dibujo de la web. Se extrae el PNG. Los cinco geométricos sí son vector.
- `bubblewrap update` borra los bloques `RODAJA_WIDGET_*` y no conoce los archivos nuevos. Sigue prohibido.
- El trabajo periódico con `CONNECTED` no corre mientras no hay red, así que no puede él solo cambiar el cartel. Al agregar o actualizar el widget, si no hay red, el receiver deja `offline` antes de encolar. No hay listener permanente de conectividad.
- El tap del pronóstico no abre la TWA todavía. La etapa 4 lo hace con un Intent explícito a `LauncherActivity`. Un PendingIntent a una actividad que no existe se caería al tocarlo.

## 11. Self-check

- [x] Filas `etapa_owner = 3` de `criterios.md` cubiertas: C09 (Glance, estados, español, °C/mm, previewLayout) y C10 (45 min, red, `updatePeriodMillis` 0, sin foreground, caché)
- [x] Prohibidos globales del prompt fijo respetados
- [x] Humanos no fingidos (Play, keystore, teléfono, AAB)
- [x] No se sube minSdk, no se toca assetlinks ni el splash
- [x] Lat/lon no se envían al servidor. La URL es solo el id. La caché reescrita no guarda `lat`/`lon`/`rankings`
