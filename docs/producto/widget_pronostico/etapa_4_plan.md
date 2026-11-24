# Plan de implementación — etapa 4 `ubicacion_pin`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

Primera apertura pide ubicación aproximada y el pin una sola vez, el widget abre la estación en la TWA, y la configuración elige la más cercana o una estación del catálogo del teléfono.

## 2. Auditoría: qué se leyó

Solo la lectura mínima del master, más los ganchos que el handoff 3 nombra (DataStore, selector, receiver). No reauditar el repo.

| Ruta | Qué se confirmó |
|------|-----------------|
| `etapa_3_handoff.md` | DataStore `pronostico_widget`: `state`, `json`, `station_id`. El worker pisa `state` en cada pasada. El tap no existe todavía. `no_location` no se puede guardar en `state`. Catálogo del asset sin parsear. Nombres de trabajo `rodaja_pronostico_periodic` y `rodaja_pronostico_now`. `git add -f` porque `app/` está ignorado. |
| `contrato_api.md` | Asset con `default_station_id` 3939 y `stations[]` de `id`, `name`, `comuna`, `lat`, `lon`. El id es lo único que vuelve al servidor. 1781 del master no es la estación por defecto. |
| `LauncherActivity.java` | `onCreate` llama `super` al toque. `notifs_asked` y `twa_boot` viven en `rodaja_twa` y corren después de `super`. No hay early-return. |
| `AndroidManifest.xml` | `MAIN/LAUNCHER` y los tres `autoVerify` https están solo en `LauncherActivity`. El bloque `RODAJA_WIDGET_*` tiene el receiver y quita `SystemForegroundService`. Permisos: `POST_NOTIFICATIONS`, `ACCESS_NETWORK_STATE`. Tema de `<application>` translúcido. |
| `index.html` líneas 29-49 | `ra-need-station` si `_twa` o `cambiar=1` o (sin favorita y sin `e`). `view=pronostico` no se mira. `twa_boot` solo enciende `_twa`. |
| `shortcuts.xml` | Intent explícito: `ACTION_MAIN`, `targetPackage` `cl.rodaja.app`, `targetClass` `cl.rodaja.app.LauncherActivity`, `data` con la URL. No es `ACTION_VIEW`. |
| Master §4.6–4.12 y §4.15 | Coarse, primer plano, Play Services, `getCurrentLocation` balanceada y si falla `lastLocation`. Sin Play Services: estación por defecto y el widget lo dice. Pin solo SDK ≥ 26 y launcher que lo soporte, una vez, después del permiso, si no hay instancia. Rechazo no insiste. Esquema `rodaja://pin` y `S.e` en modo `fixed`. Deep link explícito a `LauncherActivity`. Bypass `view=pronostico` + `e`. Early-return antes de `super`, tema opaco solo en la actividad nueva. `widgetFeatures="reconfigurable\|configuration_optional"`. |

## 3. Estado actual (evidencia)

- `StationSelector.choose` ya cubre `fixed`, 25 km, 200 km y catálogo vacío. Nada del widget lo llama.
- `WidgetStore.writeContent` solo escribe `state` y `json`. No hay claves de modo, fix ni aviso. El comentario dice que no guarda lat/lon: el JSON de caché sigue sin ellas. El fix va en claves aparte, nunca en una URL.
- `PronosticoCard` no tiene click. `pronostico_widget_info.xml` no tiene `configure` ni `widgetFeatures`. `updatePeriodMillis` es 0.
- `Application.onCreate` está vacío. No se toca.
- El catálogo de assets pesa 287 KB. Se parsea en un hilo de fondo.
- `google()` ya está en `mobile/twa/build.gradle` (`allprojects`). Play Services entra por el bloque `RODAJA_WIDGET_*` de `app/build.gradle`.
- Esta etapa no toca la API. No hay restart. El HTML de Mi reporte sí está en el nginx de producción.
- minSdk sigue 21. Glance sigue en 1.1.1.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| URL del tap, sin lat/lon. `stationId` en el frame cuando hay JSON. Aviso de sin Play Services | `WidgetFrame.java`, `WidgetLogicTest.java` | `gradlew test` | id &lt; 1 tira. `NO_LOCATION` sigue sin números: el aviso nuevo no usa esa clave |
| Parseo del asset y buscador (nombre o comuna, sin acentos) | `CatalogParser.java` | test JVM | Sin lat/lon se salta la fila. Clave `rankings` no se lee. Query vacía devuelve todo |
| Decisión: permiso, Play Services, `fixed`, 25 km. El aviso no sale en modo `fixed` | `WidgetPlacement.java` | test JVM | Coordenadas pasadas sin permiso se tiran. Sin Play Services gana sobre «toca para activar» |
| Early-return y pin: SDK, launcher, cero instancias. Extra `e` solo dígitos | `WidgetGates.java` | test JVM | SDK 25 no ofrece pin. `12.5` y texto no son un id |
| Claves `mode`, `fix_lat`, `fix_lon`, `notice`. `writeContent` no las toca | `WidgetStore.kt` | lo usa el flujo; el test de caché de la etapa 3 sigue igual | Si el JSON guardado es de otra estación, se borra y queda «Cargando» |
| Click del pronóstico y del aviso | `PronosticoWidget.kt`, `PronosticoUi.kt` | `assembleDebug` | Sin id no hay Intent. El aviso de ubicación no usa `ACTION_VIEW` |
| Onboarding opaco, permiso coarse, pin, relanza la TWA | `OnboardingActivity.kt`, layout, tema | `assembleDebug` y revisión del manifest | El flag se escribe con `commit` antes de relanzar. Si `startActivity` del onboarding tira, se sigue a `super` |
| `rodaja://pin` y `S.e` | `WidgetOfferActivity.kt` | revisión del filtro | Con `S.e` no se lee ubicación. Sin instancias no queda un trabajo periódico huérfano |
| Lista y «Usar la más cercana» | `WidgetConfigureActivity.kt`, `pronostico_widget_info.xml` | `assembleDebug` | El botón fuerza un cálculo (sin fix previo). Un fix automático posterior sí respeta los 25 km |
| Play Services en primer plano | `LocationReader.kt`, `PlaceUpdater.kt`, `app/build.gradle` | manifest fusionado con minSdk 21 | Sin Play Services no hay `LocationManager`. Si `getCurrentLocation` falla, `lastLocation`. Si las dos fallan, estación por defecto y sin aviso de permiso |
| Early-return, permiso, actividades, esquema | `LauncherActivity.java`, `AndroidManifest.xml` | grep del manifest fuente y del fusionado | Un solo `autoVerify` https, el de `LauncherActivity`. Sin `ACCESS_FINE_LOCATION` ni background |
| Bypass del selector | `nginx/html/mi-reporte/index.html` | curl del HTML y la clase en el navegador | `cambiar=1` sigue mostrando el selector. No se toca `twa_boot` |
| Textos | `widget_strings.xml`, `widget_onboarding.xml`, `widget_configure.xml`, `widget_offer.xml`, `widget_themes.xml` | `assembleDebug` | Tema opaco solo en esas actividades |

El flag de «ya visto» queda en `SharedPreferences` `rodaja_twa` / `widget_offer_seen`. DataStore es async y el chequeo es antes de `super.onCreate`. Las coordenadas no van ahí: van en DataStore y no salen.

Aviso del widget, clave `notice`, que el worker no escribe:

- Sin Play Services y modo `nearest`: «Sin servicios de Google. Estación por defecto.»
- Con Play Services, sin permiso, modo `nearest`: «Toca para activar ubicación»
- Modo `fixed`, o permiso concedido: sin aviso

«Usar la más cercana» recalcula aunque el último movimiento sea &lt; 25 km. Es una orden, no un fix nuevo del mismo modo. Los 25 km siguen valiendo en el tap de ubicación y en cualquier lectura con fix previo.

## 5. Tareas HUMANAS

ninguna

## 6. Dependencias

- Handoff anterior: etapa 3 `hecho`.
- Restart api: no.
- assembleDebug: sí, desde `mobile/twa`, junto con `test`. JDK y SDK los de `local.properties`.
- Play Services: `com.google.android.gms:play-services-location:21.0.1`. Si el manifest fusionado sube el minSdk, se baja la versión o se quita. No se sube el minSdk de la app.

## 7. Orden de implementación

1. Clases puras y pruebas JVM.
2. `WidgetFrame.reportUrl` y el campo `stationId`.
3. DataStore, lectura de ubicación, actividades, layouts.
4. Bloques `RODAJA_WIDGET_*` del manifest, de `LauncherActivity` y de Gradle.
5. Click del widget y `widgetFeatures`.
6. Condición de `index.html`.
7. `gradlew.bat test assembleDebug` y revisión del manifest fusionado.

## 8. Verificación

```
cd mobile/twa
.\gradlew.bat test assembleDebug --console=plain
```

Esperado: BUILD SUCCESSFUL, pruebas viejas más las de catálogo, placement, gates y la URL del reporte. `minSdkVersion` 21 en el fuente y en el manifest fusionado. Un solo grupo `autoVerify` https, dentro de `LauncherActivity`. Permiso de ubicación: solo `ACCESS_COARSE_LOCATION`. Filtro `rodaja` / `pin` sin `autoVerify`. `updatePeriodMillis` sigue 0. No aparece `SystemForegroundService`.

HTML: `https://app.rodaja.cl/mi-reporte/?view=pronostico&e=3939` no agrega `ra-need-station`. La misma URL con `cambiar=1` sí.

## 9. Fuera de alcance

- Guía de emulador, Data safety, página legal, AAB, versionCode, `bubblewrap`.
- Botón en `/app/`.
- Bajar el catálogo por red. La lista es el asset de la etapa 1.
- Leer ubicación dentro del worker o al abrir la TWA. El bloque de `LauncherActivity` es solo el early-return.
- Viento, rankings, alertas, `/estaciones/cercanas`.

## 10. Riesgos

- El early-return se equivoca y la TWA no abre. `androidbrowserhelper` llama a `launchTwa()` dentro de `super.onCreate` si `shouldLaunchImmediately()` es true. Retornar sin `super` tira `SuperNotCalledException`. En la pasada desviada ese método devuelve false, después se abre el onboarding y se hace `finish()`. El extra y el `commit` van antes de relanzar. El relanzamiento lleva la URL original o `launchUrl`, porque si la actividad anterior sigue viva y el intent no tiene data, el padre se cierra sin abrir Chrome. Si arrancar el onboarding tira, se marca el flag y se llama a `launchTwa()`.
- `requestPinAppWidget` no avisa el rechazo. La actividad sigue en pantalla con «Abrir rodaja». No se vuelve a pedir en el arranque.
- Play Services puede subir el minSdk por el manifest de la librería. Se mide el fusionado. No se «arregla» subiendo el de la app.
- En API 21–27 el launcher ignora `configuration_optional` y abre la configuración al poner el widget. No se sube minSdk. Salir de esa pantalla devuelve `RESULT_OK` para no borrar el widget.
- El catálogo del teléfono envejece con el asset. Una estación nueva no sale en el buscador hasta otro build. El id que mande `S.e` igual se guarda.

## 11. Self-check

- [x] Filas `etapa_owner = 4` de `criterios.md` cubiertas: C11 coarse y coordenadas quietas, C12 pin con guarda y arranque que igual abre la TWA, C13 deep link y bypass, C14 configuración y modo `fixed`
- [x] Prohibidos globales del prompt fijo respetados
- [x] Humanos no fingidos (Play, keystore, teléfono, AAB). El teléfono queda como gap de la etapa 5, no como un ok
- [x] No se sube minSdk, no se toca assetlinks ni el splash
- [x] Lat/lon no se envían al servidor
