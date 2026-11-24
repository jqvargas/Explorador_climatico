# Plan maestro — widget de pronóstico (TWA)

Baseline: **2026-11-14**. Cola: `widget_pronostico_cola.json`. Prompt: `widget_pronostico_PROMPT_FIJO.md`.

El prompt genérico de “tarjeta de clima” **sí aplica**, con las correcciones de este archivo. Esas correcciones están cerradas. Una etapa que las reabra se bloquea.

## 1. Objetivo y listo

Widget nativo en `cl.rodaja.app` (TWA Bubblewrap, no Capacitor) que muestra el pronóstico de **hoy** de una estación de la red. Por defecto, la estación con pronóstico más cercana al teléfono. El usuario puede fijar otra. Tocar la tarjeta abre esa estación en Mi reporte, dentro de la TWA.

Listo de verdad:

- `assembleDebug` compila con minSdk 24.
- Pruebas JVM de haversine/selección y del parseo JSON.
- `GET /widget/estaciones` y `GET /widget/estaciones/<id>/hoy` responden en `app.rodaja.cl` sin login, con Cache-Control, sin rankings.
- Guía de emulador y texto de Data safety escritos. El AAB **no** se publica en esta cola.

## 2. Dominio

**Es:** pronóstico Open-Meteo ya guardado por estación (`carta_del_mirador` diario y `carta_por_hora`). Visible si `estacion_calidad` es `excelente` o `buena` (`pronostico_estacion_visible` en `api/utils/pronostico_payload.R`). El endpoint actual `GET /estaciones/<id>/pronostico` es público y pesado (varios días + rankings). Las alertas tempranas sí dependen de suscripción; los números del pronóstico, no.

**No es:**

- Temperatura observada de la estación. “Ahora” = `t_2m` de la hora de pronóstico que contiene el instante actual en `America/Santiago`. Si no hay fila horaria, `current_temp` va `null` y se muestran mín/máx del día.
- La estación favorita de la cuenta. El widget no tiene sesión Clerk. La favorita se puede fijar a mano (o con el deep link `e=`).
- `/estaciones/cercanas`. Ese endpoint **recibe** lat/lon. El widget no lo llama.
- Todas las estaciones del catálogo. Muchas no tienen pronóstico y el endpoint gordo responde 422. El universo del haversine es solo el catálogo del widget.
- Viento, ráfaga, `u_max_diario`.
- Un cambio de `start_url`, splash, Digital Asset Links o del primer arranque de la TWA más allá del gancho descrito en la etapa 4.

## 3. Baseline (evidencia 2026-11-14)

| Hecho | Dónde |
|-------|--------|
| TWA `cl.rodaja.app`, host `app.rodaja.cl`, `startUrl` `/app/`, minSdk 21, target/compile 36, versionCode 7, Java, `androidbrowserhelper:2.6.2` | `mobile/twa/twa-manifest.json`, `mobile/twa/app/build.gradle` |
| SDK | `mobile/twa/local.properties` → `C:/Users/rodaja/.bubblewrap/android_sdk` |
| `LauncherActivity` ya pide `POST_NOTIFICATIONS` una vez y agrega `twa_boot=1` la primera URL | `mobile/twa/app/src/main/java/cl/rodaja/app/LauncherActivity.java` |
| `MAIN/LAUNCHER` y el `VIEW` https `autoVerify` están solo en `LauncherActivity` | `mobile/twa/app/src/main/AndroidManifest.xml` |
| `Application.onCreate` está vacío | `mobile/twa/app/src/main/java/cl/rodaja/app/Application.java` |
| Pronóstico por estación, sin `require_auth` | `api/endpoints/pronostico.R` `GET /estaciones/<id>/pronostico` |
| Auth global no bloquea anónimos; `require_auth` es opt-in | `api/utils/auth.R` |
| Hoy civil Chile | `SQL_HOY_CL` en `api/utils/pronostico_payload.R` |
| Códigos WMO → ícono | `ecPronosticoWmoCondition` en `nginx/html/mi-reporte/reporte_pronostico.js`; SVG en `nginx/html/icons/weather/` |
| Deep link de producto | `https://app.rodaja.cl/mi-reporte/?view=pronostico&e={id}` (`app.js` + `favorites.js`) |
| Dentro de la TWA, sin estación guardada **o** antes de onboard, el HTML fuerza el selector **aunque venga `e=`** | `nginx/html/mi-reporte/index.html` líneas 46-48: `_twa` basta para `ra-need-station` |
| Privacidad live dice que la ubicación solo se pide en el mapa del Explorador, no al abrir la app, y que no hay rastreo con la app cerrada | `nginx/html/legal/privacidad/index.html` |
| `cache_or_compute` pone `X-Cache`, no Cache-Control | `api/utils/cache.R` |
| Plumber concatena `api/endpoints/*.R` listados en `endpoint_files` | `api/plumber.R` |
| `.gitignore` ignora `mobile/twa/android/`. El proyecto real es `mobile/twa/app/`. `bubblewrap update` regenera manifest, gradle y `LauncherActivity` | `mobile/twa/README.md` |

No hay widget, Glance, WorkManager ni DataStore hoy.

## 4. Decisiones cerradas

1. **Catálogo** = estaciones en el bbox Chile con calidad `excelente|buena` y al menos una fila de `carta_del_mirador` en el hoy civil (filtrar por una variable, p. ej. `t_max`, para no multiplicar filas). Una sola query con JOIN, no un EXISTS por estación.
2. **Payload de hoy** (sin rankings, sin lat/lon):

```json
{
  "station_id": 1781,
  "name": "Quinta Normal",
  "comuna": "Santiago",
  "current_temp": 18.4,
  "tmin": 11.0,
  "tmax": 24.0,
  "precip_prob": 40,
  "precip_mm": 1.2,
  "condition_code": 2,
  "is_day": 1,
  "updated_at": "2026-11-14T15:00:00-03:00"
}
```

   - `current_temp`, `condition_code`, `is_day`: hora cuyo intervalo contiene “ahora”. Si no hay hora, null.
   - `tmin` / `tmax` / `precip_mm`: `t_min`, `t_max`, `pp` del día, vía `display_valor`.
   - `precip_prob`: máximo `pop` de las horas de hoy (0–100), o null.
   - `updated_at`: `fetched_at` en `America/Santiago`, offset `-03:00` o `-04:00` según la fecha.
   - Nombre: el mismo criterio que el resto de la API (`nombre_publico` / `estacion_nombre_view` si ya está cargado). No inventar otro formateo.
3. **Estación por defecto** si no hay permiso, no hay fix, o la más cercana queda a **más de 200 km**: la DMC cuyo nombre público contiene “Quinta Normal”, con pronóstico. El id real lo resuelve la etapa 1 en la base y lo escribe en el JSON de assets como `default_station_id`. Si no existe, la etapa se bloquea y lo dice; no elige otra ciudad en silencio.
4. **Umbral de movimiento:** 25 km entre el último fix guardado en el teléfono y el nuevo. Solo entonces se recalcula la más cercana, y solo si el modo es `nearest`. Modo `fixed` no se toca.
5. **Coordenadas** se quedan en DataStore (el fix anterior hace falta para los 25 km). No salen en ninguna URL, body ni header.
6. **Permiso:** solo `ACCESS_COARSE_LOCATION`, en primer plano, texto en español. `getCurrentLocation` con prioridad balanceada; si falla, `lastLocation`. Play Services. Si el teléfono no tiene Play Services, se usa la estación por defecto y el widget lo dice.
7. **minSdk 24.** Play rechazó el 21: la protección automática exige 24 o más. `requestPinAppWidget` solo si SDK ≥ 26 y `isRequestPinAppWidgetSupported()`. Si no, se sigue.
8. **Pin** una vez, después del permiso (concedido o rechazado), solo si no hay ya una instancia. Flag persistido. Rechazo = no insistir. Punto de reentrada: esquema `rodaja://pin` (no un filtro https).
9. **Deep link del tap:** Intent explícito a `cl.rodaja.app.LauncherActivity` con data `https://app.rodaja.cl/mi-reporte/?view=pronostico&e={id}`. Mismo patrón que `shortcuts.xml`. No `ACTION_VIEW` suelto.
10. **Bypass del selector:** en `index.html`, `view=pronostico` junto con `e` no agrega `ra-need-station`. `cambiar=1` sigue mostrando el selector. No tocar `twa_boot` ni el onboard de la web.
11. **Arranque:** `LauncherActivity` sigue siendo MAIN/LAUNCHER y abre la TWA directo. No hay pantalla de ubicación ni oferta de pin al abrir la app. El widget se agrega desde el lanzador. `notifs_asked` y `twa_boot` quedan como estaban. No cambiar el theme de `<application>`.
12. **Gancho en archivos generados:** el bloque en `LauncherActivity`, el manifest y `app/build.gradle` va entre comentarios `RODAJA_WIDGET_BEGIN` / `RODAJA_WIDGET_END`. `bubblewrap update` los borra: el handoff lo dice. No hay script que reescriba el manifest entero.
13. **Worker:** periódico 45 min, sin constraint de red (CONNECTED espera una red “validada” y la tarjeta se quedaba en Cargando). La primera lectura es un GET en el momento, en el proceso del widget. Backoff exponencial. `updatePeriodMillis = 0`. Sin foreground service. Último JSON válido en DataStore. Sin red: ese caché y su hora, estado “sin conexión”.
14. **Glance**, `SizeMode.Responsive`, 4×2 y 2×2, colores dinámicos con fallback &lt; API 31, claro/oscuro, `previewLayout`. Textos en español. Íconos: vector drawables a partir de los SVG ya existentes (día y noche: `luna`, `luna-parcial`, `luna-nubes`, `bruma`, `lluvia-y-luna` cuando `is_day = 0` y el mapeo web cambia el composite). El mapa de códigos es el de `ecPronosticoWmoCondition`, no uno nuevo.
15. **Configuración** del widget: `widgetFeatures="reconfigurable|configuration_optional"`. Opciones: “Usar la más cercana” o lista con buscador sobre el catálogo cacheado (assets si no hay red).
16. **No** se edita la privacidad live en esta cola. El texto para pegar queda en la etapa 5.
17. **versionCode** no se sube aquí. El humano lo pasa a 8 cuando arme el AAB.
18. **Commits** en la rama `widget-pronostico`, uno por etapa, sin push. Crear la rama en la etapa 1 sin cambiar de rama si el working tree de producción tiene otros cambios mezclados que no son de esta cola: en ese caso, commit solo de los paths de la etapa y dejar el aviso en el handoff. No hacer checkout que descarte trabajo ajeno.

## 5. Prohibido global

Ver el prompt fijo. Además: no reiniciar shiny; no `--build` de imágenes; no meter el widget en el diario ni en el compendio de pipeline.

## 6. Criterio de listo de una etapa

Plan escrito antes del diff. DoD de la sección de la etapa cumplida o el hueco está en el handoff. Filas propias de `criterios.md` sin `pendiente`. Humanos no fingidos.

## 7. Etapas

### Etapa 1 — `api_hoy` (agente)

**Objetivo:** dos GET livianos y el JSON de respaldo.

**Lee solo:** este master §2–4; `api/plumber.R` (lista `endpoint_files`); `api/endpoints/pronostico.R` (cómo no pide auth y cómo cachea); `api/utils/pronostico_payload.R` hasta `pronostico_estacion_visible`, `SQL_HOY_CL`, `display_valor` / `pronostico_fetch_*`; `api/utils/cache.R` (`cache_or_compute`); `api/scripts/smoke_pronostico_payload.R` (estilo del smoke); `api/utils/unidades_display.R` solo la firma de `display_valor`.

**Puede editar:** `api/utils/widget_pronostico.R` (nuevo), `api/endpoints/widget.R` (nuevo), una línea en `endpoint_files` de `api/plumber.R`, `api/scripts/smoke_widget_pronostico.R`, `api/scripts/export_widget_estaciones.R`, `mobile/twa/app/src/main/assets/estaciones_pronostico.json`, `docs/producto/widget_pronostico/contrato_api.md`, plan y handoff de la etapa, criterios, cola.

**Contrato HTTP:**

- `GET /widget/estaciones` → array JSON. Cache-Control `public, max-age=3600`. Redis vía `cache_or_compute`, ttl 3600.
- `GET /widget/estaciones/<id>/hoy` → objeto §4.2. 404 si no hay pronóstico o la estación no es visible. 400 si el id no es entero. Cache-Control `public, max-age=600` solo en 200. No cachear el error (el helper ya no cachea listas con `$error`).
- Sin `require_auth`. Sin rankings. Sin lat/lon en `/hoy`.

**DoD:** smoke ok; curl live de ambos; contrato con el ejemplo real (no el de mentira del §4 si los números difieren) y la tabla WMO → archivo svg; assets con `default_station_id` y el array `stations`.

**Riesgo:** query lenta. Medir el curl. Si pasa de ~2 s en frío, acotar por `id_variable` de `t_max` y decirlo en el handoff. Restart de api en producción: el endpoint nuevo queda público.

**Humano:** no.

### Etapa 2 — `android_logica` (agente)

**Objetivo:** lógica testeable en JVM, sin UI.

**Lee solo:** handoff 1; `contrato_api.md`; `mobile/twa/app/build.gradle`; `mobile/twa/build.gradle`; este master §4.3–4.5.

**Puede editar:** fuentes nuevas bajo `mobile/twa/app/src/main/java/cl/rodaja/app/widget/` que no sean Glance (haversine, selector, parser); `mobile/twa/app/src/test/`; el bloque `RODAJA_WIDGET_*` de `app/build.gradle` (plugin Kotlin, JUnit, dependencia mínima para tests JVM). No Glance todavía si se puede testear el parser con `org.json` / kotlinx.serialization ya justificado. Preferir `org.json`, que ya vendrá con Android, y tests JVM puros sin android.jar si el parser no lo necesita.

**Reglas de selección (cubiertas por tests):**

- Haversine en km.
- Más cercana dentro del catálogo.
- Si la distancia &gt; 200 km → estación por defecto, motivo `lejos`.
- Catálogo vacío → estación por defecto.
- Modo `fixed` ignora el fix nuevo.
- Modo `nearest` solo recalcula si no hay fix previo o el desplazamiento es ≥ 25 km.
- Parser: campos null, `condition_code` entero, rechaza JSON con `rankings` sin fallar (los ignora).

**DoD:** `gradlew.bat test` (o el task de unit test que quede) verde; `gradlew.bat assembleDebug` verde; minSdk sigue 21. C01–C05 no se reabren.

**Riesgo:** el plugin Kotlin rompe el Java de Bubblewrap. Si assembleDebug falla, el handoff lleva el error completo; no se sube minSdk para “arreglarlo”.

**Humano:** no.

### Etapa 3 — `widget_ui` (agente)

**Objetivo:** la tarjeta y el worker, con estación fija de prueba (la por defecto). Sin ubicación y sin pin (etapa 4).

**Lee solo:** handoff 2; `contrato_api.md`; `AndroidManifest.xml`; este master §4.13–4.15; `reporte_pronostico.js` solo `ecPronosticoWmoCondition` y `ecPronosticoNightComposite`.

**Puede editar:** clases Glance, worker, DataStore, drawables, `res/xml` del widget, bloque marcado del manifest (`receiver` del widget, sin filtro https), bloque marcado de `app/build.gradle` (Glance, WorkManager, DataStore).

**Contenido:** nombre, comuna, ícono, temperatura actual o “—”, mín/máx, prob. y/o mm, hora local. Estados: cargando, sin conexión (caché + hora), sin ubicación (“Toca para activar ubicación” — el tap de ese estado puede no hacer nada todavía si la actividad de permiso es etapa 4; dejar el PendingIntent apuntando a la actividad que la etapa 4 creará, o un no-op documentado), error genérico.

**DoD:** `assembleDebug` verde. `updatePeriodMillis` es 0. No hay `<service>` foreground. Preview layout presente.

**Riesgo:** tamaño del APK por Compose. Aceptado. No migrar el resto de la app a Compose.

**Humano:** no.

### Etapa 4 — `ubicacion_pin` (agente)

**Objetivo:** cercanía, oferta única, configuración, deep link que no abre Chrome.

**Lee solo:** handoff 3; `LauncherActivity.java` entero; `AndroidManifest.xml`; `nginx/html/mi-reporte/index.html` líneas 29-49; `shortcuts.xml` (el intent a `LauncherActivity`); este master §4.6–4.12 y §4.15.

**Puede editar:** actividades nuevas (`OnboardingActivity`, `WidgetConfigureActivity`, `WidgetOfferActivity`); DataStore de modo/estación/fix; bloque marcado de `LauncherActivity` (solo el early-return de primera vez); bloque marcado del manifest (permiso coarse, actividades, esquema `rodaja`, `widgetFeatures`); `nginx/html/mi-reporte/index.html` solo la condición de `ra-need-station`; strings de justificación.

**Secuencia de primera vez:** onboarding opaco → explicación corta (“Ver el pronóstico en tu pantalla de inicio”) → permiso coarse → pin si aplica → `LauncherActivity` con extra de ya visto → `super.onCreate()` como hoy.

**Esquema de reentrada (documentar en el handoff, este texto):**

```text
intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;end
```

Con estación fija desde la web:

```text
intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;S.e=1781;end
```

`S.e` guarda modo `fixed` y refresca el widget. No agregar botón en `/app/` en esta etapa.

**Deep link del tap:** no debe llevar `autoVerify`. Comprobar en el manifest que el único `autoVerify` https sigue siendo el de `LauncherActivity`.

**Bypass web**, sustituir la condición actual por la equivalente de:

- si `cambiar=1` → selector;
- si `view=pronostico` y hay `e` → no selector;
- si no, la lógica actual (`_twa` o sin favorita y sin `e`).

**DoD:** assembleDebug. Revisión del manifest (permisos y filtros) citada en el handoff. El HTML de mi-reporte no manda al selector cuando la URL trae `view=pronostico&e=`. No se comprobó en un teléfono: eso es gap explícito, no un `ok` de dispositivo.

**Riesgo:** el early-return se equivoca y la TWA no abre nunca. El extra de “ya visto” tiene que estar puesto **antes** de relanzar, y un fallo de la actividad de onboarding debe igual relanzar la TWA.

**Humano:** no en código. Probar en Samsung queda para la guía (etapa 5) y el operador.

### Etapa 5 — `guia_privacidad` (mixto)

**Objetivo:** que el operador pueda probar y declarar los datos. No publicar.

**Lee solo:** handoffs 1–4; `nginx/html/legal/privacidad/index.html` el ítem de ubicación (no editarlo); este master §8.

**Puede editar:** `docs/producto/widget_pronostico/guia_prueba.md`, `docs/producto/widget_pronostico/data_safety.md`. Nada de `nginx/html/legal/`.

**DoD:** la guía cubre emulador (`adb emu geo fix <lon> <lat>`), permiso concedido, permiso denegado, offline, cambio de ubicación, estación fijada, modo oscuro, tap abre la TWA. Data safety dice qué marcar y el párrafo que hay que agregar a la privacidad **cuando** se suba el AAB. TAREAS HUMANAS con casillas. Status `hecho_con_gaps`.

**Humano:** Play Console, política live, teléfono, AAB. El agente no los hace.

### Etapa 6 — `cierre_qa` (agente)

**Lee solo:** los seis handoffs, `criterios.md`, este master §1.

**Puede editar:** `docs/producto/widget_pronostico/CIERRE.md`, criterios (solo para bajar a `gap_documentado` una fila que ya no se sostiene), cola.

**Hace:** re-correr el curl de los dos GET y `assembleDebug`. No codea features. Escribe perfectas / críticas / aceptadas.

**Humano:** no, salvo las casillas que la etapa 5 dejó abiertas: se listan, no se marcan hechas.

## 8. Tareas 100 % humanas

- Subir versionCode a 8 y correr `bubblewrap build` (reaplicar a mano los bloques `RODAJA_WIDGET_*` si el build regenera).
- Internal testing / Play. No la hace el agente.
- Data safety: ubicación **aproximada**, recogida, **no compartida**, finalidad funcionalidad de la app, solo en primer plano (con la app en uso). No hay ubicación en segundo plano. El id de estación no es dato de ubicación. El fix grueso se guarda solo en el teléfono para no recalcular si el movimiento es &lt; 25 km.
- Privacidad live, al publicar: hoy el texto dice que la ubicación solo se pide en el mapa y no al abrir la app. Hay que agregar que la app Android puede pedir ubicación aproximada, una vez y en primer plano, para elegir la estación del widget, y que esas coordenadas no se envían al servidor (a diferencia del mapa, que sí llama a estaciones cercanas). No decir que el widget rastrea con la app cerrada: el worker solo pide el pronóstico del id ya guardado.
- Probar en un Samsung con One UI los casos de la guía.

## 9. Índice de lectura

| Ruta | Para |
|------|------|
| `api/utils/pronostico_payload.R` | visibilidad, hoy Chile, horas |
| `api/endpoints/pronostico.R` | endpoint gordo que no hay que usar desde el widget |
| `api/endpoints/estaciones.R` | `/cercanas` existe y no se usa |
| `nginx/html/mi-reporte/reporte_pronostico.js` | mapa WMO |
| `nginx/html/mi-reporte/index.html` | selector que tapa el deep link |
| `mobile/twa/app/src/main/java/cl/rodaja/app/LauncherActivity.java` | gancho de primera vez |
| `mobile/twa/app/src/main/AndroidManifest.xml` | no duplicar autoVerify |
