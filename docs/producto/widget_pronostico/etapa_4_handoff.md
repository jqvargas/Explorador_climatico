# Handoff etapa 4 — `ubicacion_pin`

**Fecha:** 2026-11-15
**Status en cola:** `hecho_con_gaps`
**Plan:** `docs/producto/widget_pronostico/etapa_4_plan.md`

## Qué se hizo

- Primera apertura: `shouldLaunchImmediately()` devuelve false, así el padre no llama a `launchTwa()`. Después de `super.onCreate` (obligatorio: sin él el framework tira `SuperNotCalledException`) se abre `OnboardingActivity` y se cierra el launcher. El flag `widget_offer_seen` se escribe con `commit` antes de relanzar, y el intent lleva el extra `rodaja_offer_seen`.
- Onboarding opaco, texto en español, permiso solo `ACCESS_COARSE_LOCATION`. Luego Play Services: `getCurrentLocation` con prioridad balanceada y, si falla, `lastLocation`. Sin Play Services no hay `LocationManager`: estación por defecto y el aviso «Sin servicios de Google. Estación por defecto.»
- Pin solo si SDK ≥ 26, el launcher lo soporta y no hay instancias. El rechazo no reabre el arranque: queda el botón «Abrir rodaja». El esquema `rodaja://pin` está en `WidgetOfferActivity`, sin `autoVerify`. `S.e` guarda modo `fixed` y refresca si ya hay un widget. No lee ubicación.
- Tap del pronóstico: `ACTION_MAIN` explícito a `LauncherActivity` con `https://app.rodaja.cl/mi-reporte/?view=pronostico&e={id}`. «Toca para activar ubicación» abre el onboarding solo de permiso. El aviso vive en la clave `notice`. `writeContent` no la toca, así el worker no la pisa.
- Configuración opcional: «Usar la más cercana» (recalcula aunque el último movimiento sea corto) o lista del asset con buscador. Modo `fixed` no se mueve con un fix nuevo. Los 25 km siguen en `StationSelector` cuando ya hay un fix previo.
- `index.html`: `cambiar=1` muestra el selector; `view=pronostico` con `e` no lo muestra, tampoco dentro de la TWA. El resto de la condición, incluido `twa_boot`, quedó igual.

## Qué se verificó

- [x] Desde `mobile/twa`: `.\gradlew.bat test assembleDebug --console=plain` → **BUILD SUCCESSFUL** in 37s. JDK 21, minSdk del manifest fusionado `21`.
- [x] Debug y release, 0 failures: HaversineTest 3, StationSelectorTest 12, HoyParserTest 5, WidgetLogicTest 9, WidgetStage4Test 8.
- [x] Manifest fusionado debug: `ACCESS_COARSE_LOCATION`. No aparecen `ACCESS_FINE_LOCATION`, `ACCESS_BACKGROUND_LOCATION` ni `SystemForegroundService`. Los tres `autoVerify` https siguen dentro de `LauncherActivity` (app.rodaja.cl, accounts, clerk). `WidgetOfferActivity` tiene `scheme=rodaja` `host=pin` sin `autoVerify`. `updatePeriodMillis` sigue 0.
- [x] `curl.exe` a `https://app.rodaja.cl/mi-reporte/?view=pronostico&e=3939`: 200, `cf-cache-status: DYNAMIC`, el HTML ya trae `_viewPron`. En el navegador, con `sessionStorage rodaja_twa=1`, sin favorita y sin onboard: la clase es `ra-app` y `#ra-home` está en `display:none`. Con `cambiar=1` la clase es `ra-need-station`, el home en `display:block` y se ve el buscador. Sin `e`, el selector sigue. Se restauró el `localStorage` de favoritos de esa pestaña.
- [x] C01–C10 no se reabrieron. C11–C14 quedaron en `ok`, con el teléfono en la columna de gap.
- [x] No se tocó la API. No hubo restart. No se reinició shiny.

## Gaps / caveats / qué puede fallar después (obligatorio)

El DoD de código está cumplido. Esto no se vio en un teléfono.

| Riesgo | Señal | Impacto | Mitigación o etapa dueña |
|--------|-------|---------|--------------------------|
| Sin teléfono | Nadie concedió el permiso, ancló el widget ni tocó la tarjeta | El deep link podría abrir Chrome, o el early-return dejar la TWA sin abrir | Etapa 5, guía. No marcar un Samsung como hecho |
| `bubblewrap update` | Borra los bloques `RODAJA_WIDGET_*` de `LauncherActivity`, el manifest y gradle | Se pierden el early-return, el permiso, las actividades y Play Services | Reponer los bloques a mano. No correr bubblewrap en esta cola |
| Ubicación solo en primer plano | Abrir la TWA no lee un fix nuevo. El worker tampoco | Los 25 km corren en el onboarding, en «Usar la más cercana» y en «Toca para activar ubicación». No al usar la app | No pedir `ACCESS_BACKGROUND_LOCATION` para «arreglarlo» |
| Catálogo congelado | La lista es `estaciones_pronostico.json` | Una estación nueva no sale en el buscador hasta otro build. `S.e` igual guarda el id | No bajar `/widget/estaciones` desde el widget en esta etapa |
| API 21–27 | El launcher ignora `configuration_optional` | Al poner el widget puede abrir la configuración sí o sí | Salir devuelve `RESULT_OK` para no borrarlo. No se sube minSdk |
| Pin sin callback de rechazo | El sistema no avisa si el usuario cancela | Si no hubiera botón, la TWA no seguiría | «Abrir rodaja» sigue. No se vuelve a pedir al abrir la app |
| Relanzamiento con data | El padre cierra una segunda `LauncherActivity` si sigue viva otra y el intent no trae URL | La TWA no abriría | El relanzamiento lleva la URL original o `launchUrl` |
| Pantallas nativas claras | `WidgetOpaque` es `Theme.DeviceDefault.Light` | Onboarding y configuración no siguen el modo oscuro. El widget sí, vía Glance | No cambiar el theme de `<application>` |
| `S.e` solo como extra | `intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;S.e=1781;end` | Un `?e=` en la URL no fija la estación | No agregar un botón en `/app/` |
| `index.html` sucio | El commit de la etapa no lo incluye | En la rama el bypass no está; en el servidor sí | Al commitear Mi reporte, conservar `_viewPron` |

## TAREAS HUMANAS

- [x] ninguna

## Pendiente / no es de esta etapa

- Guía de emulador, Data safety, el párrafo de privacidad, la página legal live, el AAB, versionCode.
- Probar en un Samsung los casos de la guía.

## Para la etapa siguiente

- No reabrir: minSdk 21, Glance 1.1.1, sin `FOREGROUND_SERVICE`, `updatePeriodMillis` 0, sin lat/lon en ninguna URL, sin `ACCESS_FINE_LOCATION` ni background, el `doFirst` de `android.jar`, el theme de `<application>`, `twa_boot` y el splash.
- Flag de oferta: `SharedPreferences` `rodaja_twa` / `widget_offer_seen`. Extra del relanzamiento: `rodaja_offer_seen`.
- DataStore `pronostico_widget`: además de `state`, `json` y `station_id`, ahora `mode` (`nearest` o `fixed`), `fix_lat`, `fix_lon`, `notice`. El worker no escribe esas tres últimas.
- Esquema, para la guía:

```text
intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;end
intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;S.e=1781;end
```

- `git add -f` de `mobile/twa/app/`: sigue en `mobile/twa/.gitignore`. No agregar `app/build`.
- Comando: `cd mobile/twa` y `.\gradlew.bat test assembleDebug --console=plain`.
- El HTML de Mi reporte ya está en producción. No hace falta restart.
- `nginx/html/mi-reporte/index.html` no entra en el commit: contra HEAD el archivo ya traía el rediseño de la página (135 líneas), no solo el bypass. El bypass está en el archivo servido. No reemplazar la condición `_viewPron` al commitear esa página.

## Decisiones de producto tomadas aquí

Ninguna. El aviso de sin Play Services es el que el master ya pedía. «Usar la más cercana» recalcula una vez porque es una orden, no un fix automático: los 25 km siguen cuando el modo ya es `nearest` y hay un fix previo. Retornar antes de `super.onCreate` no es viable en esta TWA; el desvío es `shouldLaunchImmediately() == false`.
