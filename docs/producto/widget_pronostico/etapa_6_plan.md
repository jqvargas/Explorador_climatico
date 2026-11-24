# Plan de implementación — etapa 6 `cierre_qa`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

Releer los handoffs 1–5, repetir el curl de los dos GET y `assembleDebug`, y escribir `CIERRE.md` con perfectas, críticas y aceptadas, sin codear features ni marcar las casillas humanas de la etapa 5.

## 2. Auditoría: qué se leyó

Solo la lectura mínima de la etapa 6. No se reabrió producto.

| Ruta | Qué se confirmó |
|------|-----------------|
| `PLAN_WIDGET_PRONOSTICO.md` §1 | Listo: `assembleDebug` con minSdk 21, pruebas JVM ya hechas, los dos GET públicos con Cache-Control y sin rankings, guía y Data safety escritos. El AAB no se publica en esta cola |
| `etapa_1_handoff.md` | Default real **3939** «Quinta Normal Santiago». `GET /widget/estaciones` y `GET /widget/estaciones/3939/hoy` públicos. Catálogo frío ~6,3 s; SQL caliente < 1 s. Sin lat/lon ni rankings en `/hoy`. Cache-Control 3600 y 600. Errores `no-store` |
| `etapa_2_handoff.md` | JVM verde (haversine, selector, parser). `assembleDebug` BUILD SUCCESSFUL. minSdk 21. Sin cliente HTTP |
| `etapa_3_handoff.md` | Glance 1.1.1, `updatePeriodMillis` 0, sin foreground service. `assembleDebug` verde. El widget no se colocó en un emulador |
| `etapa_4_handoff.md` | `hecho_con_gaps`. Coarse only. Deep link y bypass web medidos en el navegador, no en un teléfono. Tres `autoVerify` https siguen en `LauncherActivity` |
| `etapa_5_handoff.md` | Guía y `data_safety.md` escritos. Privacidad live sin tocar. Cinco casillas humanas abiertas. Status `hecho_con_gaps` |
| `criterios.md` | C01–C15 en `ok`. C16 `pendiente`, owner 6. La etapa 6 re-verifica las filas `ok` que dependen de curl o de `assembleDebug`, y solo baja a `gap_documentado` una fila que ya no se sostiene |

## 3. Estado actual (evidencia)

- Cola: etapas 1–3 `hecho`, 4 y 5 `hecho_con_gaps`, 6 `en_curso`. La 5 cumple el prerequisito.
- C16 sigue `pendiente`. No existe `CIERRE.md` de este widget.
- La etapa 5 dejó estos comandos sin correr: `curl.exe` de `/widget/estaciones/3939/hoy` y `gradlew.bat assembleDebug`.
- Las casillas de emulador, Samsung, versionCode 8, Play y privacidad live siguen abiertas. Esta etapa las copia; no las tilda.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| Cierre con perfectas / críticas / aceptadas, citando la re-medición | `docs/producto/widget_pronostico/CIERRE.md` | Los dos GET y `assembleDebug` tienen comando y resultado. Las cinco casillas de la etapa 5 aparecen sin tildar | Si un GET deja de cumplir el contrato, esa fila baja a `gap_documentado` y entra en críticas. No se parcha la API |
| C16 deja de estar `pendiente` | `criterios.md` | Evidencia = esta re-medición. C01–C15 no se reescriben si siguen sosteniéndose | Un conteo distinto de 1975 no tumba C01: el catálogo es el de hoy |
| Handoff y cola | `etapa_6_handoff.md`, `widget_pronostico_cola.json` | `hecho` si curl y compile sostienen C01–C03 y C08–C09 y C16 pasa a `ok`. `hecho_con_gaps` si una fila baja o la re-medición queda incompleta | Las casillas humanas no convierten solas el status: ya son el gap de la etapa 5 y se listan igual |

No hay diff de API, manifest, gradle, HTML ni legal.

## 5. Tareas HUMANAS

Ninguna nueva. Se listan, sin tildar, las de la etapa 5:

- Recorrer `guia_prueba.md` en un emulador con Google Play (API 31 o más).
- Los mismos casos en un Samsung con One UI.
- Subir versionCode a 8 y correr `bubblewrap build`, reponiendo `RODAJA_WIDGET_*` si regenera.
- Internal testing y Data safety según `data_safety.md`.
- Al subir el AAB, pegar en la privacidad live el párrafo de `data_safety.md`.

## 6. Dependencias

- Handoff anterior: etapa 5 `hecho_con_gaps`. Alcanza.
- Restart api (`scripts/dc.ps1 restart api`) solo si esta etapa toca la API: no.
- assembleDebug: sí, desde `mobile/twa`, sin `--build` de Docker y sin bubblewrap.

## 7. Orden de implementación

1. Este plan.
2. Curl de `GET /widget/estaciones` y `GET /widget/estaciones/3939/hoy` (status, Cache-Control, forma del JSON, ausencia de rankings; en `/hoy` también ausencia de lat/lon).
3. `gradlew.bat assembleDebug` y comprobar que minSdk sigue 21.
4. Escribir `CIERRE.md`. Actualizar solo C16, salvo que una fila `ok` de curl o compile ya no se sostenga.
5. Handoff y cola.
6. Un commit en `widget-pronostico`, solo los docs de esta etapa. Sin push. Sin checkout que tire trabajo ajeno.

## 8. Verificación

- `curl.exe -sD - -o <body> https://app.rodaja.cl/api/widget/estaciones` → 200, `Cache-Control: public, max-age=3600`, array de objetos `id/name/comuna/lat/lon`, sin clave `rankings`.
- `curl.exe -sD - https://app.rodaja.cl/api/widget/estaciones/3939/hoy` → 200, `Cache-Control: public, max-age=600`, objeto del contrato, sin `lat`, `lon` ni `rankings`.
- `cd mobile/twa` y `.\gradlew.bat assembleDebug --console=plain` → BUILD SUCCESSFUL. `minSdkVersion 21` sigue en `app/build.gradle`.

## 9. Fuera de alcance

Código del widget, endpoints, tests nuevos, `index.html`, assetlinks, splash, `start_url`, versionCode, bubblewrap, Play Console, keystore, teléfono, `nginx/html/legal/`, restart de api o shiny, push.

## 10. Riesgos

- El catálogo en frío puede volver a pasar de 2 s. Es el gap ya aceptado de la etapa 1 (nombres en R, Redis 1 h). No se toca el SQL.
- `assembleDebug` puede fallar si el SDK o un bloque `RODAJA_WIDGET_*` se movió. El error completo va al handoff. No se sube minSdk para “arreglarlo”.
- Working tree de producción con cambios ajenos. El commit nombra paths. No se cambia de rama si eso descarta trabajo.

## 11. Self-check

- [x] Filas `etapa_owner = 6` de `criterios.md` cubiertas (C16; las de curl y compile se re-miden y solo se bajan si dejan de sostenerse)
- [x] Prohibidos globales del prompt fijo respetados
- [x] Humanos no fingidos (Play, keystore, teléfono, AAB)
- [x] No se sube minSdk, no se toca assetlinks ni el splash
- [x] Lat/lon no se envían al servidor
