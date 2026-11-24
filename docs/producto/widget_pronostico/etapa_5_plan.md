# Plan de implementación — etapa 5 `guia_privacidad`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

Dejar la guía para que el operador pruebe el widget en emulador y en un Samsung, y el texto de Data safety más el párrafo de privacidad para pegar cuando se publique el AAB, sin editar la página legal live ni publicar nada.

## 2. Auditoría: qué se leyó

Solo la lectura mínima de la etapa 5. No reauditar el repo ni reabrir producto.

| Ruta | Qué se confirmó |
|------|-----------------|
| `PLAN_WIDGET_PRONOSTICO.md` §8 | versionCode 8 y `bubblewrap build` son humanos; Data safety es ubicación aproximada, recogida, no compartida, funcionalidad de la app, solo en primer plano; el id no es ubicación; el fix se queda en el teléfono; el párrafo de privacidad se agrega al publicar y no dice que el widget rastrea con la app cerrada; Samsung One UI lo prueba el operador |
| `etapa_1_handoff.md` | Default real 3939 «Quinta Normal Santiago», comuna Estación Central. El 1781 del master es otra estación. `GET /widget/estaciones/<id>/hoy` no lleva lat/lon. Catálogo frío ~6 s; el teléfono usa el asset |
| `etapa_2_handoff.md` | ≥ 25 km recalcula en modo `nearest`; > 200 km → default, motivo `lejos`. `fixed` ignora el fix. minSdk 21 |
| `etapa_3_handoff.md` | Estados en español, «Sin conexión» con caché y hora. El cartel offline no aparece solo por esperar 45 min: el periódico no corre sin red. `updatePeriodMillis` 0. Sin foreground service. Glance claro/oscuro. Pantalla de ubicación todavía no era clickable en esa etapa |
| `etapa_4_handoff.md` | Onboarding opaco, coarse, pin una vez (API ≥ 26), «Abrir rodaja» si no hay pin. Sin Play Services: aviso «Sin servicios de Google. Estación por defecto.» y no hay `LocationManager`. El worker no lee un fix nuevo. «Usar la más cercana» sí recalcula. `S.e` fija modo `fixed`. Bypass `view=pronostico&e=` ya está en el HTML servido. Teléfono no probado. Theme nativo claro; el widget sí sigue el modo oscuro |
| `nginx/html/legal/privacidad/index.html` ítem de ubicación | El bullet vivo dice que la ubicación solo se pide en el mapa, no en Inicio ni Mi reporte, y que no hay rastreo con la app cerrada. No se edita |

## 3. Estado actual (evidencia)

- C15 sigue `pendiente`. C01–C14 están `ok`. La cola tiene la etapa 5 en `en_curso` y la 4 en `hecho_con_gaps`.
- No hay `guia_prueba.md` ni `data_safety.md`.
- El handoff 4 deja explícito que nadie concedió el permiso, ancló el widget ni tocó la tarjeta. Esta etapa escribe cómo hacerlo. No marca esas pruebas como hechas.
- Frase viva que queda corta cuando exista el widget (línea 71 de la privacidad): la ubicación aproximada «solo si usas el mapa», y «No las pedimos en Inicio (`/app/`)». El worker, con la app cerrada, pide el pronóstico del id ya guardado; no pide un fix.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| Guía: `adb emu geo fix` longitud luego latitud; permiso sí; permiso no; offline; movimiento; estación fijada; modo oscuro; tap abre la TWA; nota Samsung | `docs/producto/widget_pronostico/guia_prueba.md` | El archivo nombra los ocho casos del DoD y el comando con el orden lon/lat | Sin Play Services no hay fix: estación por defecto. El modo avión no dispara solo el cartel «Sin conexión». Abrir la TWA no recalcula los 25 km |
| Qué marcar en Data safety y el párrafo listo para pegar al subir el AAB | `docs/producto/widget_pronostico/data_safety.md` | Cita el §8. No instruye marcar precisa ni segundo plano. El párrafo dice que la coordenada no sale del teléfono | Si el formulario de Play define «recopilada» como «sale del dispositivo», no inventar un envío |
| C15 deja de estar `pendiente` | `criterios.md` | Evidencia = estos dos docs + diff vacío de la privacidad live | La ejecución humana queda en la columna de gap y en el handoff |
| Cerrar la etapa | `etapa_5_handoff.md`, `widget_pronostico_cola.json` | Status `hecho_con_gaps` | Las casillas humanas siguen abiertas |

Al redactar la guía, si un rótulo visible no está en los handoffs, se lee el string ya compilado en recursos del widget y no se modifica.

## 5. Tareas HUMANAS

Quedan abiertas. El agente no las ejecuta ni las tilda.

- Recorrer `guia_prueba.md` en un emulador (imagen con Google Play, API ≥ 26).
- Los mismos casos en un Samsung con One UI.
- Subir versionCode a 8, `bubblewrap build`, y reponer a mano los bloques `RODAJA_WIDGET_*` si el build los borra.
- Internal testing. No es parte de esta cola publicar a producción.
- Marcar Data safety en Play Console como dice `data_safety.md`.
- Al subir el AAB, pegar el párrafo en la privacidad live. Hasta entonces la página no cambia.

## 6. Dependencias

- Handoff anterior: etapa 4 `hecho_con_gaps`. Alcanza. El teléfono sin probar es el motivo de la guía, no un bloqueo.
- Restart api (`scripts/dc.ps1 restart api`) solo si esta etapa toca la API: no.
- assembleDebug: no. Lo vuelve a correr la etapa 6.

## 7. Orden de implementación

1. Confirmar rótulos que la guía va a citar (solo lectura).
2. Escribir `guia_prueba.md`.
3. Escribir `data_safety.md` con el párrafo de privacidad.
4. Comprobar que `nginx/html/legal/` no tiene diff de esta etapa.
5. Pasar C15 a `ok` con el gap humano en la columna, handoff, cola `hecho_con_gaps`.
6. Commit en `widget-pronostico`, solo estos docs.

## 8. Verificación

- Los dos markdown existen y cubren el DoD (búsqueda de los casos y de `adb emu geo fix`).
- `git diff -- nginx/html/legal/privacidad/index.html` vacío respecto de esta etapa.
- No hay AAB publicado, ni `bubblewrap`, ni cambios de manifest, API o minSdk.

## 9. Fuera de alcance

Código Android, API, `index.html` de Mi reporte, assetlinks, splash, `start_url`, versionCode, Play Console, keystore, instalar en un teléfono, editar `nginx/html/legal/`.

## 10. Riesgos

- Una guía que diga que el modo avión, a los 45 min, muestra «Sin conexión». El handoff 3 dice que el trabajo periódico no corre sin red. La guía tiene que pedir quitar y volver a poner el widget, o un fetch que ya empezó.
- Declarar en Play que la coordenada se envía, para que «recopilada» cuadre. Es falso: solo viaja el id. El doc lo dice.
- Tildar las casillas humanas. Status final `hecho_con_gaps`, no `hecho`.

## 11. Self-check

- [x] Filas `etapa_owner = 5` de `criterios.md` cubiertas (C15: los dos docs + página legal sin tocar)
- [x] Prohibidos globales del prompt fijo respetados
- [x] Humanos no fingidos (Play, keystore, teléfono, AAB)
- [x] No se sube minSdk, no se toca assetlinks ni el splash
- [x] Lat/lon no se envían al servidor
