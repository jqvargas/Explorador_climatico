# Prompt fijo — widget de pronóstico (TWA)

**No edites este archivo para cada etapa.** El agente lee [widget_pronostico_cola.json](widget_pronostico_cola.json) y el master [PLAN_WIDGET_PRONOSTICO.md](PLAN_WIDGET_PRONOSTICO.md).

Este prompt se pega **6 veces**, siempre igual, en un Agent de coding con **contexto limpio**.

---

## Qué pegas (siempre igual)

Copia **solo** este recuadro:

```text
Widget nativo de pronóstico de hoy para la TWA Android de rodaja (cl.rodaja.app). La estación se elige en el teléfono. Al servidor solo viaja el id de estación.

Lee y sigue al pie de la letra, en este orden:
1. docs/producto/widget_pronostico_PROMPT_FIJO.md (este contrato)
2. docs/producto/PLAN_WIDGET_PRONOSTICO.md (plan maestro; baseline 2026-11-14). Las decisiones de producto de ahí están cerradas: no las reabras.
3. docs/producto/widget_pronostico_cola.json — reclama la primera etapa con status "pendiente" de menor n. Márcala "en_curso" de inmediato. Si la etapa n-1 no está en hecho o hecho_con_gaps, PARA.
4. docs/producto/widget_pronostico/criterios.md — solo las filas con etapa_owner = n
5. El handoff de la etapa anterior, si existe: docs/producto/widget_pronostico/etapa_<n-1>_handoff.md

NO pushear. NO publicar el AAB. NO bubblewrap update ni bubblewrap init. NO tocar assetlinks, el splash ni start_url. NO ACCESS_BACKGROUND_LOCATION ni ACCESS_FINE_LOCATION. NO enviar lat/lon al API (prohibido /estaciones/cercanas desde el widget). NO rankings ni alertas en el JSON del widget. NO bajar minSdk de 24 (Play, protección automática). NO velo/WebSocket, HTTP/3 ni Play Billing.

Commits: sí, en la rama widget-pronostico, uno al cerrar la etapa, solo los archivos de esta etapa y los docs de la cola si siguen sin commit. No commits en main. No --amend.

Este workspace ES producción (app.rodaja.cl). Si la etapa toca la API, reinicia tú con scripts/dc.ps1 restart api y verifica con curl.exe. No reinicies shiny.
```

Un solo agente a la vez. Si otro dejó la etapa `en_curso` y no hay handoff, no la reclames.

---

## Contrato del agente

### Ritmo (obligatorio)

1. **Reclamar** la etapa (`pendiente` → `en_curso`).
2. **Auditar solo** la lectura mínima de esa etapa en el master.
3. **Primera entrega = un plan**, `docs/producto/widget_pronostico/etapa_<n>_plan.md`, copiando `plan/_plantilla.md`. Self-check. Si falta una decisión que el master no cerró: `bloqueado`, handoff, parar.
4. **Ejecutar** el diff mínimo. Reusar helpers. No abstracciones nuevas.
5. **Cerrar:** criterios de esta etapa, `etapa_<n>_handoff.md` (plantilla `handoff/_plantilla.md`), cola `hecho` o `hecho_con_gaps`.

### Dueño mixto

El agente prepara copy y checklist. No abre Play Console, no firma, no instala en un teléfono. Esas casillas quedan en TAREAS HUMANAS. Status `hecho_con_gaps` mientras el operador no las confirme.

### Prohibido (todas las etapas)

- Saltar etapas o codear la N+1.
- Segundo `intent-filter` https con `autoVerify` fuera de `LauncherActivity`.
- Cambiar el theme de la application, el splash o `getLaunchingUrl()` / `twa_boot` / `notifs_asked`.
- `bubblewrap update`, `bubblewrap init`, regenerar el proyecto.
- Pedir ubicación en segundo plano o mandar coordenadas.
- Mostrar temperatura observada como si fuera el pronóstico.
- Mezclar ráfaga y viento máximo. Este widget no muestra viento.
- Foreground service. `updatePeriodMillis` distinto de 0.
- Insistir con el pin si el usuario lo rechazó.
- Editar la página legal live (`nginx/html/legal/privacidad/`). El texto nuevo va en un doc; el humano lo publica al subir el AAB.
- Fingir Play Console, keystore, Samsung físico o un AAB publicado.
- `Start-Process` del diario.

### Checklist al cerrar

- [ ] Cola actualizada
- [ ] Plan y handoff existen
- [ ] Filas `etapa_owner = n` no quedan `pendiente`
- [ ] Handoff tiene comando + resultado, gaps y TAREAS HUMANAS
- [ ] Si tocaste la API: restart hecho por el agente y curl citado
- [ ] Commit en `widget-pronostico` (sin push)
