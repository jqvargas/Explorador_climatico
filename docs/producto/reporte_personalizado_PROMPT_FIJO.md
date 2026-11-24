# Prompt fijo: Reporte personalizado (8 etapas + etapa 0 humana)

**No edites este archivo para cada etapa.** El agente lee [reporte_personalizado_cola.json](reporte_personalizado_cola.json) y el plan maestro [PLAN_REPORTE_PERSONALIZADO.md](PLAN_REPORTE_PERSONALIZADO.md).

Se pega una vez por etapa, siempre igual, en un agente con contexto limpio. **No se pega hasta que la etapa 0 (decisiones del operador) esté `hecho`.**

---

## Qué pegas (siempre igual)

```text
Construcción del producto «Reporte personalizado» de rodaja.cl: reemplaza al Reporte PDF por estación. El usuario elige un punto o un área en una sección nueva de la app y recibe un PDF inmutable, verificable por QR, generado sobre las estaciones que eligió.

Lee y sigue al pie de la letra, en este orden:
1. docs/producto/reporte_personalizado_PROMPT_FIJO.md (este contrato)
2. docs/producto/PLAN_REPORTE_PERSONALIZADO.md (§1 a §5 y la sección de tu etapa; las decisiones de la §3 ya están cerradas por el operador)
3. docs/producto/reporte_pdf/PREREQUISITOS_SEIA.md, parte A
4. docs/producto/reporte_personalizado_cola.json: reclama la primera etapa con status "pendiente" de menor n y márcala "en_curso" de inmediato. Si la etapa n-1 no está en hecho o hecho_con_gaps, PARA.
5. docs/producto/reporte_personalizado/criterios.md: solo las filas con etapa_owner = n
6. El handoff de la etapa anterior, si existe: docs/producto/reporte_personalizado/etapa_<n-1>_handoff.md

NO commits. NO borres ficha_del_mirador, /data/reportes ni los endpoints /estaciones/<id>/reporte*. NO cambies la versión de PostgreSQL/timescaledb ni el volumen ./pgdata (la imagen es db/Dockerfile, con PostGIS). NO reactives el PDF por estación (REPORTE_PDF_HABILITADO) salvo que lo pida el operador. NO reescribas un reporte_solicitud en estado listo. NO envíes la geometría del cliente a servicios externos. NO --build salvo que cambie un Dockerfile. Este workspace ES producción (app.rodaja.cl): edita aquí y reinicia según .cursor/rules/servidor-prod.mdc.
```

Un solo agente a la vez. Si otro dejó la etapa `en_curso` y no hay handoff, no la reclames: averigua si sigue activo.

---

## Contrato del agente

1. **Reclamar** la etapa en la cola (`pendiente` → `en_curso`).
2. **Leer lo mínimo:** solo lo que tu etapa usa, según la §8 del plan maestro.
3. **La primera entrega es un plan, no un diff.** Escribe `docs/producto/reporte_personalizado/etapa_<n>_plan.md` a partir de `plan/_plantilla.md`.
4. **Revisar tu propio plan** contra la §4 (prohibiciones) y la §3 (decisiones) del maestro. Si falta una decisión: status `bloqueado`, handoff y **parar**.
5. **Implementar** solo lo que dice tu etapa.
6. **Verificar** con un comando o un gesto (curl, psql o navegador) y anotar el resultado.
7. **Cerrar:** actualizar los criterios (solo tus filas), escribir `etapa_<n>_handoff.md` a partir de `handoff/_plantilla.md` y marcar la etapa `hecho` o `hecho_con_gaps`.

### La etapa 6 (SEIA) se bloquea a propósito

Si falta un prerrequisito P0 de PREREQUISITOS_SEIA.md (normales en 3 períodos, sectores A-N, licencias o ET), la etapa queda `bloqueado` con la lista de lo que falta. No inventes ni aproximes parámetros del SEA.

### Checklist al cerrar la etapa

- [ ] La cola muestra la etapa en `hecho`, `hecho_con_gaps` o `bloqueado` (con el motivo).
- [ ] Existen el plan y el handoff.
- [ ] Ninguna fila `etapa_owner = n` sigue `pendiente`.
- [ ] Si cambió la api, el worker o nginx: se hizo el restart o el recreate (`--build` solo si cambió un Dockerfile).
- [ ] Si cambió JS estático: el `?v=` es nuevo y se revisaron las comillas (js-html-quotes.mdc).
