# Plan de implementación — etapa N `<id>`

**No ejecutar código de producto hasta que este archivo exista y pase el self-check de abajo.**
Contrato: `docs/producto/widget_pronostico_PROMPT_FIJO.md`. Master: `docs/producto/PLAN_WIDGET_PRONOSTICO.md`.

## 1. Alcance de esta etapa (una frase)

## 2. Auditoría: qué se leyó

Solo la lectura mínima del master. No reauditar el repo.

| Ruta | Qué se confirmó |
|------|-----------------|
| | |

## 3. Estado actual (evidencia)

Citas a código / curl / handoff N-1. No copiar el master a ciegas.

## 4. Cambios mínimos

| Cambio | Archivo | Validación | Caso borde |
|--------|---------|------------|------------|
| | | | |

## 5. Tareas HUMANAS

Si ninguna: “ninguna”.

## 6. Dependencias

- Handoff anterior:
- Restart api (`scripts/dc.ps1 restart api`) solo si esta etapa toca la API:
- assembleDebug:

## 7. Orden de implementación

1. …

## 8. Verificación

Comando + resultado esperado.

## 9. Fuera de alcance

## 10. Riesgos

## 11. Self-check

- [ ] Filas `etapa_owner = N` de `criterios.md` cubiertas
- [ ] Prohibidos globales del prompt fijo respetados
- [ ] Humanos no fingidos (Play, keystore, teléfono, AAB)
- [ ] No se sube minSdk, no se toca assetlinks ni el splash
- [ ] Lat/lon no se envían al servidor
