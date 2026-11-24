# Convención CSS — dónde poner cada estilo

## Regla única

| ¿Qué estilizas? | Dónde | ¿!important? |
|-----------------|-------|--------------|
| Componentes propios (mapa, panel de datos, edge bar, drawer, layout) | rodaja.css | No |
| Componentes que Shiny/Bootstrap generan (selectInput, actionButton, formularios) | tags$style(HTML(...)) en ui_components.R | Sí |

## En la práctica

- **¿Es un elemento que TÚ creaste?** → rodaja.css, sin !important
- **¿Es un elemento que SHINY genera automáticamente?** → tags$style en ui_components.R, con !important

## Estructura mental

```
rodaja.css              → diseño y layout de TU app
tags$style en ui.R      → overrides de componentes de Shiny
```

Dos lugares, dos responsabilidades, sin mezclar.

## Lo que nunca debes hacer

- Agregar !important en rodaja.css para combatir Bootstrap
- Poner estilos de layout en tags$style
- Duplicar el mismo selector en ambos lugares
- Agregar parches al final de rodaja.css cuando algo no funciona

## Cuando algo no funciona

Primera pregunta: es un componente mio o de Shiny?
Eso indica donde poner el fix.
