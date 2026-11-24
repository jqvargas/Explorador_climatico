# Auditoria de cambios - Explorador Climatico

## Problema reportado
- App muy lenta, "cargando estaciones 1 de 5000"
- Timeouts al conectar
- Segundo panel que tarda mucho

## Causa raiz identificada

### Logica nueva introducida
Se anadieron dos modos al panel inferior:
1. **Modo serie** (1 estacion): Muestra grafico de serie temporal (comportamiento original)
2. **Modo tabla** (>1 estaciones): Muestra tabla con analisis por estacion

### El bug
En modo tabla, `tabla_estaciones` hacia **2 llamadas API por cada estacion**:
- `GET /estaciones/<id>/variables` 
- `GET /datos/resumen?estacion_id=X&variable_id=Y`

Sin filtros, `estaciones()` devuelve hasta **5000 estaciones** (API tiene LIMIT 5000).
Total: **~10,000 llamadas API** en secuencia -> timeouts, bloqueo, app inutilizable.

## Correcciones aplicadas

### 1. Limite de 100 estaciones (tabla_estaciones.R)
- `MAX_ESTACIONES_TABLA <- 100L`
- Si `nrow(estaciones()) > 100`: retorna mensaje pidiendo aplicar filtros
- Evita el bucle costoso cuando hay demasiadas estaciones

### 2. Guard de panel_modo (tabla_estaciones.R)
- `if (panel_modo() != "tabla") return(NULL)` al inicio de `tabla_data`
- Solo se calculan datos cuando el usuario realmente ve la tabla
- No ejecuta nada costoso al cargar la app ni en modo serie

### 3. Manejo de "too_many" en renderDataTable
- Cuando hay >100 estaciones, se muestra mensaje amigable
- No se intenta renderizar tabla con miles de filas

## Archivos modificados en esta correccion

| Archivo | Cambio |
|---------|--------|
| `modules/tabla_estaciones.R` | Guard panel_modo, limite 100, manejo too_many |
| `server.R` | Pasa `panel_modo` a tabla_estaciones_server |

## Flujo correcto esperado

1. Usuario carga app -> `panel_modo = "serie"` -> tabla_data retorna NULL de inmediato (no hace nada)
2. Usuario aplica operador + variable -> estaciones() se reduce (ej: 50 estaciones)
3. Usuario click "Ver datos" -> `panel_modo = "tabla"` -> tabla_data corre
4. Si n <= 100: hace hasta 200 llamadas API (aceptable)
5. Si n > 100: retorna mensaje "Aplica filtros para reducir..."

## Verificacion

Reiniciar contenedor para aplicar cambios:
```bash
docker compose restart shiny
```

La app debe cargar en pocos segundos. Al hacer "Ver datos" con muchas estaciones sin filtrar, se mostrara el mensaje en vez de colgarse.
