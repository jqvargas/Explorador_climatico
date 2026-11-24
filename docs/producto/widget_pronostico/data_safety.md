# Data safety y privacidad — widget de pronóstico

Lo marca una persona en Play Console, y pega el párrafo en la web **cuando** suba el AAB. Esta etapa no abre la consola y no edita `nginx/html/legal/privacidad/index.html`.

Decisiones cerradas en el plan maestro, sección 8. El id de estación no es un dato de ubicación. El fix grueso se guarda solo en el teléfono (`fix_lat` / `fix_lon` en el DataStore `pronostico_widget`) para no recalcular si el movimiento es menor a 25 km. Al servidor sale `GET /api/widget/estaciones/{id}/hoy`. Nada de latitud, longitud ni `/estaciones/cercanas`.

## Qué marcar

Si la ficha ya declara otros datos (cuenta, pagos), no los borres. Agrega esto por el widget.

| Pregunta | Respuesta |
|----------|-----------|
| ¿Hay datos de usuario nuevos por el widget? | Ubicación aproximada |
| Ubicación precisa | No |
| Ubicación aproximada: ¿se recopila? | Sí |
| ¿Se comparte con terceros? | No |
| ¿El usuario puede rechazarla? | Sí. Si la rechaza, el widget usa la estación por defecto |
| Finalidad | Funcionalidad de la app |
| ¿En segundo plano / con la app cerrada? | No. Solo en primer plano, con la app en uso |
| ¿La coordenada sale del dispositivo? | No |
| El id de estación | No lo declares como ubicación |

En el formulario de Play, «se recopila» a veces significa «se transmite fuera del teléfono». Aquí la app sí obtiene la coordenada y el plan pide declararla como recopilada y no compartida. Si el formulario no deja marcar las dos cosas a la vez, responde que **no se transmite**. No inventes un envío a `app.rodaja.cl` para que la casilla cuadre. Anótalo en la prueba y sigue sin marcar precisa ni segundo plano.

No cambies, por este widget, la respuesta global de cifrado en tránsito. El resto del tráfico de la app sigue como esté. La coordenada no viaja.

El worker, con la app cerrada, no lee el GPS. Pide el pronóstico del id que ya está guardado, cada unos 45 minutos, solo con red.

## Párrafo para la privacidad live

Pegarlo al publicar el AAB. Hasta entonces la página no se toca.

Hoy el ítem de ubicación (sección «Datos que recopilamos») dice que la aproximada se pide solo en el mapa del Explorador, que no se pide en Inicio (`/app/`) ni en Mi reporte, y que no hay rastreo con la app cerrada. Eso sigue siendo cierto para el sitio. No borres esa frase ni el resto del ítem (cámara, notificaciones, mapa). Agrega un párrafo **después** de ese ítem:

> **Ubicación aproximada en la app Android.** Al abrir por primera vez la app Android de rodaja podemos pedir, una vez y en primer plano, el permiso de ubicación aproximada para elegir la estación del pronóstico en la pantalla de inicio. Esa coordenada se guarda solo en el teléfono y no se envía al servidor: la app consulta el pronóstico con el identificador de la estación. No es un rastreo con la app cerrada. Con la app cerrada solo se actualiza el pronóstico de la estación ya elegida. Si no concedes el permiso, o si no hay una estación con pronóstico a menos de 200 km, se usa una estación por defecto. Puedes fijar otra en el widget. El mapa del Explorador es otra cosa: ahí, si activas el botón de ubicación, la sesión sí envía latitud y longitud para buscar estaciones cercanas.

Y, en «Finalidad y bases», un ítem más:

> Si lo pides en la app Android, una vez y en primer plano, elegir la estación del pronóstico de la pantalla de inicio. La coordenada no se envía al servidor.

No escribas que el widget rastrea con la app cerrada.

## Al armar el AAB (no es esta etapa)

- versionCode **13**, minSdk **24**. En el celular, «Poner en el inicio de pantalla» abre el aviso del sistema.
- `bubblewrap build`. Si regenera archivos, volver a pegar a mano los bloques `RODAJA_WIDGET_BEGIN` / `RODAJA_WIDGET_END` en `LauncherActivity.java`, `AndroidManifest.xml` y `app/build.gradle`.
- Internal testing en Play. Data safety como esta ficha. Privacidad live con el párrafo de arriba en el mismo momento, no antes: si la página cambia y el AAB no está, el texto promete un permiso que la app publicada todavía no pide.
