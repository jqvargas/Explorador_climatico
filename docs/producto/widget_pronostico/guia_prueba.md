# Guía de prueba — widget de pronóstico

Para el operador. El agente no instala, no firma y no publica. versionCode del AAB: **9**. minSdk **24** (Play, protección automática).

Paquete `cl.rodaja.app`. Icono «rodaja». En el cajón de widgets: **Pronóstico de hoy** (app «rodaja.cl»). Estación por defecto: **3939 Quinta Normal Santiago**, comuna Estación Central.

Imagen de emulador **con Google Play**, API **31** o más (el anclaje pide API 26; el color dinámico, API 31). Sin Play Services la app no lee ubicación: la tarjeta usa la 3939 y dice «Sin servicios de Google. Estación por defecto.»

## Preparar

En un emulador vacío. Si el teléfono ya tiene la app de Play, no la pises con este debug: la firma no coincide y hay que desinstalar. En un Samsung de uso diario, espera el AAB de internal testing.

```text
cd C:\Users\rodaja\Explorador_climatico\mobile\twa
.\gradlew.bat :app:assembleDebug --console=plain
adb install -r app\build\outputs\apk\debug\app-debug.apk
```

El SDK ya está en `mobile/twa/local.properties`. Repetir el primer arranque borra también el pronóstico guardado:

```text
adb shell pm clear cl.rodaja.app
adb shell am start -n cl.rodaja.app/.LauncherActivity
```

`pm clear` hace falta porque el aviso de primera vez se guarda al abrir el onboarding. Cerrar esa pantalla a medias igual entra a la TWA la vez siguiente.

`adb emu geo fix` recibe **longitud y después latitud**. Mándalo antes de abrir la app en el caso de permiso concedido. Estas coordenadas se quedan en el emulador. No van en ninguna URL.

| Lugar | Comando | Para qué |
|-------|---------|----------|
| Quinta Normal (3939, -33.445, -70.68278) | `adb emu geo fix -70.68278 -33.445` | Permiso concedido |
| Valparaíso (1098, -33.04139, -71.60444) | `adb emu geo fix -71.60444 -33.04139` | Cambio de ciudad, a menos de 200 km |
| Pacífico | `adb emu geo fix -85.0 -33.5` | A más de 200 km de cualquier estación del catálogo |

La primera vez, después del widget, la TWA puede pedir notificaciones. Eso ya existía. Aceptar o rechazar no cambia el pronóstico.

## 1. Permiso concedido

1. `pm clear`.
2. `adb emu geo fix -70.68278 -33.445`.
3. Abre rodaja.
4. Pantalla blanca: título «Pronóstico en tu pantalla de inicio» y el texto de ubicación aproximada, que no se envía al servidor. Botón **Continuar**.
5. El sistema pide ubicación **aproximada**. Concede.
6. El texto pasa a «Buscando la estación más cercana…».
7. Si el lanzador puede anclar (API 26 o más y todavía no hay widget), sale el diálogo del sistema. Si lo cierras, queda el botón **Abrir rodaja**. Úsalo. No va a volver a pedirlo al abrir la app.
8. Se abre la TWA.

Después agrega el widget **Pronóstico de hoy** (mantén pulsada la pantalla de inicio). En API 31 la configuración al soltarlo es opcional. Si se abre sola, Atrás deja el widget puesto.

**Esperado:** nombre Quinta Normal Santiago, comuna Estación Central, temperatura con coma y «°C» (o «—» si no hay hora), mín/máx, probabilidad y/o mm, hora `HH:mm`. Sin el cartel «Toca para activar ubicación».

La segunda apertura en frío entra directo a la TWA. No repite el onboarding.

## 2. Permiso denegado

1. `pm clear` y abre rodaja.
2. **Continuar** y rechaza la ubicación.
3. Sigue el anclaje o **Abrir rodaja**.
4. Coloca el widget si todavía no está.

**Esperado:** pronóstico de Quinta Normal Santiago y el cartel **«Toca para activar ubicación»**. Tocar el cartel vuelve a pedir el permiso. Tocar el resto de la tarjeta abre Mi reporte de la 3939. Cerrar y abrir rodaja no vuelve a mostrar el onboarding ni insiste con el permiso.

`adb shell pm grant cl.rodaja.app android.permission.ACCESS_COARSE_LOCATION` evita el diálogo, pero no elige estación. La elección ocurre en Continuar, en el cartel o en «Usar la más cercana».

## 3. Sin conexión

1. Con red, espera a ver números y una hora en la tarjeta.
2. Modo avión (panel del emulador, o `adb shell cmd connectivity airplane-mode enable`).
3. La tarjeta **sigue igual**. Esperar 45 minutos no pone el cartel: el trabajo periódico no arranca sin red.
4. Quita el widget y vuélvelo a poner, todavía sin red.

**Esperado:** los mismos números y la misma hora, más **«Sin conexión»**.

Vuelve la red (`airplane-mode disable`). El pedido que quedó encolado corre solo y el cartel sale. Si el teléfono nunca llegó a guardar un pronóstico, sin red solo se ve «Sin conexión», sin números.

El buscador de estaciones funciona igual sin red: lee el catálogo que vino en la app.

## 4. Cambio de ubicación

Abrir rodaja no lee un fix nuevo. El worker, con la app cerrada, pide el pronóstico del id ya guardado. Tampoco mira el GPS.

1. Deja el caso 1 terminado (Quinta Normal, permiso concedido).
2. `adb emu geo fix -71.60444 -33.04139`.
3. Abre rodaja. **La estación no cambia.** Eso es lo correcto.
4. Mantén pulsado el widget y abre su configuración. Botón **«Usar la más cercana»**.

**Esperado:** una estación de Valparaíso (el catálogo tiene «Valparaíso · Valparaíso», id 1098). Deja de ser Quinta Normal. Sigue sin el cartel de ubicación.

5. `adb emu geo fix -85.0 -33.5` y otra vez **«Usar la más cercana»**.

**Esperado:** otra vez Quinta Normal Santiago. El punto está a más de 200 km de todas las estaciones del catálogo.

Ese botón recalcula **siempre**, también si el movimiento es de 10 km. El umbral de 25 km no se demuestra con él. Ese umbral está en las pruebas JVM de la etapa 2 y solo corre en una lectura que ya tenga un fix guardado y que no sea este botón (el onboarding o el cartel). Con el permiso ya concedido el cartel no está. Un movimiento corto seguido solo de abrir la app conserva la estación porque nadie releyó la ubicación.

## 5. Estación fijada

En la configuración, busca `Punta Arenas` y toca la fila **Punta Arenas · Punta Arenas** (id 3428). El buscador dice «Buscar estación o comuna».

Luego `adb emu geo fix -70.68278 -33.445` y abre rodaja.

**Esperado:** sigue Punta Arenas. No pulses «Usar la más cercana»: eso vuelve al modo cercano.

Lo mismo desde fuera, con el id en el extra `e` (en el plan maestro el ejemplo era `S.e=1781`; el id real de esta prueba es uno que está en el catálogo):

```text
adb shell am start -a android.intent.action.VIEW -p cl.rodaja.app -d "intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;S.e=3428;end"
```

**Esperado:** texto «Estación fijada.» Si ya hay un widget, **Agregar** no abre otro anclaje. **Cerrar** vuelve. La tarjeta pasa a esa estación. Un `?e=` en una página web no hace esto.

Para ofrecer el anclaje otra vez, sin fijar estación y sin leer ubicación:

```text
adb shell am start -a android.intent.action.VIEW -p cl.rodaja.app -d "rodaja://pin"
```

Si ya hay una instancia, no insiste.

## 6. Modo oscuro

Con el widget en pantalla:

```text
adb shell cmd uimode night yes
```

**Esperado:** la tarjeta sigue el tema (en API 31, colores dinámicos). El onboarding y la configuración siguen en blanco con texto negro. Es el tema de esas dos pantallas. No es un fallo del widget.

```text
adb shell cmd uimode night no
```

## 7. El tap abre la TWA

Toca la tarjeta (no el cartel de ubicación, si está).

**Esperado:** se abre rodaja, no Chrome como otra app. La página es Mi reporte, vista pronóstico, de la estación de la tarjeta. La URL es `https://app.rodaja.cl/mi-reporte/?view=pronostico&e={id}` y no lleva `lat`, `lon` ni `cambiar`. El selector de estación no aparece.

En este APK de debug puede verse la barra de Chrome **dentro** de rodaja: la firma de debug no es la de Play y Digital Asset Links no verifica. Anótalo. No edites `assetlinks.json`. El AAB firmado es el que tiene que ir sin barra.

## 8. Samsung con One UI

Los mismos siete casos, en un teléfono de prueba. Anota además:

- Cómo se ve el diálogo de anclar (One UI, no el del emulador).
- Si al soltar el widget se abre la configuración, Atrás no borra el widget.
- Modo oscuro del sistema: la tarjeta cambia; onboarding y configuración siguen claros.
- El permiso que ofrece el sistema es aproximado, mientras se usa. No actives ubicación precisa ni «todo el tiempo». La app no los pide.
- Con la app cerrada el widget se actualiza solo (unos 45 minutos, con red). Eso es el pronóstico del id guardado, no un rastreo.

## Qué anotar

Por cada caso: pasó o falló, y una captura si falló. Las casillas de esta guía las marca la persona que prueba. El handoff de la etapa 5 las deja abiertas.

## Fuera de esta prueba

No publiques el AAB. No corras `bubblewrap update` ni `bubblewrap init`. No subas `minSdk`. No toques el splash, `start_url` ni `assetlinks.json`. No mandes latitud ni longitud a `app.rodaja.cl`. El único GET del widget es `https://app.rodaja.cl/api/widget/estaciones/{id}/hoy`.
