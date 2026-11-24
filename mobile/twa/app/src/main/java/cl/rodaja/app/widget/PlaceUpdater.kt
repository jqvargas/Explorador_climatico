package cl.rodaja.app.widget

import android.appwidget.AppWidgetManager
import android.content.ComponentName
import android.content.Context
import kotlinx.coroutines.runBlocking

internal object CatalogAsset {
    /** ponytail: lee el asset entero (~300 KB) en cada elección. Si pasa de un par de MB, parsear en streaming. */
    fun load(context: Context): CatalogParser.Catalog {
        val json = context.assets.open("estaciones_pronostico.json")
            .bufferedReader(Charsets.UTF_8)
            .use { it.readText() }
        return CatalogParser.parse(json)
    }
}

internal fun widgetCount(context: Context): Int {
    val mgr = AppWidgetManager.getInstance(context)
    val cn = ComponentName(context, PronosticoWidgetReceiver::class.java)
    return mgr.getAppWidgetIds(cn).size
}

internal object PlaceUpdater {
    fun apply(
        context: Context,
        playServices: Boolean,
        permissionGranted: Boolean,
        lat: Double?,
        lon: Double?,
        forceNearest: Boolean,
        fixedId: Long?,
        onDone: () -> Unit
    ) {
        val app = context.applicationContext
        Thread {
            try {
                runBlocking {
                    write(app, playServices, permissionGranted, lat, lon, forceNearest, fixedId)
                }
                if (widgetCount(app) > 0) {
                    PronosticoRefresh.enqueue(app)
                }
            } finally {
                onDone()
            }
        }.start()
    }

    fun writeFixed(context: Context, stationId: Long, onDone: () -> Unit) {
        apply(context, true, true, null, null, false, stationId, onDone)
    }

    private suspend fun write(
        context: Context,
        playServices: Boolean,
        permissionGranted: Boolean,
        lat: Double?,
        lon: Double?,
        forceNearest: Boolean,
        fixedId: Long?
    ) {
        val catalog = try {
            CatalogAsset.load(context)
        } catch (e: RuntimeException) {
            if (fixedId != null) {
                WidgetStore.writeChoice(
                    context,
                    StationSelector.MODE_FIXED,
                    fixedId,
                    null,
                    null,
                    null,
                    false
                )
            }
            return
        }
        if (fixedId != null && catalog.stations.none { it.id == fixedId }) {
            return
        }
        val mem = WidgetStore.memory(context)
        val mode = when {
            fixedId != null -> StationSelector.MODE_FIXED
            forceNearest -> StationSelector.MODE_NEAREST
            else -> mem.mode
        }
        val decision = WidgetPlacement.decide(
            mode,
            fixedId ?: mem.stationId,
            if (forceNearest) null else mem.lat,
            if (forceNearest) null else mem.lon,
            playServices,
            permissionGranted,
            lat,
            lon,
            CatalogParser.points(catalog.stations),
            catalog.defaultStationId
        )
        WidgetStore.writeChoice(
            context,
            mode,
            decision.stationId,
            lat,
            lon,
            decision.notice,
            decision.replaceFix
        )
    }
}
