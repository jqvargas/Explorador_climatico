package cl.rodaja.app.widget

import android.content.Context
import androidx.datastore.preferences.core.Preferences
import androidx.datastore.preferences.core.doublePreferencesKey
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.longPreferencesKey
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.preferencesDataStore
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking

internal val Context.pronosticoStore by preferencesDataStore(name = "pronostico_widget")

private val stateKey = stringPreferencesKey("state")
private val jsonKey = stringPreferencesKey("json")
private val stationKey = longPreferencesKey("station_id")
private val modeKey = stringPreferencesKey("mode")
private val latKey = doublePreferencesKey("fix_lat")
private val lonKey = doublePreferencesKey("fix_lon")
private val noticeKey = stringPreferencesKey("notice")
private val syncedKey = longPreferencesKey("synced_at")

internal data class WidgetSnap(
    val state: String,
    val json: String?,
    val notice: String?,
    val stationId: Long,
    val syncedAt: Long
)

/** Modo y último fix. Las coordenadas no entran en el JSON ni en ninguna URL. */
internal data class PlaceMemory(
    val mode: String,
    val stationId: Long?,
    val lat: Double?,
    val lon: Double?
)

internal fun Preferences.widgetSnapshot(): WidgetSnap {
    val saved = this[stationKey]
    return WidgetSnap(
        this[stateKey] ?: WidgetFrame.LOADING,
        this[jsonKey],
        this[noticeKey],
        if (saved != null && saved > 0L) saved else 0L,
        this[syncedKey] ?: 0L
    )
}

internal object WidgetStore {
    suspend fun read(context: Context): WidgetSnap {
        return context.pronosticoStore.data.first().widgetSnapshot()
    }

    suspend fun hasJson(context: Context): Boolean {
        return !context.pronosticoStore.data.first()[jsonKey].isNullOrEmpty()
    }

    suspend fun memory(context: Context): PlaceMemory {
        val prefs = context.pronosticoStore.data.first()
        val id = prefs[stationKey]
        val mode = if (prefs[modeKey] == StationSelector.MODE_FIXED) {
            StationSelector.MODE_FIXED
        } else {
            StationSelector.MODE_NEAREST
        }
        return PlaceMemory(
            mode,
            if (id != null && id > 0L) id else null,
            finite(prefs[latKey]),
            finite(prefs[lonKey])
        )
    }

    suspend fun stationId(context: Context): Long {
        val saved = context.pronosticoStore.data.first()[stationKey]
        if (saved != null && saved > 0L) {
            return saved
        }
        return defaultFromAsset(context)
    }

    suspend fun writeChoice(
        context: Context,
        mode: String,
        stationId: Long,
        lat: Double?,
        lon: Double?,
        notice: String?,
        replaceFix: Boolean
    ) {
        context.pronosticoStore.edit { prefs ->
            prefs[modeKey] = mode
            prefs[stationKey] = stationId
            if (replaceFix && finite(lat) != null && finite(lon) != null) {
                prefs[latKey] = lat as Double
                prefs[lonKey] = lon as Double
            }
            if (notice.isNullOrEmpty()) {
                prefs.remove(noticeKey)
            } else {
                prefs[noticeKey] = notice
            }
            val cached = prefs[jsonKey]
            if (!cached.isNullOrEmpty() && cachedStation(cached) != stationId) {
                prefs.remove(jsonKey)
                prefs[stateKey] = WidgetFrame.LOADING
            }
        }
    }

    suspend fun writeState(context: Context, state: String) {
        context.pronosticoStore.edit { it[stateKey] = state }
    }

    /** No toca mode, notice ni el fix. El worker no puede borrar el aviso de ubicación. */
    suspend fun writeContent(context: Context, json: String) {
        context.pronosticoStore.edit {
            it[stateKey] = WidgetFrame.CONTENT
            it[jsonKey] = json
            it[syncedKey] = System.currentTimeMillis()
        }
    }

    private fun defaultFromAsset(context: Context): Long {
        val head = context.assets.open("estaciones_pronostico.json").use { input ->
            val buf = ByteArray(256)
            val n = input.read(buf)
            if (n <= 0) "" else String(buf, 0, n, Charsets.UTF_8)
        }
        return WidgetFrame.defaultStationId(head)
    }
}

/** Para LauncherActivity (Java): id que tiene el widget, 0 si ninguno. */
object WidgetBridge {
    @JvmStatic
    fun storedStationId(context: Context): Long {
        return try {
            runBlocking {
                val saved = context.pronosticoStore.data.first()[stationKey]
                if (saved != null && saved > 0L) saved else 0L
            }
        } catch (e: Exception) {
            0L
        }
    }
}

private fun finite(v: Double?): Double? {
    if (v == null || v.isNaN() || v.isInfinite()) {
        return null
    }
    return v
}

private fun cachedStation(json: String): Long {
    return try {
        HoyParser.parse(json).stationId
    } catch (e: RuntimeException) {
        0L
    }
}
