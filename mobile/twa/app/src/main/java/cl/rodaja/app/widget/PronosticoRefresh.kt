package cl.rodaja.app.widget

import android.content.Context
import android.net.ConnectivityManager
import android.net.NetworkCapabilities
import android.os.Build
import androidx.work.BackoffPolicy
import androidx.work.ExistingPeriodicWorkPolicy
import androidx.work.ExistingWorkPolicy
import androidx.work.OneTimeWorkRequestBuilder
import androidx.work.PeriodicWorkRequestBuilder
import androidx.work.WorkManager
import java.io.IOException
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

internal enum class Load { OK, RETRY, FAIL, SKIP }

internal object PronosticoRefresh {
    private const val PERIODIC = "rodaja_pronostico_periodic"
    private const val NOW = "rodaja_pronostico_now"
    private val busy = AtomicBoolean(false)
    @Volatile private var lastOk = 0L

    /**
     * GET directo. Sin constraint de red: CONNECTED espera NET_CAPABILITY_VALIDATED
     * y en algunos teléfonos eso no llega, así que la tarjeta se queda en Cargando.
     */
    suspend fun fetchNow(context: Context, force: Boolean = false): Load {
        val snap = WidgetStore.read(context)
        if (!force &&
            !snap.json.isNullOrEmpty() &&
            snap.state == WidgetFrame.CONTENT &&
            System.currentTimeMillis() - lastOk < 20_000L
        ) {
            return Load.SKIP
        }
        if (!busy.compareAndSet(false, true)) {
            return Load.SKIP
        }
        try {
            val (code, body) = fetchHoy(WidgetStore.stationId(context))
            if (code == 200) {
                WidgetStore.writeContent(context, WidgetFrame.cacheJson(body))
                lastOk = System.currentTimeMillis()
                return Load.OK
            }
            WidgetStore.writeState(context, WidgetFrame.ERROR)
            return if (code in 500..599) Load.RETRY else Load.FAIL
        } catch (e: IOException) {
            WidgetStore.writeState(context, WidgetFrame.OFFLINE)
            return Load.RETRY
        } catch (e: kotlinx.coroutines.CancellationException) {
            throw e
        } catch (e: RuntimeException) {
            WidgetStore.writeState(context, WidgetFrame.ERROR)
            return Load.FAIL
        } finally {
            busy.set(false)
        }
    }

    suspend fun prepare(context: Context) {
        val next = WidgetFrame.stateAfterSchedule(isOnline(context), WidgetStore.hasJson(context))
        val current = WidgetStore.read(context).state
        if (current == next) {
            return
        }
        WidgetStore.writeState(context, next)
    }

    fun enqueue(context: Context) {
        val wm = WorkManager.getInstance(context)
        val periodic = PeriodicWorkRequestBuilder<PronosticoWorker>(45, TimeUnit.MINUTES)
            .setBackoffCriteria(BackoffPolicy.EXPONENTIAL, 30, TimeUnit.SECONDS)
            .build()
        wm.enqueueUniquePeriodicWork(PERIODIC, ExistingPeriodicWorkPolicy.UPDATE, periodic)
        val now = OneTimeWorkRequestBuilder<PronosticoWorker>()
            .setBackoffCriteria(BackoffPolicy.EXPONENTIAL, 30, TimeUnit.SECONDS)
            .build()
        wm.enqueueUniqueWork(NOW, ExistingWorkPolicy.REPLACE, now)
    }

    fun cancel(context: Context) {
        val wm = WorkManager.getInstance(context)
        wm.cancelUniqueWork(PERIODIC)
        wm.cancelUniqueWork(NOW)
    }
}

@Suppress("DEPRECATION")
internal fun isOnline(context: Context): Boolean {
    val cm = context.getSystemService(Context.CONNECTIVITY_SERVICE) as ConnectivityManager
    if (Build.VERSION.SDK_INT >= 23) {
        val network = cm.activeNetwork ?: return false
        val caps = cm.getNetworkCapabilities(network) ?: return false
        return caps.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)
    }
    val info = cm.activeNetworkInfo ?: return false
    return info.isConnected
}
