package cl.rodaja.app.widget

import android.content.Context
import androidx.glance.appwidget.updateAll
import androidx.work.CoroutineWorker
import androidx.work.WorkerParameters
import java.io.IOException
import java.net.HttpURLConnection
import java.net.URL
import kotlinx.coroutines.CancellationException

/** GET del pronóstico de hoy. Sin lat/lon. 4xx no reintenta; la red caída sí. */
class PronosticoWorker(
    context: Context,
    params: WorkerParameters
) : CoroutineWorker(context, params) {

    override suspend fun doWork(): Result {
        val load = try {
            PronosticoRefresh.fetchNow(applicationContext)
        } catch (e: CancellationException) {
            throw e
        }
        if (load != Load.SKIP) {
            PronosticoWidget().updateAll(applicationContext)
        }
        return when (load) {
            Load.RETRY -> Result.retry()
            Load.FAIL -> Result.failure()
            else -> Result.success()
        }
    }
}

internal fun fetchHoy(id: Long): Pair<Int, String> {
    val conn = (URL(WidgetFrame.hoyUrl(id)).openConnection() as HttpURLConnection)
    conn.connectTimeout = 8_000
    conn.readTimeout = 8_000
    conn.instanceFollowRedirects = false
    conn.requestMethod = "GET"
    conn.setRequestProperty("Accept", "application/json")
    conn.setRequestProperty("User-Agent", "rodaja-widget")
    try {
        val code = conn.responseCode
        val stream = if (code in 200..299) conn.inputStream else conn.errorStream
        val body = readLimited(stream)
        return code to body
    } finally {
        conn.disconnect()
    }
}

private fun readLimited(stream: java.io.InputStream?): String {
    if (stream == null) {
        return ""
    }
    return stream.bufferedReader(Charsets.UTF_8).use { reader ->
        val buf = CharArray(4096)
        val out = StringBuilder()
        while (true) {
            val n = reader.read(buf)
            if (n < 0) {
                break
            }
            if (out.length + n > 65536) {
                throw IOException("respuesta grande")
            }
            out.append(buf, 0, n)
        }
        out.toString()
    }
}
