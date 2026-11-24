package cl.rodaja.app.widget

import android.appwidget.AppWidgetManager
import android.content.Context
import androidx.glance.appwidget.GlanceAppWidget
import androidx.glance.appwidget.GlanceAppWidgetReceiver
import androidx.glance.appwidget.updateAll
import kotlin.concurrent.thread
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking

class PronosticoWidgetReceiver : GlanceAppWidgetReceiver() {
    override val glanceAppWidget: GlanceAppWidget = PronosticoWidget()

    override fun onEnabled(context: Context) {
        super.onEnabled(context)
        prepare(context)
        kick(context)
        PronosticoRefresh.enqueue(context)
    }

    override fun onUpdate(
        context: Context,
        appWidgetManager: AppWidgetManager,
        appWidgetIds: IntArray
    ) {
        prepare(context)
        kick(context)
        super.onUpdate(context, appWidgetManager, appWidgetIds)
        PronosticoRefresh.enqueue(context)
    }

    override fun onDisabled(context: Context) {
        PronosticoRefresh.cancel(context)
        super.onDisabled(context)
    }

    private fun kick(context: Context) {
        val app = context.applicationContext
        thread(name = "pronostico-hoy") {
            runBlocking {
                val load = PronosticoRefresh.fetchNow(app)
                if (load != Load.SKIP) {
                    PronosticoWidget().updateAll(app)
                }
            }
        }
    }

    private fun prepare(context: Context) {
        // GlanceAppWidgetReceiver ya usa goAsync en onUpdate. No volver a llamarlo.
        runBlocking(Dispatchers.IO) {
            PronosticoRefresh.prepare(context)
        }
    }
}
