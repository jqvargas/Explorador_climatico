package cl.rodaja.app.widget

import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.net.Uri
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.glance.GlanceId
import androidx.glance.GlanceTheme
import androidx.glance.appwidget.GlanceAppWidget
import androidx.glance.appwidget.SizeMode
import androidx.glance.appwidget.action.actionRunCallback
import androidx.glance.appwidget.action.actionStartActivity
import androidx.glance.appwidget.provideContent
import cl.rodaja.app.LauncherActivity
import kotlinx.coroutines.flow.first

/**
 * 2×2 (110 dp), 4×1 (250×110) y 4×2 (250×180). El fondo lo pinta la tarjeta;
 * GlanceTheme queda solo por el contrato de provideContent.
 */
class PronosticoWidget : GlanceAppWidget() {
    override val sizeMode: SizeMode = SizeMode.Responsive(
        setOf(DpSize(110.dp, 110.dp), DpSize(250.dp, 110.dp), DpSize(250.dp, 180.dp))
    )

    override suspend fun provideGlance(context: Context, id: GlanceId) {
        val initial = context.pronosticoStore.data.first()
        val snap0 = initial.widgetSnapshot()
        if (snap0.json.isNullOrEmpty() || snap0.syncedAt == 0L) {
            PronosticoRefresh.fetchNow(context)
        }
        provideContent {
            val prefs by context.pronosticoStore.data.collectAsState(initial)
            val snap = prefs.widgetSnapshot()
            val frame = WidgetFrame.frame(snap.state, snap.json)
            val tapId = when {
                frame.stationId > 0L -> frame.stationId
                snap.stationId > 0L -> snap.stationId
                else -> 0L
            }
            val open = if (tapId > 0L) actionStartActivity(reportIntent(context, tapId)) else null
            val location = actionStartActivity(locationIntent(context))
            val refresh = actionRunCallback<WidgetRefreshAction>()
            GlanceTheme {
                PronosticoCard(frame, snap.notice, open, location, refresh, snap.syncedAt)
            }
        }
    }
}

/** Mismo patrón que shortcuts.xml: MAIN explícito a LauncherActivity, no ACTION_VIEW. */
internal fun reportIntent(context: Context, stationId: Long): Intent {
    return Intent(Intent.ACTION_MAIN).apply {
        component = ComponentName(context, LauncherActivity::class.java)
        data = Uri.parse(WidgetFrame.reportUrl(stationId))
    }
}

private fun locationIntent(context: Context): Intent {
    return Intent(context, OnboardingActivity::class.java).apply {
        putExtra(WidgetGates.EXTRA_ONLY_LOCATION, true)
        flags = Intent.FLAG_ACTIVITY_NEW_TASK
    }
}
