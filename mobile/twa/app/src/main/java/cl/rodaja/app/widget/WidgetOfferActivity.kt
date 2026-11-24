package cl.rodaja.app.widget

import android.app.Activity
import android.appwidget.AppWidgetManager
import android.content.ComponentName
import android.os.Build
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.TextView
import cl.rodaja.app.R

/**
 * intent://pin#Intent;scheme=rodaja;package=cl.rodaja.app;end
 * Con estación fija: S.e=1781. No lee ubicación.
 */
class WidgetOfferActivity : Activity() {
    private var pinAsked = false

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.widget_offer)
        findViewById<Button>(R.id.widget_offer_add).setOnClickListener { pin() }
        findViewById<Button>(R.id.widget_offer_close).setOnClickListener { finish() }
        val fixed = WidgetGates.stationExtra(intent.getStringExtra(WidgetGates.EXTRA_STATION))
        if (fixed != null) {
            findViewById<TextView>(R.id.widget_offer_body).setText(R.string.widget_offer_saved)
            PlaceUpdater.writeFixed(this, fixed) {
                runOnUiThread {
                    if (!isFinishing) {
                        pin()
                    }
                }
            }
        } else if (!pin()) {
            val already = widgetCount(this) > 0
            findViewById<TextView>(R.id.widget_offer_body).setText(
                if (already) R.string.widget_offer_already else R.string.widget_offer_manual
            )
            findViewById<Button>(R.id.widget_offer_add).visibility = View.GONE
        }
    }

    private fun pin(): Boolean {
        if (pinAsked || isFinishing) {
            return false
        }
        val mgr = AppWidgetManager.getInstance(this)
        val supported = Build.VERSION.SDK_INT >= 26 && mgr.isRequestPinAppWidgetSupported
        if (!WidgetGates.shouldOfferPin(Build.VERSION.SDK_INT, supported, widgetCount(this))) {
            return false
        }
        pinAsked = true
        return mgr.requestPinAppWidget(
            ComponentName(this, PronosticoWidgetReceiver::class.java),
            null,
            null
        )
    }
}
