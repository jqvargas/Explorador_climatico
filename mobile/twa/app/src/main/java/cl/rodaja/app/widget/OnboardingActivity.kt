package cl.rodaja.app.widget

import android.app.Activity
import android.appwidget.AppWidgetManager
import android.content.ComponentName
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.TextView
import androidx.core.app.ActivityCompat
import cl.rodaja.app.LauncherActivity
import cl.rodaja.app.R

/** Primera vez: explicación, permiso aproximado, pin si se puede, y la TWA. */
class OnboardingActivity : Activity() {
    private var launchData: String? = null
    private var onlyLocation = false
    private var busy = false
    private var pinAsked = false

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        readIntent(intent)
        if (!onlyLocation) {
            markSeen()
        }
        try {
            setContentView(R.layout.widget_onboarding)
            findViewById<Button>(R.id.widget_onboarding_continue).setOnClickListener { onContinue() }
            findViewById<Button>(R.id.widget_onboarding_open).setOnClickListener { leave() }
            if (onlyLocation) {
                onContinue()
            }
        } catch (e: RuntimeException) {
            leave()
        }
    }

    override fun onNewIntent(intent: Intent) {
        super.onNewIntent(intent)
        setIntent(intent)
        val data = intent.getStringExtra(WidgetGates.EXTRA_LAUNCH_DATA)
        if (!data.isNullOrEmpty()) {
            launchData = data
        }
    }

    @Deprecated("API 33")
    override fun onBackPressed() {
        leave()
    }

    private fun readIntent(intent: Intent?) {
        launchData = intent?.getStringExtra(WidgetGates.EXTRA_LAUNCH_DATA)
        onlyLocation = intent?.getBooleanExtra(WidgetGates.EXTRA_ONLY_LOCATION, false) == true
    }

    private fun onContinue() {
        if (busy) {
            return
        }
        if (Build.VERSION.SDK_INT >= 23 && !LocationReader.coarseGranted(this)) {
            busy = true
            ActivityCompat.requestPermissions(
                this,
                arrayOf(android.Manifest.permission.ACCESS_COARSE_LOCATION),
                REQ
            )
            return
        }
        resolve()
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        if (requestCode != REQ) {
            return
        }
        busy = false
        resolve()
    }

    private fun resolve() {
        if (busy) {
            return
        }
        busy = true
        val body = findViewById<TextView>(R.id.widget_onboarding_body)
        body?.setText(R.string.widget_onboarding_searching)
        val play = LocationReader.playServices(this)
        val granted = LocationReader.coarseGranted(this)
        if (!play || !granted) {
            PlaceUpdater.apply(this, play, granted, null, null, false, null) { afterPlace() }
            return
        }
        LocationReader.read(this) { lat, lon ->
            PlaceUpdater.apply(this, true, true, lat, lon, false, null) { afterPlace() }
        }
    }

    private fun afterPlace() {
        runOnUiThread {
            if (isFinishing) {
                return@runOnUiThread
            }
            if (onlyLocation) {
                finish()
                return@runOnUiThread
            }
            offerPinOrLeave()
        }
    }

    private fun offerPinOrLeave() {
        val mgr = AppWidgetManager.getInstance(this)
        val supported = Build.VERSION.SDK_INT >= 26 && mgr.isRequestPinAppWidgetSupported
        if (!pinAsked && WidgetGates.shouldOfferPin(Build.VERSION.SDK_INT, supported, widgetCount(this))) {
            pinAsked = true
            findViewById<Button>(R.id.widget_onboarding_open).visibility = View.VISIBLE
            findViewById<Button>(R.id.widget_onboarding_continue).visibility = View.GONE
            val asked = mgr.requestPinAppWidget(
                ComponentName(this, PronosticoWidgetReceiver::class.java),
                null,
                null
            )
            if (!asked) {
                leave()
            }
            return
        }
        leave()
    }

    private fun leave() {
        if (onlyLocation) {
            if (!isFinishing) {
                finish()
            }
            return
        }
        openTwa()
    }

    private fun openTwa() {
        markSeen()
        if (isFinishing) {
            return
        }
        startActivity(twaIntent())
        finish()
    }

    private fun twaIntent(): Intent {
        val i = Intent(this, LauncherActivity::class.java)
        i.putExtra(WidgetGates.EXTRA_OFFER_SEEN, true)
        val url = if (launchData.isNullOrEmpty()) getString(R.string.launchUrl) else launchData
        i.data = Uri.parse(url)
        i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TOP)
        return i
    }

    private fun markSeen() {
        getSharedPreferences(WidgetGates.PREFS, MODE_PRIVATE)
            .edit()
            .putBoolean(WidgetGates.PREF_OFFER_SEEN, true)
            .commit()
    }

    companion object {
        private const val REQ = 41
    }
}
