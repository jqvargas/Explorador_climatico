package cl.rodaja.app.widget

import android.app.Activity
import android.appwidget.AppWidgetManager
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.text.Editable
import android.text.TextWatcher
import android.widget.ArrayAdapter
import android.widget.Button
import android.widget.EditText
import android.widget.ListView
import androidx.core.app.ActivityCompat
import cl.rodaja.app.R

/** Más cercana, o una estación del catálogo que ya está en el teléfono. */
class WidgetConfigureActivity : Activity() {
    private var widgetId = AppWidgetManager.INVALID_APPWIDGET_ID
    private var catalog: List<CatalogParser.Station> = emptyList()
    private var shown: List<CatalogParser.Station> = emptyList()
    private var busy = false
    private lateinit var list: ListView

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        widgetId = intent.getIntExtra(
            AppWidgetManager.EXTRA_APPWIDGET_ID,
            AppWidgetManager.INVALID_APPWIDGET_ID
        )
        if (widgetId == AppWidgetManager.INVALID_APPWIDGET_ID) {
            finish()
            return
        }
        setResult(RESULT_OK, resultIntent())
        setContentView(R.layout.widget_configure)
        list = findViewById(R.id.widget_configure_list)
        findViewById<Button>(R.id.widget_configure_nearest).setOnClickListener { onNearest() }
        findViewById<EditText>(R.id.widget_configure_search).addTextChangedListener(
            object : TextWatcher {
                override fun beforeTextChanged(s: CharSequence?, start: Int, count: Int, after: Int) {}
                override fun onTextChanged(s: CharSequence?, start: Int, before: Int, count: Int) {}
                override fun afterTextChanged(s: Editable?) {
                    bind(s?.toString())
                }
            }
        )
        list.setOnItemClickListener { _, _, position, _ ->
            val station = shown.getOrNull(position) ?: return@setOnItemClickListener
            if (busy) {
                return@setOnItemClickListener
            }
            busy = true
            PlaceUpdater.writeFixed(this, station.id) { done() }
        }
        Thread {
            val parsed = try {
                CatalogAsset.load(this)
            } catch (e: RuntimeException) {
                null
            }
            runOnUiThread {
                if (isFinishing || parsed == null) {
                    return@runOnUiThread
                }
                catalog = parsed.stations
                bind("")
            }
        }.start()
    }

    @Deprecated("API 33")
    override fun onBackPressed() {
        setResult(RESULT_OK, resultIntent())
        super.onBackPressed()
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        if (requestCode == REQ) {
            busy = false
            resolveNearest()
        }
    }

    private fun onNearest() {
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
        resolveNearest()
    }

    private fun resolveNearest() {
        if (busy) {
            return
        }
        busy = true
        val play = LocationReader.playServices(this)
        val granted = LocationReader.coarseGranted(this)
        if (!play || !granted) {
            PlaceUpdater.apply(this, play, granted, null, null, true, null) { done() }
            return
        }
        LocationReader.read(this) { lat, lon ->
            PlaceUpdater.apply(this, true, true, lat, lon, true, null) { done() }
        }
    }

    private fun done() {
        runOnUiThread {
            if (!isFinishing) {
                setResult(RESULT_OK, resultIntent())
                finish()
            }
        }
    }

    private fun bind(query: String?) {
        shown = CatalogParser.filter(catalog, query)
        val lines = ArrayList<String>(shown.size)
        for (i in shown.indices) {
            val s = shown[i]
            lines.add(if (s.comuna.isEmpty()) s.name else s.name + " · " + s.comuna)
        }
        list.adapter = ArrayAdapter(this, android.R.layout.simple_list_item_1, lines)
    }

    private fun resultIntent(): Intent {
        return Intent().putExtra(AppWidgetManager.EXTRA_APPWIDGET_ID, widgetId)
    }

    companion object {
        private const val REQ = 42
    }
}
