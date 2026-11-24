package cl.rodaja.app.widget

import android.Manifest
import android.annotation.SuppressLint
import android.content.Context
import android.content.pm.PackageManager
import androidx.core.content.ContextCompat
import com.google.android.gms.common.ConnectionResult
import com.google.android.gms.common.GoogleApiAvailability
import com.google.android.gms.location.LocationRequest
import com.google.android.gms.location.LocationServices
import com.google.android.gms.tasks.CancellationTokenSource

/**
 * Ubicación aproximada, solo en primer plano. Si no hay Play Services, el
 * llamador usa la estación por defecto. No hay LocationManager de respaldo.
 */
internal object LocationReader {
    fun playServices(context: Context): Boolean {
        val code = GoogleApiAvailability.getInstance().isGooglePlayServicesAvailable(context)
        return code == ConnectionResult.SUCCESS
    }

    fun coarseGranted(context: Context): Boolean {
        return ContextCompat.checkSelfPermission(
            context,
            Manifest.permission.ACCESS_COARSE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED
    }

    @SuppressLint("MissingPermission")
    fun read(context: Context, onDone: (Double?, Double?) -> Unit) {
        if (!playServices(context) || !coarseGranted(context)) {
            onDone(null, null)
            return
        }
        val client = LocationServices.getFusedLocationProviderClient(context)
        val token = CancellationTokenSource()
        try {
            @Suppress("DEPRECATION")
            client.getCurrentLocation(
                LocationRequest.PRIORITY_BALANCED_POWER_ACCURACY,
                token.token
            ).addOnCompleteListener { task ->
                val loc = if (task.isSuccessful) task.result else null
                if (loc != null) {
                    onDone(loc.latitude, loc.longitude)
                } else {
                    client.lastLocation.addOnCompleteListener { last ->
                        val older = if (last.isSuccessful) last.result else null
                        if (older != null) {
                            onDone(older.latitude, older.longitude)
                        } else {
                            onDone(null, null)
                        }
                    }
                }
            }
        } catch (e: SecurityException) {
            onDone(null, null)
        }
    }
}
