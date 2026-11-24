/*
 * Copyright 2020 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package cl.rodaja.app;

import android.Manifest;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;

import cl.rodaja.app.widget.WidgetBridge;
import cl.rodaja.app.widget.WidgetGates;



public class LauncherActivity
        extends com.google.androidbrowserhelper.trusted.LauncherActivity {
    

    

    @Override
    protected boolean shouldLaunchImmediately() {
        // RODAJA_WIDGET_BEGIN
        // Abrir la app entra a la TWA. Sin pantalla de ubicación ni de pin.
        // RODAJA_WIDGET_END
        return super.shouldLaunchImmediately();
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // Setting an orientation crashes the app due to the transparent background on Android 8.0
        // Oreo and below. We only set the orientation on Oreo and above. This only affects the
        // splash screen and Chrome will still respect the orientation.
        // See https://github.com/GoogleChromeLabs/bubblewrap/issues/496 for details.
        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.O) {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);
        } else {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);
        }
        askNotificationOnFirstOpen();
    }

    private SharedPreferences bootPrefs() {
        return getSharedPreferences(WidgetGates.PREFS, MODE_PRIVATE);
    }

    private void askNotificationOnFirstOpen() {
        if (Build.VERSION.SDK_INT < 33) return;
        SharedPreferences prefs = bootPrefs();
        if (prefs.getBoolean("notifs_asked", false)) return;
        prefs.edit().putBoolean("notifs_asked", true).apply();
        if (checkSelfPermission(Manifest.permission.POST_NOTIFICATIONS)
                == PackageManager.PERMISSION_GRANTED) return;
        requestPermissions(new String[]{Manifest.permission.POST_NOTIFICATIONS}, 1001);
    }

    @Override
    protected Uri getLaunchingUrl() {
        Uri uri = super.getLaunchingUrl();
        Uri.Builder out = uri.buildUpon();
        // La web compara su estación con la del widget y, si difiere, abre rodaja://estacion.
        if (uri.getQueryParameter("rw") == null) {
            out.appendQueryParameter("rw",
                    String.valueOf(WidgetBridge.storedStationId(getApplicationContext())));
        }
        SharedPreferences prefs = bootPrefs();
        if (!prefs.getBoolean("boot_url_sent", false)) {
            prefs.edit().putBoolean("boot_url_sent", true).apply();
            out.appendQueryParameter("twa_boot", "1");
        }
        return out.build();
    }
}
