/*
 * Copyright 2011 Giles Malet.
 *
 * This file is part of GRTransit.
 * 
 * GRTransit is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * GRTransit is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GRTransit.  If not, see <http://www.gnu.org/licenses/>.
 */

package net.kw.shrdlu.grtgtfs;

import android.content.Context;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.Signature;
import android.os.StrictMode;
import android.util.Log;

import com.google.android.gms.analytics.GoogleAnalytics;
import com.google.android.gms.analytics.HitBuilders;
import com.google.android.gms.analytics.Tracker;

public class GRTApplication extends android.app.Application {
	public static final String TAG = "GRTApplication";

	public static Tracker tracker = null;
	public static Preferences mPreferences = null;
	public static DatabaseHelper dbHelper = null;
	public static boolean isDebugBuild = false;

	private Context mContext;

	// Define the debug signature hash (Android default debug cert). Code from sigs[i].hashCode()
	protected final static int DEBUG_SIGNATURE_HASH = -1270195494;

	@Override
	public void onCreate() {
		super.onCreate();
		// Log.d(TAG, "onCreate");

		mContext = this; // also returned by getApplicationContext();

		isDebugBuild = CheckDebugBuild();

		// Do this before instantiating Globals, as that may do something we'd like
		// to see by having StrictMode on already.
		if (isDebugBuild) {
            StrictMode.setThreadPolicy(new StrictMode.ThreadPolicy.Builder()
                    .detectNetwork()
                    .detectDiskReads()
                    .build());
            // .detectDiskReads() -- too noisy
            // .detectDiskWrites() -- too noisy
            StrictMode.setVmPolicy(new StrictMode.VmPolicy.Builder()
                    .detectLeakedSqlLiteObjects()
                    .detectLeakedClosableObjects()
                    .detectActivityLeaks()
                    .penaltyLog().penaltyDeath().build());
		}

		mPreferences = new Preferences(mContext);
		dbHelper = new DatabaseHelper(mContext);

        GoogleAnalytics analytics = GoogleAnalytics.getInstance(mContext);
        analytics.enableAutoActivityReports(this);
		tracker = analytics.newTracker(getString(R.string.ga_api_key));
        tracker.enableAutoActivityTracking(true);   // need this here and in the parent?
        tracker.setClientId(GRTApplication.mPreferences.getUUID());
		//if (isDebugBuild) {
		//	analytics.setDryRun(true);
		//} else {
		//	analytics.setDryRun(false);
		//}

        // Report whether it's a debug build (not reported if DryRun is set).
		try {
			String v = getPackageManager().getPackageInfo(getPackageName(), 0).versionName;
			if (isDebugBuild) {
				v += " debug";
			}
            tracker.send(new HitBuilders.EventBuilder()
                    .setCategory("Startup")
                    .setAction(getString(R.string.app_short_name))
                    .setLabel(v)
                    .build());
		} catch (final NameNotFoundException e) {
			Log.e(TAG, "Exception when getting versionName");
			e.printStackTrace();
		}
	}

	// Checks if this apk was built using the debug certificate
	// See http://stackoverflow.com/questions/3029819/android-automatically-choose-debug-release-maps-api-key/3828864#3828864
	private static boolean checkedBuild = false;

	private boolean CheckDebugBuild() {
		if (!checkedBuild) {
			try {
				final Signature[] sigs = getPackageManager().getPackageInfo(getPackageName(), PackageManager.GET_SIGNATURES).signatures;
				for (final Signature sig : sigs) {
					if (sig.hashCode() == DEBUG_SIGNATURE_HASH) {
						Log.d(TAG, "This is a debug build!");
						isDebugBuild = true;
						break;
					}
				}
			} catch (final NameNotFoundException e) {
				e.printStackTrace();
			}
			checkedBuild = true;
		}
		return isDebugBuild;
	}
}
