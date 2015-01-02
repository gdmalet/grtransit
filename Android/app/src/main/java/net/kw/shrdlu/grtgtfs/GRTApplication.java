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
import android.os.StrictMode;

import com.google.android.gms.analytics.GoogleAnalytics;
import com.google.android.gms.analytics.HitBuilders;
import com.google.android.gms.analytics.Tracker;

public class GRTApplication extends android.app.Application {
	public static final String TAG = "GRTApplication";

	public static Tracker tracker = null;
	public static Preferences mPreferences = null;
	public static DatabaseHelper dbHelper = null;
    public final static boolean isDebugBuild = BuildConfig.DEBUG;

	private Context mContext;

	@Override
	public void onCreate() {
		super.onCreate();
		// Log.d(TAG, "onCreate");

		mContext = this; // also returned by getApplicationContext();

		// Do this before instantiating Globals, as that may do something we'd like
		// to see by having StrictMode on already.
		if (isDebugBuild) {
            StrictMode.setThreadPolicy(new StrictMode.ThreadPolicy.Builder()
                    .detectNetwork()
                    .build());
            // .detectDiskReads() -- too noisy
            // .detectDiskWrites() -- too noisy
//            StrictMode.setVmPolicy(new StrictMode.VmPolicy.Builder()
//                    .detectLeakedSqlLiteObjects()
//                    .detectActivityLeaks()
//                    .penaltyLog().penaltyDeath().build());
                    //.detectLeakedClosableObjects()
		}

		mPreferences = new Preferences(mContext);
		dbHelper = new DatabaseHelper(mContext);

        GoogleAnalytics analytics = GoogleAnalytics.getInstance(mContext);
        analytics.enableAutoActivityReports(this);
		tracker = analytics.newTracker(getString(R.string.ga_api_key));
        tracker.enableAutoActivityTracking(true);   // need this here and in the parent?
        tracker.setClientId(GRTApplication.mPreferences.getUUID());
		if (isDebugBuild) {
			analytics.setDryRun(true);
		} else {
			analytics.setDryRun(false);
		}

        // Report version details (not reported if analytics DryRun is set).
        String v = "Version " + BuildConfig.VERSION_NAME + " " + BuildConfig.BUILD_TYPE;
        v += ", Db " + dbHelper.GetDBVersion();
        tracker.send(new HitBuilders.EventBuilder()
                .setCategory("Startup")
                .setAction(GRTApplication.mPreferences.getUUID())
                .setLabel(v)
                .build());
	}
}
