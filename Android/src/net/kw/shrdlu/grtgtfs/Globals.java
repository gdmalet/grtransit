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

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.pm.Signature;
import android.util.Log;
import android.view.View;
import android.widget.TextView;

import com.google.android.apps.analytics.GoogleAnalyticsTracker;

public class Globals {
	public static final String TAG = "Globals";
	public static GoogleAnalyticsTracker tracker = null;
	public static Preferences mPreferences = null;
	public static DatabaseHelper dbHelper = null;
	public static boolean isDebugBuild = false;

	// Define the debug signature hash (Android default debug cert). Code from sigs[i].hashCode()
	protected final static int DEBUG_SIGNATURE_HASH = -1270195494;

	// Used by tracker.
	private static final int TRACKER_VISITOR_SCOPE = 1;
	// private static final int TRACKER_SESSION_SCOPE = 2;
	// private static final int TRACKER_PAGE_SCOPE = 3;
	private static final int TRACKER_UUID = 1;
	private static final int TRACKER_VERSION = 2;

	// private static final int TRACKER_CV_SCREEN_ORIENTATION_SLOT = 3;

	public Globals(Context context) {
		isDebugBuild = CheckDebugBuild(context);
		mPreferences = new Preferences(context);
		dbHelper = new DatabaseHelper(context);

		tracker = GoogleAnalyticsTracker.getInstance();
		if (isDebugBuild) {
			tracker.setDebug(true);
			tracker.setDryRun(true);
			tracker.startNewSession(context.getString(R.string.ga_api_key), 1, context);
		} else {
			tracker.setDebug(false);
			tracker.setDryRun(false);
			tracker.startNewSession(context.getString(R.string.ga_api_key), 77, context);
		}

		tracker.setCustomVar(TRACKER_UUID, // Slot
				"UUID", // Name
				Globals.mPreferences.getUUID(), // Value
				TRACKER_VISITOR_SCOPE); // Scope

		try {
			String v = context.getPackageManager().getPackageInfo(context.getPackageName(), 0).versionName;
			if (isDebugBuild) {
				v += " debug";
			}
			tracker.setCustomVar(TRACKER_VERSION, "Version", v, TRACKER_VISITOR_SCOPE);
		} catch (final NameNotFoundException e) {
			Log.e(TAG, "Exception when getting versionName");
			e.printStackTrace();
		}
	}

	/**
	 * Show an about dialog that cites data sources.
	 */
	public static void showAbout(Activity context) {

		final View messageView = context.getLayoutInflater().inflate(R.layout.about, null, false);

		// When linking text, force to always use default color. This works
		// around a pressed color state bug.
		final TextView textView = (TextView) messageView.findViewById(R.id.about_credits);
		final int defaultColor = textView.getTextColors().getDefaultColor();
		textView.setTextColor(defaultColor);

		final AlertDialog.Builder builder = new AlertDialog.Builder(context);
		builder.setIcon(R.drawable.grticon);
		builder.setTitle(R.string.app_name);
		builder.setView(messageView);
		builder.create();
		builder.show();
	}

	// Checks if this apk was built using the debug certificate
	// See http://stackoverflow.com/questions/3029819/android-automatically-choose-debug-release-maps-api-key/3828864#3828864
	private static boolean checkedBuild = false;

	public static boolean CheckDebugBuild(Context context) {
		if (!checkedBuild) {
			try {
				final Signature[] sigs = context.getPackageManager().getPackageInfo(context.getPackageName(),
						PackageManager.GET_SIGNATURES).signatures;
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
