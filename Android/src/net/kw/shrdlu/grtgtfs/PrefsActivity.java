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

import android.content.Intent;
import android.os.Bundle;
import android.preference.PreferenceActivity;
import android.util.Log;

public class PrefsActivity extends PreferenceActivity {
	private static final String TAG = "PreferenceActivity";

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Override the default file name, since we already use a name.
		getPreferenceManager().setSharedPreferencesName(getApplicationInfo().packageName);

		addPreferencesFromResource(R.xml.preferences);
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this activity gets the focus - but if the activity was
		// previously destroyed we could have lost our global data, so this is a bit of a hack to avoid a crash!
		if (Globals.tracker == null) {
			Log.e(TAG, "null tracker!");
			startActivity(new Intent(this, FavstopsActivity.class));
		} else {
			Globals.tracker.trackPageView("/" + this.getLocalClassName());
		}
	}
}
