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

import java.util.ArrayList;
import java.util.UUID;

import android.content.Context;
import android.content.SharedPreferences;
import android.text.TextUtils;
import android.widget.Toast;

public class Preferences {

	private final Context mContext;
	private static String mPrefsFile;
	private static SharedPreferences mPrefs;

	private static String AMPMTIMES_KEY, AUTOUPDATE_KEY, FAVSTOPS_KEY, SHOWALLBUSSES_KEY, UUID_KEY;

	public Preferences(Context context) {
		mContext = context;
		mPrefsFile = mContext.getApplicationInfo().packageName;
		mPrefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);

		AMPMTIMES_KEY = new String(mContext.getString(R.string.pref_ampmtimes_key));
		AUTOUPDATE_KEY = new String(mContext.getString(R.string.pref_db_autoupdate_key));
		FAVSTOPS_KEY = new String(mContext.getString(R.string.pref_favstops_key));
		SHOWALLBUSSES_KEY = new String(mContext.getString(R.string.pref_showallbusses_key));
		UUID_KEY = new String(mContext.getString(R.string.pref_uuid_key));
	}

	// If we don't already have a uuid, generate and save one.
	public String getUUID() {
		String uuid = mPrefs.getString(UUID_KEY, "");
		if (uuid.equals("")) {
			uuid = UUID.randomUUID().toString();
			mPrefs.edit().putString(UUID_KEY, uuid).commit();
		}
		return uuid;
	}

	/* Force values to match the defaults the first time round, so the preferences screen reflects that. This is only necessary
	 * where the default is `true', since on-screen the default is `false'. */
	public boolean autoDbUpdate() {
		if (!mPrefs.contains(AUTOUPDATE_KEY)) {
			mPrefs.edit().putBoolean(AUTOUPDATE_KEY, true).commit();
		}
		return mPrefs.getBoolean(AUTOUPDATE_KEY, true);
	}

	public boolean showAllBusses() {
		return mPrefs.getBoolean(SHOWALLBUSSES_KEY, false);
	}

	public boolean showAMPMTimes() {
		return mPrefs.getBoolean(AMPMTIMES_KEY, false);
	}

	public void AddBusstopFavourite(String busstop, String stopname) {
		String favs = mPrefs.getString(FAVSTOPS_KEY, "");

		final TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
		splitter.setString(favs);
		boolean already = false;
		for (final String s : splitter) {
			if (s.equals(busstop)) {
				already = true;
				break;
			}
		}

		if (already) {
			Toast.makeText(mContext, "Stop " + busstop + " is already a favourite!", Toast.LENGTH_LONG).show();
			;
		} else {
			favs += busstop + ";";
			mPrefs.edit().putString(FAVSTOPS_KEY, favs).putString(FAVSTOPS_KEY + "-" + busstop, stopname).commit();
			Toast.makeText(mContext, "Stop " + busstop + " was added to your favourites.", Toast.LENGTH_LONG).show();
			;
		}
		GRTApplication.tracker.trackEvent("Favourites", "Add stop", busstop, 1);
	}

	public void RemoveBusstopFavourite(String busstop) {
		final String favs = mPrefs.getString(FAVSTOPS_KEY, "");
		String newfavs = "";

		final TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
		splitter.setString(favs);

		for (final String s : splitter) {
			if (!s.equals(busstop)) {
				newfavs += s + ";";
			}
		}
		mPrefs.edit().putString(FAVSTOPS_KEY, newfavs).remove(FAVSTOPS_KEY + "-" + busstop).commit();
		Toast.makeText(mContext, "Stop " + busstop + " was removed from your favourites.", Toast.LENGTH_LONG).show();

		GRTApplication.tracker.trackEvent("Favourites", "Remove stop", busstop, 1);
	}

	public ArrayList<String[]> GetBusstopFavourites() {
		final String favs = mPrefs.getString(FAVSTOPS_KEY, "");

		// Load the array for the list
		final ArrayList<String[]> details = new ArrayList<String[]>();

		// favs is a semi-colon separated string of stops, with a trailing semi-colon.
		// Then each stop has a description stored as KEY-stop.
		if (!favs.equals("")) {
			final TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
			splitter.setString(favs);
			for (final String s : splitter) {
				final String[] strs = { s, mPrefs.getString(FAVSTOPS_KEY + "-" + s, "") };
				details.add(strs);
			}
		}
		return details;
	}
}
