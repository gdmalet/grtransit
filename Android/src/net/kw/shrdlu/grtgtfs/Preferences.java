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
	private static final String FAVSTOPS_KEY = "favstops";
	private static final String SHOWALLBUSSES_KEY = "showallbusses";
	private static final String UUID_KEY = "uuid";
	private static final String AMPMTIMES_KEY = "ampmtimes";

	public Preferences(Context context) {
		mContext = context;
		mPrefsFile = mContext.getApplicationInfo().packageName;
	}

	// If we don't already have a uuid, generate and save one.
	public String getUUID() {
		final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String uuid = prefs.getString(UUID_KEY, "");
		if (uuid.equals("")) {
			uuid = UUID.randomUUID().toString();
			prefs.edit().putString(UUID_KEY, uuid).commit();
		}
		return uuid;
	}

	public boolean getShowAllBusses() {
		final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		return prefs.getBoolean(SHOWALLBUSSES_KEY, false);
	}

	// public void setShowAllBusses(boolean b) {
	// final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
	// prefs.edit().putBoolean(SHOWALLBUSSES_KEY, b).commit();
	// Globals.tracker.trackEvent("Preference", "Show all busses", b ? "true" : "false", 1);
	// }

	public boolean showAMPMTimes() {
		final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		return prefs.getBoolean(AMPMTIMES_KEY, false);
	}

	// public void setAMPMTimes(boolean b) {
	// final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
	// prefs.edit().putBoolean(AMPMTIMES_KEY, b).commit();
	// Globals.tracker.trackEvent("Preference", "AM/PM Times", b ? "true" : "false", 1);
	// }

	public void AddBusstopFavourite(String busstop, String stopname) {
		final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String favs = prefs.getString(FAVSTOPS_KEY, "");

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
			prefs.edit().putString(FAVSTOPS_KEY, favs).putString(FAVSTOPS_KEY + "-" + busstop, stopname).commit();
			Toast.makeText(mContext, "Stop " + busstop + " was added to your favourites.", Toast.LENGTH_LONG).show();
			;
		}
		Globals.tracker.trackEvent("Favourites", "Add stop", busstop, 1);
	}

	public void RemoveBusstopFavourite(String busstop) {
		final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		final String favs = prefs.getString(FAVSTOPS_KEY, "");
		String newfavs = "";

		final TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
		splitter.setString(favs);

		for (final String s : splitter) {
			if (!s.equals(busstop)) newfavs += s + ";";
		}
		prefs.edit().putString(FAVSTOPS_KEY, newfavs).remove(FAVSTOPS_KEY + "-" + busstop).commit();
		Toast.makeText(mContext, "Stop " + busstop + " was removed from your favourites.", Toast.LENGTH_LONG).show();

		Globals.tracker.trackEvent("Favourites", "Remove stop", busstop, 1);
	}

	public ArrayList<String[]> GetBusstopFavourites() {
		final SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		final String favs = prefs.getString(FAVSTOPS_KEY, "");

		// Load the array for the list
		final ArrayList<String[]> details = new ArrayList<String[]>();

		// favs is a semi-colon separated string of stops, with a trailing semi-colon.
		// Then each stop has a description stored as KEY-stop.
		if (!favs.equals("")) {
			final TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
			splitter.setString(favs);
			for (final String s : splitter) {
				final String[] strs = { s, prefs.getString(FAVSTOPS_KEY + "-" + s, "") };
				details.add(strs);
			}
		}
		return details;
	}
}
