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

import java.lang.reflect.Array;
import java.util.ArrayList;

import android.content.Context;
import android.content.SharedPreferences;
import android.text.TextUtils;
import android.util.Pair;
import android.widget.Toast;

public class Preferences {

	private Context mContext;
	private static String mPrefsFile;
	private static final String FAVSTOPS_KEY = "favstops";
	private static final String SHOWALLBUSSES_KEY = "showallbusses";
	
	public Preferences(Context context) {
		mContext = context;
		mPrefsFile = mContext.getApplicationInfo().packageName;
	}
	
	public boolean getShowAllBusses() {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		return prefs.getBoolean(SHOWALLBUSSES_KEY, false);
	}
	
	public void setShowAllBusses(boolean b) {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		prefs.edit()
		.putBoolean(SHOWALLBUSSES_KEY, b)
		.commit();
	}
	
	public void AddBusstopFavourite(String busstop, String stopname) {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String favs = prefs.getString(FAVSTOPS_KEY, "");
	
		TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
		splitter.setString(favs);
		boolean already = false;
		for (String s : splitter) {
			if (s.equals(busstop)) {
				already = true;
				break;
			}
		}
	   
		if (already) {
			Toast.makeText(mContext, "Stop " + busstop + " is already a favourite!",
					Toast.LENGTH_LONG).show();;
		} else {
			favs += busstop + ";";
			prefs.edit().putString(FAVSTOPS_KEY, favs)
			.putString(FAVSTOPS_KEY + "-" + busstop, stopname)
			.commit();
			Toast.makeText(mContext, "Stop " + busstop + " was added to your favourites.",
					Toast.LENGTH_LONG).show();;
		}
	}
	
	public void RemoveBusstopFavourite(String busstop) {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String favs = prefs.getString(FAVSTOPS_KEY, "");
		String newfavs = "";
		
		TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
		splitter.setString(favs);

		for (String s : splitter) {
			if (!s.equals(busstop))
				newfavs += s + ";";
		}
		prefs.edit().putString(FAVSTOPS_KEY, newfavs)
		.remove(FAVSTOPS_KEY + "-" + busstop)
		.commit();
		Toast.makeText(mContext, "Stop " + busstop + " was removed from your favourites.",
				Toast.LENGTH_LONG).show();;
	}

	public ArrayList<Pair<String,String>> GetBusstopFavourites() {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String favs = prefs.getString(FAVSTOPS_KEY, "");
		
    	// Load the array for the list
		ArrayList<Pair<String,String>> details = new ArrayList<Pair<String,String>>();
        Pair<String,String> pair;

		// favs is a semi-colon separated string of stops, with a trailing semi-colon.
        // Then each stop has a description stored as KEY-stop.
		if (!favs.equals("")) {
			TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
			splitter.setString(favs);
			for (String s : splitter) {
        		pair = new Pair<String,String>(s, prefs.getString(FAVSTOPS_KEY + "-" + s, ""));
				details.add(pair);
			}
		}
		return details;
	}
}
