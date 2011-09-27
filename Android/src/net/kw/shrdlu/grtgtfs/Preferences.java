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
import android.widget.Toast;

public class Preferences {

	private Context mContext;
	private String mPrefsFile;
	private final String mPrefFavstops = "favstops";
	
	public Preferences(Context context) {
		mContext = context;
		mPrefsFile = mContext.getApplicationInfo().packageName;
	}
	
	public void SaveBusstopFavourite(String busstop) {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String favs = prefs.getString(mPrefFavstops, "");
	
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
			prefs.edit().putString("favstops", favs).commit();
			Toast.makeText(mContext, "Stop " + busstop + " was added to your favourites",
					Toast.LENGTH_LONG).show();;
		}
	}
	
	public String [] GetBusstopFavourites() {
		SharedPreferences prefs = mContext.getSharedPreferences(mPrefsFile, Context.MODE_PRIVATE);
		String favs = prefs.getString(mPrefFavstops, "");
		
		String [] stops = {};

		// favs is a semi-colon separated string of stops, with a trailing semi-colon.
		if (!favs.equals("")) {
			TextUtils.StringSplitter splitter = new TextUtils.SimpleStringSplitter(';');
			splitter.setString(favs);
			ArrayList<String> ary = new ArrayList<String>();
			for (String s : splitter)
				ary.add(s);
			stops = ary.toArray(new String[0]);
		}
		
		return stops;
	}
}
