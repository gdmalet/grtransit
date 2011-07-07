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

/**
 * An adapter that is used when drawing the main window list of details.
 */
package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;

import android.app.ListActivity;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class BustimesArrayAdapter extends ArrayAdapter/*<ArrayList<Pair<String,String>>>*/ {
	private static final String TAG = "BustimesArrayAdapter";

	private final ArrayList<Pair<String,String>> mDetails;
	private final LayoutInflater mInflater;
	
	public BustimesArrayAdapter(ListActivity context, ArrayList<Pair<String,String>> details) {
    	super(context, R.layout.rowlayout, details);
//    	Log.v(TAG, "BustimesArrayAdapter()");
    
    	mDetails = details;
    	mInflater = LayoutInflater.from(context);
	}
	
	@Override
	public View getView(int position, View view, ViewGroup parent) {
//    	Log.v(TAG, "getview(): position " + position);

    	View rowview = mInflater.inflate(R.layout.rowlayout, null, false);

    	TextView labelview = (TextView) rowview.findViewById(R.id.label);
		labelview.setText(mDetails.get(position).first);

		TextView valueview = (TextView) rowview.findViewById(R.id.value);
		valueview.setText(mDetails.get(position).second);

		return rowview;
	}
}
