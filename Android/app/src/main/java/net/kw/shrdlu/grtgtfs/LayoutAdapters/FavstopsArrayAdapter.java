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
package net.kw.shrdlu.grtgtfs.LayoutAdapters;

import android.app.ListActivity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import net.kw.shrdlu.grtgtfs.Activities.MenuListActivity;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.ServiceCalendar;

import java.util.ArrayList;

public class FavstopsArrayAdapter extends ArrayAdapter /* <ArrayList<String[]>> */{
	private static final String TAG = "FavstopsArrayAdapter";

	private final ArrayList<String[]> mDetails;
	private final LayoutInflater mInflater;
	private final int mLayout;

	public FavstopsArrayAdapter(MenuListActivity context, int layout, ArrayList<String[]> details) {
		super(context, layout, details);
		// Log.v(TAG, "FavstopsArrayAdapter()");

		mDetails = details;
		mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	static class ViewHolder {
		TextView stoplabel;
		TextView stopdesc;
		TextView stoptime;
		TextView routelabel;
		TextView routedesc;
	}

	@Override
	public View getView(int position, View view, ViewGroup parent) {
		// Log.v(TAG, "getview(): position " + position);
		ViewHolder holder;

		// Reuse the convertView if we already have one.... Android will create
		// only enough to fill the screen.
		if (view == null) {
			view = mInflater.inflate(mLayout, parent, false);
			// Log.d(TAG, "new view " + view);

			// Save the view when we look them up.
			holder = new ViewHolder();
			holder.stoplabel = (TextView) view.findViewById(R.id.stoplabel);
			holder.stopdesc = (TextView) view.findViewById(R.id.stopdesc);
			holder.stoptime = (TextView) view.findViewById(R.id.stoptime);
			holder.routelabel = (TextView) view.findViewById(R.id.routelabel);
			holder.routedesc = (TextView) view.findViewById(R.id.routedesc);
			view.setTag(holder);
		} else {
			// Log.d(TAG, "reusing view " + view);
			holder = (ViewHolder) view.getTag();
		}

		holder.stoplabel.setText(mDetails.get(position)[0]);
		holder.stopdesc.setText(mDetails.get(position)[1]);
		holder.stoptime.setText(ServiceCalendar.formattedTime(mDetails.get(position)[2]));

		// Look for things like route 7A, where the A is part of the description
		// TODO - char test should use a type test or something. This assumes US ASCII...
		String headsign = mDetails.get(position)[3];
		String route = mDetails.get(position)[4];
		if (headsign.length() > 2 && headsign.charAt(1) == ' ' && headsign.charAt(0) >= 'A' && headsign.charAt(0) <= 'Z') {
			route += headsign.charAt(0); // route number
			headsign = headsign.substring(2); // route headsign
		}

		holder.routedesc.setText(headsign);
		holder.routelabel.setText(route);

		return view;
	}
}
