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
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class TwoRowArrayAdapter extends ArrayAdapter /* <ArrayList<String[]>> */{
	private static final String TAG = "TwoRowAdapter";

	private final ArrayList<String[]> mDetails;
	private final LayoutInflater mInflater;
	private final int mLayout;

	public TwoRowArrayAdapter(ListActivity context, int layout, ArrayList<String[]> details) {
		super(context, layout, details);
		// Log.v(TAG, "TwoRowArrayAdapter()");

		mDetails = details;
		mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	static class ViewHolder {
		TextView label;
		TextView value;
		TextView label2;
		TextView value2;
		TextView routelabel;
	}

	@Override
	public View getView(int position, View view, ViewGroup parent) {
		// Log.v(TAG, "getview(): position " + position);
		ViewHolder holder;

		// If we're filling the favouritesrow, then there's an extra field,
		// the route number, which needs to be put its own textview.

		// Reuse the convertView if we already have one.... Android will create
		// only enough to fill the screen.
		if (view == null) {
			view = mInflater.inflate(mLayout, parent, false);
			// Log.d(TAG, "new view " + view);

			// Save the view when we look them up.
			holder = new ViewHolder();
			holder.label = (TextView) view.findViewById(R.id.label);
			holder.value = (TextView) view.findViewById(R.id.value);
			holder.label2 = (TextView) view.findViewById(R.id.label2);
			holder.value2 = (TextView) view.findViewById(R.id.value2);
			if (mLayout == R.layout.favouritesrow) holder.routelabel = (TextView) view.findViewById(R.id.routelabel);
			view.setTag(holder);
		} else {
			// Log.d(TAG, "reusing view " + view);
			holder = (ViewHolder) view.getTag();
		}

		holder.label.setText(mDetails.get(position)[0]);
		holder.value.setText(mDetails.get(position)[1]);

		String route, headsign;
		if (mLayout != R.layout.favouritesrow) {
			route = mDetails.get(position)[2];
			headsign = mDetails.get(position)[3];
		} else {
			route = mDetails.get(position)[4];
			headsign = mDetails.get(position)[3];
		}

		// Look for things like route 7A, where the A is part of the description
		// TODO - char test should use a type test or something. This assumes US ASCII...
		if (headsign.length() > 2 && headsign.charAt(1) == ' ' && headsign.charAt(0) >= 'A' && headsign.charAt(0) <= 'Z') {
			route += headsign.charAt(0); // route number
			headsign = headsign.substring(2); // route headsign
		}

		if (mLayout != R.layout.favouritesrow) {
			holder.label2.setText(route);
			holder.value2.setText(headsign);
		} else {
			holder.label2.setText(mDetails.get(position)[2]);
			holder.value2.setText(headsign);
			holder.routelabel.setText(route);
		}
		return view;
	}
}
