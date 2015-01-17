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

import android.app.Activity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import net.kw.shrdlu.grtgtfs.R;

import java.util.ArrayList;

public class TimeStopdescArrayAdapter extends ArrayAdapter /* <ArrayList<String[]>> */{
	private static final String TAG = "timestopdescAdapter";

	private final ArrayList<String[]> mDetails;
	private final LayoutInflater mInflater;
	private final int mLayout;

	public TimeStopdescArrayAdapter(Activity context, int layout, ArrayList<String[]> details) {
		super(context, layout, details);
		// Log.v(TAG, "TimeStopdescArrayAdapter()");

		mDetails = details;
		mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	static class ViewHolder {
		TextView stoptime;
		TextView label;
		TextView value;
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
			holder.stoptime = (TextView) view.findViewById(R.id.stoptime);
			holder.label = (TextView) view.findViewById(R.id.label);
			holder.value = (TextView) view.findViewById(R.id.value);
			view.setTag(holder);
		} else {
			// Log.d(TAG, "reusing view " + view);
			holder = (ViewHolder) view.getTag();
		}

		holder.stoptime.setText(mDetails.get(position)[0]);
		holder.label.setText(mDetails.get(position)[1]);
		holder.value.setText(mDetails.get(position)[2]);

		return view;
	}
}
