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

public class TwoRowAdapter extends ArrayAdapter /* <ArrayList<String[]>> */{
	private static final String TAG = "TwoRowAdapter";

	private final ArrayList<String[]> mDetails;
	private final LayoutInflater mInflater;
	private final int mLayout;

	public TwoRowAdapter(ListActivity context, int layout, ArrayList<String[]> details) {
		super(context, layout, details);
		// Log.v(TAG, "TimesArrayAdapter()");

		mDetails = details;
		mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	static class ViewHolder {
		TextView label;
		TextView value;
		TextView label2;
		TextView value2;
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
			holder.label = (TextView) view.findViewById(R.id.label);
			holder.value = (TextView) view.findViewById(R.id.value);
			holder.label2 = (TextView) view.findViewById(R.id.label2);
			holder.value2 = (TextView) view.findViewById(R.id.value2);
			view.setTag(holder);
		} else {
			// Log.d(TAG, "reusing view " + view);
			holder = (ViewHolder) view.getTag();
		}

		holder.label.setText(mDetails.get(position)[0]);
		holder.value.setText(mDetails.get(position)[1]);
		holder.label2.setText(mDetails.get(position)[2]);
		holder.value2.setText(mDetails.get(position)[3]);

		return view;
	}
}
