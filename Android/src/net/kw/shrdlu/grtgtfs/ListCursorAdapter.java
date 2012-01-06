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

import android.app.ListActivity;
import android.content.Context;
import android.database.Cursor;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CursorAdapter;
import android.widget.TextView;

public class ListCursorAdapter extends CursorAdapter {
	// private static final String TAG = "ListCursorAdapter";

	private final LayoutInflater mInflater;
	private final int mLayout;

	public ListCursorAdapter(ListActivity context, int layout, Cursor cursor) {
		super(context, cursor, true);

		this.mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	@Override
	public void bindView(View view, Context context, Cursor cursor) {
		// Log.v(TAG, "bindView()");
		// Log.v(TAG, "cursor has " + cursor.getCount() + " entries, at " + cursor.getPosition());

		final TextView labelview = (TextView) view.findViewById(R.id.label);
		final String item = cursor.getString(cursor.getColumnIndex("_id"));

		final TextView valueview = (TextView) view.findViewById(R.id.value);
		final String descr = cursor.getString(cursor.getColumnIndex("descr"));

		final TextView timeview;
		final String time;
		if (mLayout == R.layout.timestopdesc) {
			timeview = (TextView) view.findViewById(R.id.time);
			time = cursor.getString(cursor.getColumnIndex("departure_time"));
			timeview.setText(time);
		}

		// Look for things like route 7A, where the A is part of the description
		// TODO - char test should use a type test or something. This assumes US ASCII...
		if (mLayout == R.layout.route_numanddesc && descr.length() > 2 && descr.charAt(1) == ' ' && descr.charAt(0) >= 'A'
				&& descr.charAt(0) <= 'Z') {
			labelview.setText(item + descr.charAt(0));
			valueview.setText(descr.substring(2));
		} else {
			labelview.setText(item);
			valueview.setText(descr);
		}
	}

	@Override
	public View newView(Context context, Cursor cursor, ViewGroup parent) {
		// Log.v(TAG, "newView()");

		return mInflater.inflate(mLayout, parent, false);
	}
}
