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
import android.content.Context;
import android.database.Cursor;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CursorAdapter;
import android.widget.TextView;

import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.ServiceCalendar;

public class ListCursorAdapter extends CursorAdapter {

	private final LayoutInflater mInflater;
	private final int mLayout;

	public ListCursorAdapter(Activity context, int layout, Cursor cursor) {
		super(context, cursor, true);

		this.mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	@Override
	public void bindView(View view, Context context, Cursor cursor)
	{
		final String item, desc;

        TextView itemview = (TextView) view.findViewById(R.id.label);
        TextView descview = (TextView) view.findViewById(R.id.desc);

		item = cursor.getString(cursor.getColumnIndex("_id"));
        desc = cursor.getString(cursor.getColumnIndex("descr"));

        final TextView timeview;
        final String time;
        if (mLayout == R.layout.timestopdesc) {
            timeview = (TextView) view.findViewById(R.id.stoptime);
            time = cursor.getString(cursor.getColumnIndex("departure_time"));
            timeview.setText(ServiceCalendar.formattedTime(time));
        }

		// Look for things like route 7A, where the A is part of the description
		if (mLayout == R.layout.route_numanddesc && desc.length() > 4
				&& desc.charAt(1) == ' '
				&& desc.charAt(2) == '-'
				&& desc.charAt(3) == ' '
				&& Character.isUpperCase(desc.charAt(0))) {
			itemview.setText(item + desc.charAt(0));
			descview.setText(desc.substring(4));
		} else {
			itemview.setText(item);
			descview.setText(desc);
		}
	}

	@Override
	public View newView(Context context, Cursor cursor, ViewGroup parent) {

		return mInflater.inflate(mLayout, parent, false);
	}
}
