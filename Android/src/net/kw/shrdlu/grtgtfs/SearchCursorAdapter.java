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

public class SearchCursorAdapter extends CursorAdapter {
	private static final String TAG = "SearchCursorAdapter";

	private final LayoutInflater mInflater;
	
	public SearchCursorAdapter(ListActivity context, Cursor cursor) {
    	super(context, cursor, true);
//    	Log.v(TAG, "SearchCursorAdapter()");
    
		this.mInflater = LayoutInflater.from(context);
	}
	
	@Override
	public void bindView(View view, Context context, Cursor cursor) {
//    	Log.v(TAG, "bindView()");
//    	Log.v(TAG, "cursor has " + cursor.getCount() + " entries, at " + cursor.getPosition());

    	TextView labelview = (TextView) view.findViewById(R.id.label);
    	String item = cursor.getString(cursor.getColumnIndex("_id"));
		labelview.setText(item);

		TextView valueview = (TextView)view.findViewById(R.id.value);
    	String descr = cursor.getString(cursor.getColumnIndex("descr"));
		valueview.setText(descr);
		
//		Log.v(TAG, "Bound <" + item + ">, <" + descr + ">");
}
 
	@Override
	public View newView(Context context, Cursor cursor, ViewGroup parent) {
//    	Log.v(TAG, "newView()");
    	
    	return mInflater.inflate(R.layout.rowlayout, parent, false);
	}
}
