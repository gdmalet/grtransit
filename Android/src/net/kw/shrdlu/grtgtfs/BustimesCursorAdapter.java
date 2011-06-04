/**
 * An adapter that is used when drawing the main window list of details.
 */
package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.content.Context;
import android.database.Cursor;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CursorAdapter;
import android.widget.TextView;

public class BustimesCursorAdapter extends CursorAdapter {
	private static final String TAG = "ListCursorAdapter";

	private final LayoutInflater mInflater;
	
	public BustimesCursorAdapter(ListActivity context, Cursor cursor) {
    	super(context, cursor, true);
    	Log.v(TAG, "BustimesCursorAdapter()");
    
		this.mInflater = LayoutInflater.from(context);
	}
	
	@Override
	public void bindView(View view, Context context, Cursor cursor) {
//    	Log.v(TAG, "bindView()");
//    	Log.v(TAG, "cursor has " + cursor.getCount() + " entries, at " + cursor.getPosition());

    	TextView labelview = (TextView) view.findViewById(R.id.label);
    	String bus_time = cursor.getString(cursor.getColumnIndex("_id"));
		labelview.setText(bus_time);

		// Get and translate the service id
		String trip_id = cursor.getString(cursor.getColumnIndex("trip_id"));
		String q = String.format("select service_id from trips where trip_id = \"%s\"", trip_id);
        Cursor csr = BusstopsOverlay.DB.rawQuery(q, null);
        csr.moveToFirst();
        String service_id = csr.getString(0);
        csr.close();
        
		TextView valueview = (TextView)view.findViewById(R.id.value);
		valueview.setText(service_id);
		
//		Log.v(TAG, "Bound <" + bus_time + ">, <" + service_id + ">");
}
 
	@Override
	public View newView(Context context, Cursor cursor, ViewGroup parent) {
//    	Log.v(TAG, "newView()");
    	
    	return mInflater.inflate(R.layout.rowlayout, parent, false);
	}
}
