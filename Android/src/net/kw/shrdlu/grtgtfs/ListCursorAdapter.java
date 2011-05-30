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

public class ListCursorAdapter extends CursorAdapter {
	private static final String TAG = "ListCursorAdapter";

	private final LayoutInflater mInflater;
//	private final ListActivity mContext;
	
	public ListCursorAdapter(ListActivity context, Cursor cursor) {
    	super(context, cursor, true);
    	Log.v(TAG, "ListCursorAdapter()");
    
//    	this.mContext = context;
		this.mInflater = LayoutInflater.from(context);
	}
	
	@Override
	public void bindView(View view, Context context, Cursor cursor) {
    	Log.v(TAG, "bindView()");
    	Log.v(TAG, "cursor has " + cursor.getCount() + " entries, at " + cursor.getPosition());

    	LayoutInflater inflater = LayoutInflater.from(context);
    	View rowView = inflater.inflate(R.layout.rowlayout, null, false);
//    	View rowView = mInflater.inflate(R.layout.rowlayout, null, false);
  
    	TextView labelview = (TextView) rowView.findViewById(R.id.label);
    	String bus_time = cursor.getString(cursor.getColumnIndex("_id"));
		labelview.setText(bus_time);

		// Get and translate the service id
		String trip_id = cursor.getString(cursor.getColumnIndex("trip_id"));
		String q = String.format("select service_id from trips where trip_id = \"%s\"", trip_id);
        Cursor csr = GrtGtfs.DB.rawQuery(q, null);
        csr.moveToFirst();
        String service_id = csr.getString(0);
        csr.close();
        
		TextView valueview = (TextView)rowView.findViewById(R.id.value);
		valueview.setText(service_id);
		
		ListActivity foo = (ListActivity)context;
		foo.addContentView(rowView, new ViewGroup.LayoutParams(
				ViewGroup.LayoutParams.WRAP_CONTENT,
				ViewGroup.LayoutParams.WRAP_CONTENT));
		
		Log.v(TAG, "Bound <" + bus_time + ">, <" + service_id + ">");
}
 
	@Override
	public View newView(Context context, Cursor cursor, ViewGroup parent) {
    	Log.v(TAG, "newView()");
    	Log.v(TAG, "cursor has " + cursor.getCount() + " entries, at " + cursor.getPosition());

    	LayoutInflater inflater = LayoutInflater.from(context);
    	return inflater.inflate(R.layout.timesview, parent, false);
	}
}
