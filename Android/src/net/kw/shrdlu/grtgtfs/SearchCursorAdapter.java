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

public class SearchCursorAdapter extends CursorAdapter {
	private static final String TAG = "SearchCursorAdapter";

	private final LayoutInflater mInflater;
	
	public SearchCursorAdapter(ListActivity context, Cursor cursor) {
    	super(context, cursor, true);
    	Log.v(TAG, "SearchCursorAdapter()");
    
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
