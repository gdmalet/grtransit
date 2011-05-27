/**
 * An adapter that is used when drawing the main window list of details.
 */
package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;

import android.app.Activity;
import android.content.Context;
import android.database.Cursor;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.CursorAdapter;
import android.widget.SimpleCursorAdapter;
import android.widget.TextView;

/**
 * @author gdmalet
 *
 */
public class ListCursorAdapter extends SimpleCursorAdapter {
	private final Activity context;
	private final Cursor details;
	
	public ListCursorAdapter(Activity context, int layout, Cursor details, String[] from, int[] to) {
		super(context, layout, details, from, to);
		this.context = context;
		this.details = details;
		
	}
	
	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		LayoutInflater inflater = context.getLayoutInflater();
		View rowView = inflater.inflate(R.layout.rowlayout, null, true);

		String str;
		
		TextView label = (TextView) rowView.findViewById(R.id.label);
//		str = details.get(position).first;
//		label.setText(str);

		TextView value = (TextView) rowView.findViewById(R.id.value);
//		str = details.get(position).second;
//		value.setText(str);
		
		return rowView;
	}
}
