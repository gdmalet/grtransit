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

package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.app.SearchManager;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

public class RoutesearchActivity extends ListActivity {
	private static final String TAG = "RoutesearchActivity";

	private RoutesearchActivity mContext;
    private TextView mTitle;
	private Cursor mCsr;

	@Override
    public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.timeslayout);

//    	Log.v(TAG, "OnCreate()");
    	mContext = this;
   	
        Intent intent = getIntent();
        if (intent.getAction().equals(Intent.ACTION_SEARCH)) {

            String query = intent.getStringExtra(SearchManager.QUERY);

            mTitle = (TextView) findViewById(R.id.timestitle);
            mTitle.setText("Routes matching `" + query + "'");
        
            // Hide the `Show' button used for showing routes.
            Button btn = (Button) findViewById(R.id.timesbutton);
            btn.setVisibility(View.GONE);
            
            final String table = "trips";
            final String [] columns = {"distinct route_id as _id", "trip_headsign as descr"};
            final String whereclause = "route_id like '%' || ? || '%' or trip_headsign like '%' || ? || '%'";
            String [] selectargs = {query, query};
            final String orderby = "cast(route_id as integer)";
            mCsr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, orderby, null); 
            startManagingCursor(mCsr);

	        SearchCursorAdapter adapter = new SearchCursorAdapter(this, mCsr);
	    	setListAdapter(adapter);
        } else {
        	// Called from another activity, so put up search box
        	onSearchRequested();
        }
    }
	
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
//		Log.v(TAG, "clicked position " + position);
		
		Cursor csr = (Cursor)l.getItemAtPosition(position);
		final String route_id = csr.getString(0);
		final String route_name = csr.getString(1);

		final String pkgstr = mContext.getApplicationContext().getPackageName();
        final String routestr = pkgstr + ".route_id";
        final String headsign = pkgstr + ".headsign";

		Intent route = new Intent(mContext, BusroutesActivity.class);
		route.putExtra(routestr, route_id);
		route.putExtra(headsign, route_name);	// TODO - this is not actually the headsign if using routes file...
        route.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
		startActivity(route);
	}
}
