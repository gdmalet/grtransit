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
            
//          String q = String.format(
//          		"select distinct route_id as _id, trip_headsign as descr from trips "
//          		+ "where route_id like \"%s%%\" or trip_headsign like \"%%%s%%\" "
//          		+ "order by cast(route_id as integer)",
//          		query, query);
//			TODO Or use the routes file?            
//    		"select route_id as _id, route_long_name as descr from routes where route_id like \"%s%%\" or route_long_name like \"%%%s%%\"",
//    		query, query);
//          mCsr = BusstopsOverlay.DB.rawQuery(q, null);

            final String table = "trips";
            final String [] columns = {"distinct route_id as _id", "trip_headsign as descr"};
            final String whereclause = "route_id like ? || '%' or trip_headsign like '%' || ? || '%'";
            String [] selectargs = {query, query};
            final String orderby = "cast(route_id as integer)";
            mCsr = Globals.dbHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, orderby, null); 
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
//		Log.v(TAG, "clicked position " + position + ", route number " + id);
		
        String pkgstr = mContext.getApplicationContext().getPackageName();
        String routestr = pkgstr + ".route_id";
        String headsign = pkgstr + ".headsign";

		Intent route = new Intent(mContext, BusroutesActivity.class);
		route.putExtra(routestr, Integer.toString((int)id));
		mCsr.moveToPosition(position);
		route.putExtra(headsign, mCsr.getString(1));	// TODO - this is not actually the headsign if using routes file...
        route.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
		startActivity(route);
	}
}
