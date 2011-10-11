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
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

public class RouteselectActivity extends ListActivity {
	private static final String TAG = "RouteselectActivity";

	private ListActivity mContext;
	private String mStopid, mStopname;
	private Cursor mCsr;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
//    	Log.v(TAG, "OnCreate()");
        mContext = this;

        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        mStopid = intent.getStringExtra(pkgstr + ".stop_id");
        mStopname  = intent.getStringExtra(pkgstr + ".stop_name");

        setContentView(R.layout.timeslayout);
        TextView v = (TextView) findViewById(R.id.timestitle);
        v.setText("Routes using stop " + mStopid + ", " + mStopname);

        // Hide the `Show' button used for showing routes.
//        Button button = (Button) findViewById(R.id.timesbutton);
//        button.setVisibility(View.GONE);
        
        final Button button = (Button) findViewById(R.id.timesbutton);
        button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                // Perform action on click
                String pkgstr = mContext.getApplicationContext().getPackageName();
        		Intent busstop = new Intent(mContext, BusstopsActivity.class);
        		busstop.putExtra(pkgstr + ".stop_id", mStopid);
        		startActivity(busstop);
            }
        });


        // Find which routes use the given stop.
		final String table = "trips";
		final String [] select = {"distinct route_id as _id, trip_headsign as descr"};
		final String where = "trip_id in (select trip_id from stop_times where stop_id = ?)";
		final String [] selectargs = {mStopid};
		mCsr = DatabaseHelper.ReadableDB().query(table, select, where, selectargs, null,null,null);
		startManagingCursor(mCsr);
		
        SearchCursorAdapter adapter = new SearchCursorAdapter(this, mCsr);
    	setListAdapter(adapter);
	}
	
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
//		Log.v(TAG, "clicked position " + position);
		
		  String route_id = mCsr.getString(0);
		  String headsign = mCsr.getString(1);

		  Intent bustimes = new Intent(mContext, BustimesActivity.class);
		  String pkgstr = mContext.getApplicationContext().getPackageName();
		  bustimes.putExtra(pkgstr + ".route_id", route_id);
		  bustimes.putExtra(pkgstr + ".headsign", headsign);
		  bustimes.putExtra(pkgstr + ".stop_id", mStopid);
		  mContext.startActivity(bustimes);
	}
}
