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

import com.google.android.maps.MapView;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.util.TimingLogger;
import android.view.GestureDetector;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
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
    	Log.v(TAG, "OnCreate()");
		Log.d(TAG, "Log.isLoggable says " + Log.isLoggable(TAG, Log.VERBOSE));
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
   		// Only show bus routes where the schedule is valid for the current date
    	final Time t = new Time();	// TODO - this duplicates BusTimes?
    	t.setToNow();
    	final String datenow = String.format("%04d%02d%02d", t.year, t.month+1, t.monthDay);
    	final String qry = "select distinct route_id as _id, trip_headsign as descr from trips" +
    	" join calendar on trips.service_id = calendar.service_id where " + 
    	" trip_id in (select trip_id from stop_times where stop_id = ?) and " +
    	" start_date <= ? and end_date >= ?";
   		final String [] selectargs = {mStopid, datenow, datenow};
    	mCsr = DatabaseHelper.ReadableDB().rawQuery(qry, selectargs);
        	
		startManagingCursor(mCsr);

    	ListView lv = getListView();
        lv.setOnTouchListener(mGestureListener);

        TextView tv = new TextView(mContext);
        tv.setText(R.string.route_fling);
        lv.addFooterView(tv);

        SearchCursorAdapter adapter = new SearchCursorAdapter(this, mCsr);
    	setListAdapter(adapter);
	}
	
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		Log.v(TAG, "clicked position " + position);
		
		  String route_id = mCsr.getString(0);
		  String headsign = mCsr.getString(1);

		  Intent bustimes = new Intent(mContext, BustimesActivity.class);
		  String pkgstr = mContext.getApplicationContext().getPackageName();
		  bustimes.putExtra(pkgstr + ".route_id", route_id);
		  bustimes.putExtra(pkgstr + ".headsign", headsign);
		  bustimes.putExtra(pkgstr + ".stop_id", mStopid);
		  mContext.startActivity(bustimes);
	}

	private View.OnTouchListener mGestureListener = new View.OnTouchListener() {
		public boolean onTouch(View v, MotionEvent event) {
			return mGestureDetector.onTouchEvent(event); 
		}
	};

	// Catch flings, to show all busses coming to this stop.
	// This must be called on the GIU thread.
    private GestureDetector mGestureDetector = new GestureDetector(mContext, new GestureDetector.SimpleOnGestureListener() {
    	public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
    		Log.d(TAG, "fling X " + velocityX + ", Y " + velocityY);
    		// Catch a fling sort of from right to left
    		if (velocityX < -100 && Math.abs(velocityX) > Math.abs(velocityY)) {
    			Log.d(TAG, "fling detected");
				Intent bustimes = new Intent(mContext, BustimesActivity.class);
				String pkgstr = mContext.getApplicationContext().getPackageName();
				bustimes.putExtra(pkgstr + ".stop_id", mStopid);
				mContext.startActivity(bustimes);
    			return true;
    		}
    		return false;
    	}
    });
}
