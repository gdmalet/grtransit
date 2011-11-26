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
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.view.GestureDetector;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;

public class RouteselectActivity extends ListActivity implements AnimationListener {
	private static final String TAG = "RouteselectActivity";

	private ListActivity mContext;
    private View mListDetail;
    private Animation mSlideIn, mSlideOut;
    private ProgressBar mProgress;
	private String mStopid, mStopname;
    private TextView mTitle;
	private Cursor mCsr;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
//    	Log.v(TAG, "OnCreate()");

        setContentView(R.layout.timeslayout);
        mContext = this;

    	// Load animations used to show/hide progress bar
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mListDetail = findViewById(R.id.detail_area);
        mSlideIn  = AnimationUtils.loadAnimation(this, R.anim.slide_in);
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mSlideIn.setAnimationListener(this);
        mTitle = (TextView) findViewById(R.id.timestitle);

        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        mStopid = intent.getStringExtra(pkgstr + ".stop_id");
        mStopname = intent.getStringExtra(pkgstr + ".stop_name");

        // Do the rest off the main thread
        new ProcessRoutes().execute();
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this Activity gets the focus.
        Globals.tracker.trackPageView("/" + this.getLocalClassName());
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
//		Log.v(TAG, "clicked position " + position);
		
		  String route_id = mCsr.getString(0);
		  String headsign = mCsr.getString(1);
		  
		  Globals.tracker.trackEvent("Routes","Select route",route_id + " - " + headsign,1);

		  Intent bustimes = new Intent(mContext, TimesActivity.class);
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
//    		Log.d(TAG, "fling X " + velocityX + ", Y " + velocityY);
    		// Catch a fling sort of from right to left
    		if (velocityX < -100 && Math.abs(velocityX) > Math.abs(velocityY)) {
//    			Log.d(TAG, "fling detected");
    			Globals.tracker.trackEvent("Routes","Select all",mStopid,1);
				Intent bustimes = new Intent(mContext, TimesActivity.class);
				String pkgstr = mContext.getApplicationContext().getPackageName();
				bustimes.putExtra(pkgstr + ".stop_id", mStopid);
				mContext.startActivity(bustimes);
    			return true;
    		}
    		return false;
    	}
    });
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.routeselectmenu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_showonmap: {
            	Globals.tracker.trackEvent("Menu","Show stop", mStopid, 1);
                String pkgstr = mContext.getApplicationContext().getPackageName();
        		Intent busstop = new Intent(mContext, StopsActivity.class);
        		busstop.putExtra(pkgstr + ".stop_id", mStopid);
        		startActivity(busstop);
        		return true;
            }
        }
    	return super.onOptionsItemSelected(item);
    }
    
	/*
	 * Do the processing to load the ArrayAdapter for display.
	 */
	private class ProcessRoutes extends AsyncTask<Void, Integer, Void> {
		static final String TAG = "ProcessBusTimes";

		private Integer maxcount, progresscount = 0;
		private int savedpos = -1, currentcount = 0;
		
		@Override
		protected void onPreExecute() {
			// Log.v(TAG, "onPreExecute()");
			mListDetail.startAnimation(mSlideIn);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
		}
		
		@Override
		protected Void doInBackground(Void... foo) {
			// Log.v(TAG, "doInBackground()");

			publishProgress(25);	// fake it

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
	        	
			publishProgress(50);	// fake it
			startManagingCursor(mCsr);

	    	ListView lv = getListView();
	        lv.setOnTouchListener(mGestureListener);

			publishProgress(75);	// fake it
	        if (mCsr.getCount()>1) {
	        	// Show msg describing a fling to see times for all routes.
		        TextView tv = new TextView(mContext);
		        tv.setText(R.string.route_fling);
		        lv.addFooterView(tv);
	        } else if (mCsr.getCount() == 0) {
		        TextView tv = new TextView(mContext);
		        tv.setText(R.string.stop_unused);
		        lv.addFooterView(tv);        	
	        }
	        return null;
		}
		
		@Override
		protected void onPostExecute(Void foo) {
			//    	Log.v(TAG, "onPostExecute()");
			
			setListAdapter(new SearchCursorAdapter(mContext, mCsr));
			
			publishProgress(100);	// fake it

			mProgress.setVisibility(View.INVISIBLE);
	    	mListDetail.startAnimation(mSlideOut);
            mTitle.setText("Routes using stop " + mStopid + ", " + mStopname);
		}
	}
	
	/**
     * Make the {@link ProgressBar} visible when our in-animation finishes.
     */
    public void onAnimationEnd(Animation animation) {
    	mProgress.setVisibility(View.VISIBLE);
    }
    public void onAnimationRepeat(Animation animation) {
    	// Not interested if the animation repeats
    }
    public void onAnimationStart(Animation animation) {
    	// Not interested when the animation starts
    }
}
