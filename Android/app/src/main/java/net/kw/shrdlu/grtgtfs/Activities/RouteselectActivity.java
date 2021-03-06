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

package net.kw.shrdlu.grtgtfs.Activities;

import android.content.Intent;
import android.database.Cursor;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;

import com.google.android.gms.analytics.HitBuilders;

import net.kw.shrdlu.grtgtfs.GRTApplication;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.ListCursorAdapter;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.ServiceCalendar;

public class RouteselectActivity extends MenuListActivity {
	private static final String TAG = "RouteselectActivity";

	private String mStopid, mStopname;
	private Cursor mCsr = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;

        // Will use the action bar progress bar
        requestWindowFeature(Window.FEATURE_PROGRESS);

        setContentView(R.layout.timeslayout);
		super.onCreate(savedInstanceState);

        getActionBar().setTitle("Select a Route");
        getActionBar().setSubtitle(null);

		final String pkgstr = mContext.getApplicationContext().getPackageName();
		final Intent intent = getIntent();
		mStopid = intent.getStringExtra(pkgstr + ".stop_id");
		mStopname = intent.getStringExtra(pkgstr + ".stop_name");

        View v = findViewById(R.id.detail_area);
        v.setOnTouchListener(mGestureListener);

		// Do the rest off the main thread
		new ProcessRoutes().execute();
	}

    @Override
    public void onDestroy()
    {
		super.onDestroy();

        if (mCsr != null)
            mCsr.close();
    }

	public void onListItemClick(View view)
    {
        LinearLayout v = (LinearLayout)view;
        TextView tv;

        tv = (TextView)v.getChildAt(0);
        final String route_id = String.valueOf(tv.getText());
        tv = (TextView)v.getChildAt(1);
        final String headsign = String.valueOf(tv.getText());

		if (route_id == null || headsign == null) {
			return;
		}

        GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                .setCategory(mContext.getLocalClassName())
                .setAction("Select route")
                .setLabel(route_id + " - " + headsign)
                .build());

		final Intent bustimes = new Intent(mContext, TimesActivity.class);
		final String pkgstr = mContext.getApplicationContext().getPackageName();
		bustimes.putExtra(pkgstr + ".route_id", route_id);
		bustimes.putExtra(pkgstr + ".headsign", headsign);
		bustimes.putExtra(pkgstr + ".stop_id", mStopid);
        bustimes.putExtra(pkgstr + ".stop_name", mStopname);

        mContext.startActivity(bustimes);
	}

	private final View.OnTouchListener mGestureListener = new View.OnTouchListener() {
		@Override
		public boolean onTouch(View v, MotionEvent event) {
			return mGestureDetector.onTouchEvent(event);
		}
	};

	// Catch flings, to show all busses coming to this stop.
	// This must be called on the GIU thread.
	private final GestureDetector mGestureDetector = new GestureDetector(mContext,
			new GestureDetector.SimpleOnGestureListener() {
				@Override
				public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
					// Log.d(TAG, "fling X " + velocityX + ", Y " + velocityY);
					// Catch a fling sort of from right to left
					if (velocityX < -100 && Math.abs(velocityX) > Math.abs(velocityY)) {
						// Log.d(TAG, "left fling detected");
                        GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                                .setCategory(mContext.getLocalClassName())
                                .setAction("fling left")
                                .setLabel(mStopid)
                                .build());
						final Intent bustimes = new Intent(mContext, TimesActivity.class);
						final String pkgstr = mContext.getApplicationContext().getPackageName();
						bustimes.putExtra(pkgstr + ".stop_id", mStopid);
                        bustimes.putExtra(pkgstr + ".stop_name", mStopname);
						mContext.startActivity(bustimes);
						return true;
					} else if (velocityX > 100 && Math.abs(velocityX) > Math.abs(velocityY)) {
						// Log.d(TAG, "right fling detected");
                        GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                                .setCategory(mContext.getLocalClassName())
                                .setAction("fling right")
                                .build());
						finish();
						return true;
					}
					return false;
				}
			});

    // Catch the user selecting the map option from the navigation drawer,
    // and show the map with this stop centered.
    @Override
	public boolean onNavOptionSelected(int itemid) {
		switch (itemid) {
            case R.id.menu_showmap: {
            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory(mContext.getLocalClassName())
                    .setAction("Show stop")
                    .setLabel(mStopid)
                    .build());
			final String pkgstr = mContext.getApplicationContext().getPackageName();
			final Intent busstop = new Intent(mContext, StopsActivity.class);
			busstop.putExtra(pkgstr + ".stop_id", mStopid);
            busstop.putExtra(pkgstr + ".stop_name", mStopname);
			startActivity(busstop);
			return true;
		}
		default: {
			return false;
		}
		}
	}

	/* Do the processing to load the ArrayAdapter for display. */
	private class ProcessRoutes extends AsyncTask<Void, Integer, Integer> {
		// static final String TAG = "ProcessRoutes";

		@Override
		protected void onPreExecute() {
            setProgressBarVisibility(true);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			setProgress(parms[0]);
		}

		@Override
		protected Integer doInBackground(Void... foo) {
			// Log.v(TAG, "doInBackground()");

			publishProgress(2500); // fake it

			// Find which routes use the given stop.
			// Only show bus routes where the schedule is valid for the current date
			mCsr = ServiceCalendar.getRoutesUsingStop(mStopid);

			publishProgress(5000); // fake it

			publishProgress(7500); // fake it
			return mCsr.getCount();
		}

		@Override
		protected void onPostExecute(Integer listcount)
        {

			final ListView lv = (ListView)findViewById(android.R.id.list);
			lv.setOnTouchListener(mGestureListener);

			if (listcount > 1) {
				// Show msg describing a fling to see times for all routes.
				final TextView tv = new TextView(mContext);
				tv.setText(R.string.route_fling);
				lv.addFooterView(tv);
			} else if (listcount == 0) {
				final TextView tv = new TextView(mContext);
				tv.setText(R.string.stop_unused);
				lv.addFooterView(tv);
			}

			lv.setAdapter(new ListCursorAdapter(mContext, R.layout.route_numanddesclayout, mCsr));

            getActionBar().setTitle("Routes using stop " + mStopid);
            getActionBar().setSubtitle(mStopname);
            setProgress(10000); // max -- makes it slide away
		}
	}
}
