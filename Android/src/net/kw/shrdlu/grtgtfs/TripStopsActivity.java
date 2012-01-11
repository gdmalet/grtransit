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
import android.view.GestureDetector;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;

public class TripStopsActivity extends ListActivity implements AnimationListener {
	private static final String TAG = "TripStopsActivity";

	private ListActivity mContext;
	private View mListDetail;
	private Animation mSlideIn, mSlideOut;
	private ProgressBar mProgress;
	private String mTrip_id, mRoute_id = null, mHeadsign, mStop_id;
	private TextView mTitle;
	private Cursor mCsr;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// Log.v(TAG, "OnCreate()");

		mContext = this;

		setContentView(R.layout.timeslayout);

		// Load animations used to show/hide progress bar
		mProgress = (ProgressBar) findViewById(R.id.progress);
		mListDetail = findViewById(R.id.detail_area);
		mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);
		mTitle = (TextView) findViewById(R.id.timestitle);

		final String pkgstr = mContext.getApplicationContext().getPackageName();
		final Intent intent = getIntent();
		mTrip_id = intent.getStringExtra(pkgstr + ".trip_id");
		mStop_id = intent.getStringExtra(pkgstr + ".stop_id");
		mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
		mHeadsign = intent.getStringExtra(pkgstr + ".headsign");

		new ProcessBusStops().execute();
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this Activity gets the focus.
		Globals.tracker.trackPageView("/" + this.getLocalClassName());
	}

	/*
	 * Do the processing to load the ArrayAdapter for display.
	 */
	private class ProcessBusStops extends AsyncTask<Void, Integer, Integer> {
		static final String TAG = "ProcessBusStops";

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
		protected Integer doInBackground(Void... foo) {
			// Log.v(TAG, "doInBackground()");

			final ListView lv = getListView();
			lv.setOnTouchListener(mGestureListener);

			final String qry = "select stop_times.stop_id as _id, stop_name as descr, departure_time from stop_times"
					+ " join stops on stops.stop_id = stop_times.stop_id where trip_id = ? order by departure_time";
			final String[] selectargs = new String[] { mTrip_id };
			mCsr = DatabaseHelper.ReadableDB().rawQuery(qry, selectargs);
			startManagingCursor(mCsr);

			// Find where the current stop is, to position list
			final int maxcount = mCsr.getCount();
			int savedpos = -1;
			int progresscount = 0;
			boolean more = mCsr.moveToPosition(0);
			while (more) {
				if (mCsr.getString(0).equals(mStop_id)) {
					savedpos = progresscount;
					break;
				}
				more = mCsr.moveToNext();
				publishProgress(((int) ((++progresscount / (float) maxcount) * 100)));
			}

			return savedpos;
		}

		@Override
		protected void onPostExecute(Integer savedpos) {
			// Log.v(TAG, "onPostExecute()");

			mProgress.setVisibility(View.INVISIBLE);
			mListDetail.startAnimation(mSlideOut);

			mTitle.setText("Stops on route " + mRoute_id + ": " + mHeadsign);

			final ListCursorAdapter adapter = new ListCursorAdapter(mContext, R.layout.timestopdesc, mCsr);
			mContext.setListAdapter(adapter);

			getListView().setSelectionFromTop(savedpos, 50); // position stop just below top
		}
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		// Log.v(TAG, "clicked position " + position);

		final Cursor csr = (Cursor) l.getItemAtPosition(position);
		final String stop_id = csr.getString(0);

		final Intent bustimes = new Intent(mContext, TimesActivity.class);
		final String pkgstr = mContext.getApplicationContext().getPackageName();
		// bustimes.putExtra(pkgstr + ".route_id", mRoute_id);
		// bustimes.putExtra(pkgstr + ".headsign", mHeadsign);
		bustimes.putExtra(pkgstr + ".stop_id", stop_id);
		mContext.startActivity(bustimes);
	}

	// This is only called once....
	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.timesmenu, menu);
		return true;
	}

	// This is called when redisplaying the menu
	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		final boolean showingall = Globals.mPreferences.getShowAllBusses();
		MenuItem item = menu.findItem(R.id.menu_showallbusses);
		item.setEnabled(!showingall);
		item = menu.findItem(R.id.menu_showtodaysbusses);
		item.setEnabled(showingall);
		return true;
	}

	// TODO menus
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_showallbusses: {
			Globals.mPreferences.setShowAllBusses(true);
			// Just start again
			final Intent intent = getIntent();
			intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
			startActivity(intent);
			return true;
		}
		case R.id.menu_showtodaysbusses: {
			Globals.mPreferences.setShowAllBusses(false);
			final Intent intent = getIntent();
			intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
			startActivity(intent);
			return true;
		}
		case R.id.menu_showonmap: {
			Globals.tracker.trackEvent("Menu", "Show route", mRoute_id == null ? "All" : mRoute_id + " - " + mHeadsign, 1);
			// Perform action on click
			final String pkgstr = mContext.getApplicationContext().getPackageName();
			final Intent busroutes = new Intent(mContext, RouteActivity.class);
			busroutes.putExtra(pkgstr + ".route_id", mRoute_id);
			busroutes.putExtra(pkgstr + ".headsign", mHeadsign);
			busroutes.putExtra(pkgstr + ".stop_id", mStop_id);
			startActivity(busroutes);
		}
		}
		return super.onOptionsItemSelected(item);
	}

	private final View.OnTouchListener mGestureListener = new View.OnTouchListener() {
		public boolean onTouch(View v, MotionEvent event) {
			return mGestureDetector.onTouchEvent(event);
		}
	};

	// Catch flings, to show all busses coming to this stop.
	// This must be called on the GIU thread.
	private final GestureDetector mGestureDetector = new GestureDetector(mContext, new GestureDetector.SimpleOnGestureListener() {
		@Override
		public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
			// Log.d(TAG, "fling X " + velocityX + ", Y " + velocityY);
			// Catch a fling sort of from left to right
			if (velocityX > 100 && Math.abs(velocityX) > Math.abs(velocityY)) {
				// Log.d(TAG, "fling detected");
				Globals.tracker.trackEvent("TripStops", "fling left", "", 1);
				finish();
				return true;
			}
			return false;
		}
	});

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
