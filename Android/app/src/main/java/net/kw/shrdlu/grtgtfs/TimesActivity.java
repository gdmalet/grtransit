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

import android.app.AlertDialog;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.view.GestureDetector;
import android.view.Gravity;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.gms.analytics.HitBuilders;

import net.kw.shrdlu.grtgtfs.LayoutAdapters.ListArrayAdapter;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.TimesArrayAdapter;

import java.util.ArrayList;

public class TimesActivity extends MenuListActivity {
	private static final String TAG = "TimesActivity";

	private String mRouteid = null, mHeadsign, mStopid, mStopname;
	private ArrayList<String[]> mListDetails = null;
	private boolean PrefChanged = true; // force redraw

	private static boolean mCalendarChecked = false, mCalendarOK;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;

        // Will use the action bar progress bar
        requestWindowFeature(Window.FEATURE_PROGRESS);

        setContentView(R.layout.timeslayout);
		super.onCreate(savedInstanceState);

		final String pkgstr = mContext.getApplicationContext().getPackageName();
		final Intent intent = getIntent();
		mRouteid = intent.getStringExtra(pkgstr + ".route_id");
		mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
		mStopid = intent.getStringExtra(pkgstr + ".stop_id");
        mStopname = intent.getStringExtra(pkgstr + ".stop_name");
	}

	@Override
	protected void onResume() {
		super.onResume();

		// See if we need to recalculate and redraw the screen.
		// This happens if the user brings up the preferences screen.
		if (PrefChanged) {
			new ProcessBusTimes().execute();
			PrefChanged = false;
		}
	}

	/* Do the processing to load the ArrayAdapter for display. */
	private class ProcessBusTimes extends AsyncTask<Void, Integer, Integer> implements NotificationCallback {
		static final String TAG = "";

		// TODO -- should set a listener that will call this callback.

		// A callback from CalendarService, for updating our progress bar
		@Override
		public void notificationCallback(Integer progress) {
			publishProgress(progress);
		}

		@Override
		protected void onPreExecute() {
			// Log.v(TAG, "onPreExecute()");
			mListDetail.startAnimation(mSlideIn);
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

			// Will find where to position the list of bus departure times
			final Time t = new Time();
			t.setToNow();
			final String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);
			final String datenow = String.format("%04d%02d%02d", t.year, t.month + 1, t.monthDay);

			// Make sure we actually have some valid data, since schedules change often.
			if (!mCalendarChecked) {
				mCalendarOK = CheckCalendar(datenow);
			}
			if (!mCalendarOK) {
				return null;
			}

			if (mRouteid == null) {
				// showing all routes
				mListDetails = ServiceCalendar.getRouteDepartureTimes(mStopid, datenow,
						!GRTApplication.mPreferences.showAllBusses(), this);
			} else {

				// TODO Setting a listener means not passing `this'

				// showing just one route
				mListDetails = ServiceCalendar.getRouteDepartureTimes(mStopid, mRouteid, mHeadsign, datenow,
						!GRTApplication.mPreferences.showAllBusses(), this);
			}

			// Find when the next bus leaves
			int savedpos = -1;
			for (int i = 0; i < mListDetails.size(); i++) {
				final String departure_time = mListDetails.get(i)[0];
				if (departure_time.compareTo(timenow) >= 0) {
					savedpos = i;
					break;
				}
			}

			return savedpos;
		}

		@Override
		protected void onPostExecute(Integer savedpos) {
			// Log.v(TAG, "onPostExecute()");

			if (!mCalendarOK) {
				final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
				builder.setIcon(R.drawable.grticon);
				builder.setTitle(R.string.app_name);
				builder.setMessage(R.string.calendar_expired);
				builder.create();
				builder.show();
				return;
			}

            setProgress(10000); // max -- makes it slide away
			mListDetail.startAnimation(mSlideOut);

			TextView tv = null;
			final ListView lv = getListView();
			if (lv.getFooterViewsCount() == 0) {
				tv = new TextView(mContext);
				lv.addFooterView(tv);
			}
			lv.setOnTouchListener(mGestureListener);

            getActionBar().setTitle("Stop " + mStopid + " " + mStopname);
			if (mRouteid == null) { // showing all routes
                getActionBar().setSubtitle("All routes");
				if (tv != null) {
					tv.setText(R.string.tap_time_for_route);
				}
				final TimesArrayAdapter adapter = new TimesArrayAdapter(mContext, R.layout.row2layout, mListDetails);
				mContext.setListAdapter(adapter);
			} else {
                getActionBar().setSubtitle("Route " + mHeadsign);
				if (tv != null) {
					tv.setText(R.string.tap_time_for_trip);
				}
				final ListArrayAdapter adapter = new ListArrayAdapter(mContext, R.layout.rowlayout, mListDetails);
				mContext.setListAdapter(adapter);
			}

			// Calculate the time difference
			Toast msg;
			if (savedpos >= 0) {
				final Time t = new Time();
				t.setToNow();

				final String nextdeparture = mListDetails.get(savedpos)[0];
				int hourdiff = Integer.parseInt(nextdeparture.substring(0, 2));
				hourdiff -= t.hour;
				hourdiff *= 60;
				int mindiff = Integer.parseInt(nextdeparture.substring(3, 5));
				mindiff -= t.minute;
				hourdiff += mindiff;

				if (hourdiff >= 60) {
					msg = Toast.makeText(mContext, "Next bus leaves at " + ServiceCalendar.formattedTime(nextdeparture),
							Toast.LENGTH_LONG);
				} else {
					final String plural = hourdiff > 1 ? "s" : "";
					msg = Toast.makeText(mContext, "Next bus leaves in " + hourdiff + " minute" + plural, Toast.LENGTH_LONG);
				}

				getListView().setSelectionFromTop(savedpos, 50); // position next bus just below top

			} else {
				setSelection(mListDetails.size()); // position the list at the last bus
				msg = Toast.makeText(mContext, R.string.no_more_busses, Toast.LENGTH_LONG);
			}

			msg.setGravity(Gravity.TOP, 0, 0);
			msg.show();
		}
	}

	/* Make sure the calendar is current. Updates mCalendarChecked if we get a result of some sort. */
	private boolean CheckCalendar(String datenow) {
		boolean retval = true; // report OK even if failure, so we just continue
		final String[] selectargs = { datenow, datenow };
		Cursor csr = null;

		try {
			csr = DatabaseHelper.ReadableDB().rawQuery(
					"select count(*) from calendar where " + "start_date <= ? and end_date >= ?", selectargs);
		} catch (final SQLException e) {
			Log.e(TAG, "DB query failed checking calendar expiry: " + e.getMessage());
		}

		if (csr != null) {
			if (csr.getCount() == 0 || !csr.moveToFirst() || csr.getInt(0) <= 0) {
				retval = false;
			}

			mCalendarChecked = true;
			csr.close();
		}

		return retval;
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		// Log.v(TAG, "clicked position " + position);

		final String[] items = (String[]) l.getAdapter().getItem(position);
		if (items == null) {
			return;
		}

		// Allow narrowing to one route, if we're showing many.
		if (mRouteid == null) { // showing all routes
			final String route_id = items[2];
			final String headsign = items[3];

			final Intent bustimes = new Intent(mContext, TimesActivity.class);
			final String pkgstr = mContext.getApplicationContext().getPackageName();
			bustimes.putExtra(pkgstr + ".route_id", route_id);
			bustimes.putExtra(pkgstr + ".headsign", headsign);
			bustimes.putExtra(pkgstr + ".stop_id", mStopid);
            bustimes.putExtra(pkgstr + ".stop_name", mStopname);
			mContext.startActivity(bustimes);

		} else { // 1 route
			final String trip_id = items[2];

			final Intent tripstops = new Intent(mContext, TripStopsActivity.class);
			final String pkgstr = mContext.getApplicationContext().getPackageName();
			tripstops.putExtra(pkgstr + ".trip_id", trip_id);
			tripstops.putExtra(pkgstr + ".stop_id", mStopid);
            tripstops.putExtra(pkgstr + ".stop_name", mStopname);
			tripstops.putExtra(pkgstr + ".route_id", mRouteid);
			tripstops.putExtra(pkgstr + ".headsign", mHeadsign);
			mContext.startActivity(tripstops);

		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
            case R.id.menu_showmap: {
            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory(mContext.getLocalClassName())
                    .setAction("Menu show route")
                    .setLabel(mRouteid == null ? "All" : mRouteid + " - " + mHeadsign)
                    .build());

			// Perform action on click
			final String pkgstr = mContext.getApplicationContext().getPackageName();
			final Intent busroutes = new Intent(mContext, RouteActivity.class);
			busroutes.putExtra(pkgstr + ".route_id", mRouteid);
			busroutes.putExtra(pkgstr + ".headsign", mHeadsign);
			busroutes.putExtra(pkgstr + ".stop_id", mStopid);
            busroutes.putExtra(pkgstr + ".stop_name", mStopname);
			startActivity(busroutes);
			return true;
		}
		default: {
			return TitlebarClick.onOptionsItemSelected(mContext, item.getItemId());
		}
		}
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
					// Catch a fling sort of from left to right
					if (velocityX > 100 && Math.abs(velocityX) > Math.abs(velocityY)) {
						// Log.d(TAG, "fling detected");
                        GRTApplication.tracker.send(new HitBuilders.EventBuilder(mContext.getLocalClassName(), "fling left").build());
						finish();
						return true;
					}
					return false;
				}
			});
}
