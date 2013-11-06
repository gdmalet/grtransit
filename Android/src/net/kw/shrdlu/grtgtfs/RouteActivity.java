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

import java.util.ArrayList;

import android.content.Intent;
import android.database.Cursor;
import android.graphics.Rect;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.view.View;
import android.widget.ProgressBar;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapController;

public class RouteActivity extends MenuMapActivity {
	private static final String TAG = "RouteActivity";

	private String mRoute_id, mHeadsign, mStop_id;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;
		super.onCreate(savedInstanceState);

		final String pkgstr = mContext.getApplicationContext().getPackageName();
		final Intent intent = getIntent();
		mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
		mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
		mStop_id = intent.getStringExtra(pkgstr + ".stop_id"); // TODO show position?

		// Get the busstop overlay set up in the background
		new PrepareOverlays().execute();
	}

	/**
	 * Background task to handle initial load of the overlays.
	 */
	private class PrepareOverlays extends AsyncTask<Void, Integer, ArrayList<RouteOverlay>> implements NotificationCallback {
		// static final String TAG = "LookupTask";

		// A callback from LoadDB, for updating our progress bar
		@Override
		public void notificationCallback(Integer progress) {
			publishProgress(progress);
		}

		/**
		 * Before jumping into background thread, start sliding in the {@link ProgressBar}. We'll only show it once the
		 * animation finishes.
		 */
		@Override
		protected void onPreExecute() {
			// Log.v(TAG, "onPreExecute()");
			mDetailArea.startAnimation(mSlideIn);
			mProgress.setVisibility(View.VISIBLE);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
		}

		/**
		 * Perform the background query.
		 */
		@Override
		protected ArrayList<RouteOverlay> doInBackground(Void... foo) {
			// Log.v(TAG, "doInBackground()");

			final ArrayList<RouteOverlay> overlays = new ArrayList<RouteOverlay>(16);

			if (mRoute_id != null) { // doing one route
				// final String whereclause = "stop_id in "
				// + "(select stop_id from stop_times where trip_id = "
				// + "(select trip_id from trips where route_id = ? and trip_headsign = ?))";
				// final String [] selectargs = {mRoute_id, mHeadsign};

				// It's too slow to fish out these stops, so for now show them all
				// mStopsOverlay.LoadDB(whereclause, selectargs, this);
				mStopsOverlay.LoadDB(null, null, this);

				// Now draw the route
				final RouteOverlay routeoverlay = new RouteOverlay(mContext, mRoute_id, mHeadsign);
				overlays.add(routeoverlay);

			} else {
				// doing many routes
				// final String whereclause = "stop_id in "
				// + "(select distinct stop_id from stop_times where trip_id in "
				// + "(select trip_id from stop_times where stop_id = ?))";
				// String [] selectargs = {mStop_id};

				// It's too slow to fish out these stops, so for now show them all
				// mBusstopsOverlay.LoadDB(whereclause, selectargs, this);
				mStopsOverlay.LoadDB(null, null, this);

				// Now draw the routes - taken from RouteselectActivity
				final Time t = new Time(); // TODO - this duplicates BusTimes?
				t.setToNow();
				final String datenow = String.format("%04d%02d%02d", t.year, t.month + 1, t.monthDay);
				final String qry = "select distinct routes.route_short_name, trip_headsign from trips"
						+ " join routes on routes.route_id = trips.route_id"
						+ " join calendar on trips.service_id = calendar.service_id where"
						+ " trip_id in (select trip_id from stop_times where stop_id = ?) and"
						+ " start_date <= ? and end_date >= ?";
				final String[] selectargs = new String[] { mStop_id, datenow, datenow };
				final Cursor csr = DatabaseHelper.ReadableDB().rawQuery(qry, selectargs);

				final int maxcount = csr.getCount();
				int progresscount = 0;
				boolean more = csr.moveToPosition(0);
				while (more) {
					final RouteOverlay routeoverlay = new RouteOverlay(mContext, csr.getString(0), csr.getString(1));
					overlays.add(routeoverlay);
					more = csr.moveToNext();
					publishProgress(((int) ((++progresscount / (float) maxcount) * 100)));
				}
				csr.close();
			}

			return overlays;
		}

		/**
		 * When finished, link in the new overlay.
		 */
		@Override
		protected void onPostExecute(ArrayList<RouteOverlay> overlays) {
			// Log.v(TAG, "onPostExecute()");

			// Overlays must be added on the GIU thread
			mapOverlays.add(mStopsOverlay);

			Rect boundingbox = null;

			// Need to calculate span and centre of overlays
			for (final RouteOverlay overlay : overlays) {
				mapOverlays.add(overlay);
				if (boundingbox == null) {
					boundingbox = overlay.getBoundingBoxE6();
				} else {
					boundingbox.union(overlay.getBoundingBoxE6());
				}
			}

			// Centre the map over the bus stops
			final MapController mcp = mMapview.getController();
			mcp.setCenter(new GeoPoint(boundingbox.centerX(), boundingbox.centerY()));
			mcp.zoomToSpan(boundingbox.right - boundingbox.left, boundingbox.bottom - boundingbox.top);

			mProgress.setVisibility(View.INVISIBLE);
			mDetailArea.startAnimation(mSlideOut);

			if (mRoute_id != null) { // doing one route
				// TODO should be route_short_name?
				mTitle.setText("Rt " + mRoute_id + " - " + mHeadsign);
			} else {
				mTitle.setText("Routes using stop " + mStop_id);
			}
		}
	}
}
