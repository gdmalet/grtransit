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

import android.content.Intent;
import android.database.Cursor;
import android.graphics.Rect;
import android.location.Location;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.Toast;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapController;

public class StopsActivity extends MenuMapActivity {
	private static final String TAG = "StopsActivity";

	private String mStopId;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;
		super.onCreate(savedInstanceState);

		// See if we're entering as a result of a search. Show given stop if so,
		// else will try show current location.
		final String stopstr = mContext.getApplicationContext().getPackageName() + ".stop_id";
		final Intent intent = getIntent();
		mStopId = intent.getStringExtra(stopstr);

		if (mStopId != null && mStopId.equals("2040")) {
			Toast.makeText(mContext, "Aaaaarrrrr!", Toast.LENGTH_LONG).show();
		}

		// Get the busstop overlay set up in the background
		new LoadOverlay().execute();
	}

	/**
	 * Background task to handle initial load of the bus stops. This correctly shows and hides the loading animation from the
	 * GUI thread before starting a background query to the DB. When finished, it transitions back to the GUI thread where it
	 * updates with the newly-found entries.
	 */
	private class LoadOverlay extends AsyncTask<Void, Integer, Void> implements NotificationCallback {

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
		protected Void doInBackground(Void... foo) {
			mStopsOverlay.LoadDB(null, null, this);
			return null;
		}

		/**
		 * When finished, link in the new overlay.
		 */
		@Override
		protected void onPostExecute(Void foo) {
			// Log.v(TAG, "onPostExecute()");

			mapOverlays.add(mStopsOverlay);

			// Centre the map over given bus stop, else location, else the whole
			// area
			final MapController mcp = mMapview.getController();
			GeoPoint center;
			if (mStopId == null) {
				center = mMylocation.getMyLocation();
				if (center == null) {
					final Location l = mMylocation.getLastFix();
					if (l != null) {
						Toast.makeText(mContext, R.string.last_location_fix, Toast.LENGTH_LONG).show();
						center = new GeoPoint((int) (l.getLatitude() * 1000000), (int) (l.getLongitude() * 1000000));
					}
				}
				if (center != null) {
					mcp.animateTo(center);
					while (mMapview.getZoomLevel() < 17) {
						if (!mcp.zoomIn()) {
							break;
						}
					}
				} else {
					Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
					final Rect boundingbox = mStopsOverlay.getBoundingBoxE6();
					mcp.setCenter(new GeoPoint(boundingbox.centerX(), boundingbox.centerY()));
					mcp.zoomToSpan(boundingbox.right - boundingbox.left, boundingbox.bottom - boundingbox.top);
				}
			} else {
				final String table = "stops", where = "stop_id = ?";
				final String[] columns = { "stop_lat", "stop_lon" }, selectargs = { mStopId };
				final Cursor locn = DatabaseHelper.ReadableDB().query(table, columns, where, selectargs, null, null, null);
				if (locn.moveToFirst()) {
					final int stop_lat = (int) (locn.getFloat(0) * 1000000); // microdegrees
					final int stop_lon = (int) (locn.getFloat(1) * 1000000);
					center = new GeoPoint(stop_lat, stop_lon);
					mcp.setCenter(center);
					mcp.setZoom(19);
				}
				locn.close();
			}

			mProgress.setVisibility(View.INVISIBLE);
			mDetailArea.startAnimation(mSlideOut);
			mTitle.setText(R.string.title_mapstops);
		}
	}
}
