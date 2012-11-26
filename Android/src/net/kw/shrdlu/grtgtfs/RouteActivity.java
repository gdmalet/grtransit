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
import java.util.List;

import android.content.Intent;
import android.database.Cursor;
import android.graphics.Rect;
import android.location.Location;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;

public class RouteActivity extends MapActivity implements AnimationListener {
	private static final String TAG = "BusroutesActivity";

	private MapActivity mContext;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
	private View mDetailArea;
	private TextView mTitle;
	private Animation mSlideIn, mSlideOut;
	private ProgressBar mProgress;
	private MyLocationOverlay mMylocation;
	private String mRoute_id, mHeadsign, mStop_id;
	private StopsOverlay mStopsOverlay = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mContext = this;
		setContentView(R.layout.mapview);

		mMapview = (MapView) findViewById(R.id.mapview);
		mMapview.setBuiltInZoomControls(true);

		mapOverlays = mMapview.getOverlays();

		final String pkgstr = mContext.getApplicationContext().getPackageName();
		final Intent intent = getIntent();
		mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
		mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
		mStop_id = intent.getStringExtra(pkgstr + ".stop_id"); // TODO show position?

		// Load animation used to hide progress bar
		mProgress = (ProgressBar) findViewById(R.id.map_progress);
		mDetailArea = findViewById(R.id.mapview);
		mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);
		mTitle = (TextView) findViewById(R.id.title);

		mMylocation = new MyLocationOverlay(this, mMapview);
		mapOverlays.add(mMylocation);

		// Get the busstop overlay set up in the background
		mStopsOverlay = new StopsOverlay(mContext);
		new PrepareOverlays().execute();
	}

	@Override
	public void onResume() {
		super.onResume();

		// We want to track a pageView every time this Activity gets the focus.
		Globals.tracker.trackPageView("/" + this.getLocalClassName());

		mMylocation.enableMyLocation();
		mMylocation.enableCompass();
	}

	@Override
	public void onPause() {
		super.onPause();
		// Log.d(TAG, "onPause()");
		mMylocation.disableMyLocation();
		mMylocation.disableCompass();
	}

	@Override
	protected boolean isRouteDisplayed() {
		return false;
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.busstopsmenu, menu);

		// Hide some menu options
		menu.removeItem(R.id.menu_about);
		menu.removeItem(R.id.menu_preferences);

		return true;
	}

	/* This is called when redisplaying the menu
	 * 
	 * @Override public boolean onPrepareOptionsMenu(Menu menu) { return true; } */
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_mylocation: {
			// Center the map over the current location
			GeoPoint locn = mMylocation.getMyLocation();
			if (locn == null) {
				final Location l = mMylocation.getLastFix();
				if (l != null) {
					Toast.makeText(mContext, R.string.last_location_fix, Toast.LENGTH_LONG).show();
					locn = new GeoPoint((int) (l.getLatitude() * 1000000), (int) (l.getLongitude() * 1000000));
				}
			}
			if (locn != null) {
				final MapController mcp = mMapview.getController();
				mcp.animateTo(locn);
				while (mMapview.getZoomLevel() < 17) {
					if (!mcp.zoomIn()) {
						break;
					}
				}
			} else {
				Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
			}
			return true;
		}
		case R.id.menu_closeststops: {
			Globals.tracker.trackEvent("Menu", "Closest stops", "", 1);
			final Intent stops = new Intent(mContext, ClosestStopsActivity.class);
			startActivity(stops);
			return true;
		}
		// case R.id.menu_searchstops: {
		// final Intent stopsearch = new Intent(mContext, SearchStopsActivity.class);
		// stopsearch.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
		// startActivity(stopsearch);
		// return true;
		// }
		// case R.id.menu_searchroutes: {
		// onSearchRequested();
		// return true;
		// }
		}
		return super.onOptionsItemSelected(item);
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
				final String qry = "select distinct route_id, trip_headsign from trips"
						+ " join calendar on trips.service_id = calendar.service_id where "
						+ " trip_id in (select trip_id from stop_times where stop_id = ?) and "
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
				mTitle.setText("Rt " + mRoute_id + " - " + mHeadsign);
			} else {
				mTitle.setText("Routes using stop " + mStop_id);
			}
		}
	}

	/**
	 * Make the {@link ProgressBar} visible when our in-animation finishes.
	 */
	@Override
	public void onAnimationEnd(Animation animation) {
	}

	@Override
	public void onAnimationRepeat(Animation animation) {
		// Not interested if the animation repeats
	}

	@Override
	public void onAnimationStart(Animation animation) {
		// Not interested when the animation starts
	}
}
