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

import java.util.List;

import android.content.Intent;
import android.database.Cursor;
import android.graphics.Rect;
import android.location.Location;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
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

public class StopsActivity extends MapActivity implements AnimationListener {
	private static final String TAG = "StopsActivity";

	private MapActivity mContext;
	private View mDetailArea;
	private TextView mTitle;
	private Animation mSlideIn, mSlideOut;
	private ProgressBar mProgress;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
	private MyLocationOverlay mMylocation;
	private String mStopId;
	private StopsOverlay mOverlay = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mContext = this;
		setContentView(R.layout.mapview);

		// Load animations used to show/hide progress bar
		mProgress = (ProgressBar) findViewById(R.id.progress);
		mDetailArea = findViewById(R.id.mapview);
		mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);

		mTitle = (TextView) findViewById(R.id.listtitle);
		mTitle.setText(R.string.loading_stops);

		mMapview = (MapView) findViewById(R.id.mapview);
		mMapview.setBuiltInZoomControls(true);

		mapOverlays = mMapview.getOverlays();

		mMylocation = new MyLocationOverlay(this, mMapview);
		mapOverlays.add(mMylocation);

		// See if we're entering as a result of a search. Show given stop if so,
		// else will try show current location.
		final String stopstr = mContext.getApplicationContext().getPackageName() + ".stop_id";
		final Intent intent = getIntent();
		mStopId = intent.getStringExtra(stopstr);

		if (mStopId != null && mStopId.equals("2040")) {
			Toast.makeText(mContext, "Aaaaarrrrr!", Toast.LENGTH_LONG).show();
		}

		// Get the busstop overlay set up in the background
		mOverlay = new StopsOverlay(mContext);
		new LoadOverlay().execute();
	}

	@Override
	public void onResume() {
		super.onResume();
		// We want to track a pageView every time this activity gets the focus - but if the activity was
		// previously destroyed we could have lost our global data, so this is a bit of a hack to avoid a crash!
		if (Globals.tracker == null) {
			Log.e(TAG, "null tracker!");
			startActivity(new Intent(this, FavstopsActivity.class));
		} else {
			Globals.tracker.trackPageView("/" + this.getLocalClassName());
		}

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

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_mylocation: {
			Globals.tracker.trackEvent("Menu", "My location", "", 1);
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
		case R.id.menu_preferences: {
			Globals.tracker.trackEvent("Menu", "Preferences", "", 1);
			final Intent prefs = new Intent(mContext, PrefsActivity.class);
			startActivity(prefs);
			return true;
		}
		case R.id.menu_closeststops: {
			Globals.tracker.trackEvent("Menu", "Closest stops", "", 1);
			final Intent stops = new Intent(mContext, ClosestStopsActivity.class);
			startActivity(stops);
			return true;
		}
		}
		return super.onOptionsItemSelected(item);
	}

	/**
	 * Background task to handle initial load of the bus stops. This correctly shows and hides the loading animation from the
	 * GUI thread before starting a background query to the DB. When finished, it transitions back to the GUI thread where it
	 * updates with the newly-found entries.
	 */
	private class LoadOverlay extends AsyncTask<Void, Integer, Void> implements NotificationCallback {
		static final String TAG = "LoadOverlay";

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
			mOverlay.LoadDB(null, null, this);
			return null;
		}

		/**
		 * When finished, link in the new overlay.
		 */
		@Override
		protected void onPostExecute(Void foo) {
			// Log.v(TAG, "onPostExecute()");

			mapOverlays.add(mOverlay);

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
					final Rect boundingbox = mOverlay.getBoundingBoxE6();
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

	// Called when a button is clicked on the title bar
	public void onTitlebarClick(View v) {
		TitlebarClick.onTitlebarClick(mContext, v);
	}
}
