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
import android.location.Location;
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

public class MenuMapActivity extends MapActivity implements AnimationListener {
	private static final String TAG = "MenuMapActivity";

	protected MapActivity mContext;
	protected View mDetailArea;
	protected TextView mTitle;
	protected Animation mSlideIn, mSlideOut;
	protected ProgressBar mProgress;
	protected MapView mMapview;
	protected List<Overlay> mapOverlays;
	protected MyLocationOverlay mMylocation;
	protected StopsOverlay mStopsOverlay = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.mapview);

		mMapview = (MapView) findViewById(R.id.mapview);
		mMapview.setBuiltInZoomControls(true);

		mapOverlays = mMapview.getOverlays();

		// Load animations used to show/hide progress bar
		mProgress = (ProgressBar) findViewById(R.id.progress);
		mDetailArea = findViewById(R.id.mapview);
		mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);

		mTitle = (TextView) findViewById(R.id.listtitle);
		mTitle.setText(R.string.loading_stops);

		mMylocation = new MyLocationOverlay(this, mMapview);
		mapOverlays.add(mMylocation);

		mStopsOverlay = new StopsOverlay(mContext);
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

	// Called when a button is clicked on the title bar
	public void onTitlebarClick(View v) {
		TitlebarClick.onTitlebarClick(mContext, v);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.busstopsmenu, menu);

		// Twiddle menu options
		menu.removeItem(R.id.menu_about);
		menu.removeItem(R.id.menu_preferences);
		menu.findItem(R.id.menu_showmap).setTitle(R.string.mylocation);

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_showmap: {
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
		default: {
			return TitlebarClick.onOptionsItemSelected(mContext, item);
		}
		}
	}

	@Override
	public void onAnimationEnd(Animation animation) {
	}

	@Override
	public void onAnimationRepeat(Animation animation) {
	}

	@Override
	public void onAnimationStart(Animation animation) {
	}

	@Override
	protected boolean isRouteDisplayed() {
		return false;
	}

}
