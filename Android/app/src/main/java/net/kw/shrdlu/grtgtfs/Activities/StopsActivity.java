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
import android.graphics.Rect;
import android.location.Location;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.Window;
import android.widget.Toast;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapController;

import net.kw.shrdlu.grtgtfs.DatabaseHelper;
import net.kw.shrdlu.grtgtfs.NotificationCallback;
import net.kw.shrdlu.grtgtfs.R;

import java.util.ArrayList;

public class StopsActivity extends MenuMapActivity {
	private static final String TAG = "StopsActivity";

	private String mStopId;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;

        // Will use the action bar progress bar
        requestWindowFeature(Window.FEATURE_PROGRESS);

        super.onCreate(savedInstanceState);

		// See if we're entering as a result of a search. Show given stop if so,
		// else will try show current location.
		final String stopstr = mContext.getApplicationContext().getPackageName() + ".stop_id";
		final Intent intent = getIntent();
		mStopId = intent.getStringExtra(stopstr);

		// Get the busstop overlay set up in the background
		new LoadOverlay().execute();
	}

// TODO -- not working
//   	@Override
//	public boolean onPrepareOptionsMenu(Menu menu)
//    {
//
//        for (int i = 0; i < mDrawerItems.size(); i++) {
//            NavDrawerItem item = mDrawerItems.get(i);
//            if (item.getId() == R.id.menu_showmap) {
//                item.setId(R.string.mylocation);
//                mNavAdapter.notifyDataSetChanged();
//                menu.clear();
//                break;
//            }
//        }
//
//        boolean ret = super.onPrepareOptionsMenu(menu);
//        return ret;
//	}

	/**
	 * Background task to handle initial load of the bus stops.
     * When finished, it transitions back to the GUI thread where it
	 * updates with the newly-found entries.
	 */
	private class LoadOverlay extends AsyncTask<Void, Integer, Void> implements NotificationCallback {

		// A callback from LoadDB, for updating our progress bar
		@Override
		public void notificationCallback(Integer progress) {
			publishProgress(progress);
		}

		@Override
		protected void onPreExecute() {
            setProgressBarVisibility(true);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
            setProgress(parms[0]);
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
		 * When finished, add markers to the map.
		 */
		@Override
		protected void onPostExecute(Void foo) {
			// Log.v(TAG, "onPostExecute()");

			// Put all the markers on the map.
            Marker savedMarker = null;
			for (final MarkerOptions markeropt : mStopsOverlay.getStops()) {
				final Marker stopMarker = mMap.addMarker(markeropt);
                if (mStopId != null && stopMarker.getTitle().equals(mStopId)) {
                    savedMarker = stopMarker;
                }
			}

			// Centre the map over given bus stop, else location, else the whole area
			if (mStopId == null) {
				final Location center = mLastLocation;
				if (center != null) {
					final LatLng ll = new LatLng(center.getLatitude(), center.getLongitude());
					mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(ll, 17.0f));
				} else {
					Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
					final LatLngBounds boundingbox = mStopsOverlay.getBoundingBox();
					mMap.moveCamera(CameraUpdateFactory.newLatLngBounds(boundingbox, 32));
				}
			} else {
				final String table = "stops", where = "stop_id = ?";
				final String[] columns = { "stop_lat", "stop_lon" }, selectargs = { mStopId };
				final Cursor locn = DatabaseHelper.ReadableDB().query(table, columns, where, selectargs, null, null, null);
				if (locn.moveToFirst()) {
					final double stop_lat = locn.getDouble(0);
					final double stop_lon = locn.getDouble(1);
					final LatLng ll = new LatLng(stop_lat, stop_lon);
					mMap.moveCamera(CameraUpdateFactory.newLatLngZoom(ll, 18.0f));
                    if (savedMarker != null) {
                        savedMarker.showInfoWindow();
                    }
				}
				locn.close();
			}

            getActionBar().setTitle(R.string.title_mapstops);
            getActionBar().setSubtitle(null);
            setProgress(10000); // max -- makes it slide away
		}
	}
}
