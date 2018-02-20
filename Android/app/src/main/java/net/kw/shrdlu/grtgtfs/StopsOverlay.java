/*
 * Copyright 2011 Giles Malet.
 *
 * This file is part of GRTransit.
 * 
 * GRTransit is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as ed by
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

import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.util.Log;

import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.MarkerOptions;

import java.util.ArrayList;

public class StopsOverlay {
	private static final String TAG = "StopsOverlay";

//	private final ArrayList mStopItems = new ArrayList<>(1);
	private LatLngBounds mBoundingBox;
	private static LatLngBounds mCachedBoundingBox;
	private String mStopid;
    private Context mContext = null;

	private ArrayList<MarkerOptions> mStops = new ArrayList<>(3000);
	private static ArrayList<MarkerOptions> mCachedStops = null;

   public StopsOverlay(Context context) {
       mContext = context;
   }


	// This is time consuming, and should not be called on the GUI thread
	public void LoadDB(String whereclause, String[] selectargs, NotificationCallback task) {
		// Log.d(TAG, "starting LoadDB");

		final String table = "stops";
		final String[] columns = { "stop_lat", "stop_lon", "stop_id", "stop_name" };

		if (whereclause == null && mCachedStops != null) {
			// Log.d(TAG, "using cached values");
			mStops = mCachedStops;
			mBoundingBox = mCachedBoundingBox;

		} else {

			// Log.d(TAG, "no cached values");

			Cursor csr;
			try {
				csr = DatabaseHelper.ReadableDB().query(true, table, columns, whereclause, selectargs, null, null, null, null);
			} catch (final SQLException e) {
				Log.e(TAG, "DB query failed: " + e.getMessage());
				return;
			}
			final int maxcount = csr.getCount();
			int progresscount = 0;

			// Going to track the edges
			LatLngBounds.Builder boundsbuilder = new LatLngBounds.Builder();

			boolean more = csr.moveToPosition(0);
			while (more) {
				final double stop_lat = csr.getDouble(0);
				final double stop_lon = csr.getDouble(1);

				final LatLng point = new LatLng(stop_lat, stop_lon);
                final MarkerOptions marker = new MarkerOptions()
                        .position(point)
                        .title(csr.getString(2))
                        .snippet(csr.getString(3))
						.icon(BitmapDescriptorFactory.fromResource(R.drawable.bluepin));
                mStops.add(marker);

				more = csr.moveToNext();

				boundsbuilder.include(point);

				if (++progresscount % 25 == 0) {
					task.notificationCallback((int) ((progresscount / (float) maxcount) * 10000));
				}
			}
			csr.close();

			// Stash values needed for later calls
			mBoundingBox = boundsbuilder.build();
		}

		if (whereclause == null && mCachedStops == null) {
			// Log.d(TAG, "priming cache");
			mCachedStops = mStops;
			mCachedBoundingBox = mBoundingBox;
		}

		// Log.d(TAG, "exiting LoadDB");
	}

    public ArrayList<MarkerOptions> getStopMarkerOptions()
	{
        return mStops;
    }

	public LatLngBounds getBoundingBox()
	{
		return mBoundingBox;
	}
}
