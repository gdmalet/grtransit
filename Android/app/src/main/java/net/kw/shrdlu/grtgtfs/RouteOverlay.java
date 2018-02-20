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

import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Color;
import android.util.Log;

import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.LatLngBounds;
import com.google.android.gms.maps.model.Polyline;
import com.google.android.gms.maps.model.PolylineOptions;

import java.util.zip.Adler32;

public class RouteOverlay {
	private static final String TAG = "BusrouteOverlay";

	private int mCount;
	private LatLng[] mPoints = null;
	private int mColourDiff = 0;
	private LatLngBounds mBoundingBox;
    private PolylineOptions mRoutePolyOptions = null;

	public RouteOverlay(Context context, String route, String headsign) {
		super();
		// Log.v(TAG, "starting RouteOverlay");

		// Try get different colours for different routes
		final Adler32 chksum = new Adler32();
		chksum.update((route + headsign).getBytes());
		mColourDiff = ((int)chksum.getValue() & 0x00FFFFFF) | 0xFF000000; // opacity is the high bits

		final String table = "shapes";
		final String[] columns = { "shape_pt_lat", "shape_pt_lon" };
		final String whereclause = "shape_id = (select shape_id from trips where route_id = ? and trip_headsign = ?)";
		final String[] selectargs = { route, headsign };
		final String orderby = "cast(shape_pt_sequence as integer)";

		Cursor csr;
		try {
			csr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, orderby);
		} catch (final SQLException e) {
			Log.e(TAG, "DB query failed: " + e.getMessage());
			return;
		}

		csr.moveToPosition(0);
		mCount = csr.getCount();
		mPoints = new LatLng[mCount];

		// Going to track the edges
		LatLngBounds.Builder boundsbuilder = new LatLngBounds.Builder();

		for (int i = 0; i < mCount; i++) {
			final double stop_lat = csr.getDouble(0);
			final double stop_lon = csr.getDouble(1);

			mPoints[i] = new LatLng(stop_lat, stop_lon);

			boundsbuilder.include(mPoints[i]);

			csr.moveToNext();
		}
		csr.close();

		// Stash values needed for later calls
		mBoundingBox = boundsbuilder.build();

        mRoutePolyOptions = new PolylineOptions()
                .add(mPoints)
                .width(20)
                .color(mColourDiff);

		// Log.v(TAG, "ending RouteOverlay");
	}

	public LatLngBounds getBoundingBox()
	{
		return mBoundingBox;
	}

    public PolylineOptions getRoutePolyOptions()
    {
        return mRoutePolyOptions;
    }
//		paint.setARGB(255, 96 + mColourDiff, 128 - mColourDiff / 4, 128 + mColourDiff / 2);
}
