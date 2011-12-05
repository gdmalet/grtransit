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
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.util.Log;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapView;
import com.google.android.maps.Overlay;
import com.google.android.maps.Projection;

public class RouteOverlay extends Overlay {
	private static final String TAG = "BusrouteOverlay";
	
	private int mCount;
	private int [] mPoints = null;

	private int mLonSpan, mLatSpan;
	private GeoPoint mCenter;

	public RouteOverlay(Context context, String route, String headsign) {
		super();
//		Log.v(TAG, "starting RouteOverlay");
		
		final String table = "shapes";
		final String [] columns = {"shape_pt_lat", "shape_pt_lon"};
		final String whereclause = "shape_id = (select shape_id from trips where route_id = ? and trip_headsign = ?)";
		String [] selectargs = {route, headsign};
		final String orderby = "cast(shape_pt_sequence as integer)";
		
    	Cursor csr;
    	try {
    		csr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null,null, orderby);
    	} catch (SQLException e) {
    		Log.e(TAG, "DB query failed: " + e.getMessage());
    		return;
    	}

    	// TODO -- caching points in an array -- perhaps better to just cache the
    	// cursor, and repeat the query on draw()?
        csr.moveToPosition(0);
        mCount = csr.getCount();
   		mPoints = new int[mCount*2];
   		
		// Going to calculate our centre
		int min_lat = 360000000, min_lon = 360000000, max_lat = -360000000, max_lon = -360000000;

   		for (int i=0; i<mCount; i++) {
   			int stop_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
   			int stop_lon = (int)(csr.getFloat(1) * 1000000);

   			mPoints[i*2] = stop_lat;
   			mPoints[(i*2)+1] = stop_lon;
   			
			if (stop_lat < min_lat)
				min_lat = stop_lat;
			if (stop_lat > max_lat)
				max_lat = stop_lat;
			if (stop_lon < min_lon)
				min_lon = stop_lon;
			if (stop_lon > max_lon)
				max_lon = stop_lon;

 			csr.moveToNext();
   		}
   		csr.close();
   		
		// Stash some values needed for later calls
		mLatSpan = max_lat - min_lat;
		mLonSpan = max_lon - min_lon;
		mCenter = new GeoPoint(min_lat + mLatSpan/2, min_lon + mLonSpan/2);

//		Log.v(TAG, "ending RouteOverlay");
	}
		
	// Seeing we don't store all points in the overlay, we need to provide our own
	// span values, since the overlay has no clue of what we're doing.
	public int getLonSpanE6() {
		return mLonSpan;
	}
	public int getLatSpanE6() {
		return mLatSpan;
	}
	public GeoPoint getCenter() {
		return mCenter;
	}
	
	@Override
	public void draw(Canvas canvas, MapView view, boolean shadow) {
		super.draw(canvas, view, shadow);
//		Log.v(TAG, "draw " + shadow);
		
		if (shadow || mPoints == null || mCount <= 0)
			return;
		
		// Convert geo points to points on the canvas
		Projection proj = view.getProjection();
		Point pt_scr = new Point();
		Path path = new Path();

		proj.toPixels(new GeoPoint(mPoints[0], mPoints[1]), pt_scr);
		path.moveTo(pt_scr.x, pt_scr.y);

		for (int i=1; i<mCount; i++) {
			proj.toPixels(new GeoPoint(mPoints[i*2], mPoints[(i*2)+1]), pt_scr);
	  		path.lineTo(pt_scr.x, pt_scr.y);
		}

		Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
		paint.setARGB(192, 224, 64, 32);
		paint.setStyle(Paint.Style.STROKE);
		paint.setStrokeWidth(5);
		canvas.drawPath(path, paint);

//		Log.v(TAG, "draw exit");
	}
}
