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

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.util.Log;
import android.util.TimingLogger;
import android.view.GestureDetector;
import android.view.MotionEvent;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.ItemizedOverlay;
import com.google.android.maps.MapView;
import com.google.android.maps.OverlayItem;
import com.google.android.maps.Projection;

public class StopsOverlay extends ItemizedOverlay<OverlayItem> {
	private static final String TAG = "GrtItemizedOverlay";

	private ArrayList<OverlayItem> mOverlayItems = new ArrayList<OverlayItem>(1);
	private int mLonSpan, mLatSpan;
	private static int mCachedLonSpan, mCachedLatSpan;
	private GeoPoint mCenter;
	private static GeoPoint mCachedCenter;

	private Context mContext;
	private String mStopid;

	private class stopDetail {
		public final GeoPoint pt;
		public final String num, name;
		public stopDetail(GeoPoint g, String n, String a) {
			pt = g; num = n; name = a;
		}
	}
	private ArrayList<stopDetail> mStops = new ArrayList<stopDetail>(3000);
	private static ArrayList<stopDetail> mCachedStops = null;
	
	// Used to limit which stops are displayed
	public StopsOverlay(Context context) {
		super(boundCenter(context.getResources().getDrawable(R.drawable.blank)));
//		super(boundCenter(context.getResources().getDrawable(R.drawable.bluepin)));
//		super(boundCenter(context.getResources().getDrawable(android.R.drawable.star_big_on)));
		mContext = context;
	}
	
	// This is time consuming, and should not be called on the GUI thread
	public void LoadDB(String whereclause, String [] selectargs, NotificationCallback task) {
		Log.d(TAG, "starting LoadDB");
		Log.d(TAG, "Log.isLoggable says " + Log.isLoggable(TAG, Log.VERBOSE));
		
		final String table = "stops";
		final String [] columns = {"stop_lat", "stop_lon", "stop_id", "stop_name"};

		TimingLogger timings = new TimingLogger(TAG, "LoadDB");

		if (whereclause == null && mCachedStops != null) {
			Log.d(TAG, "using cached values");
	   		mLatSpan = mCachedLatSpan;
	   		mLonSpan = mCachedLonSpan;
	   		mCenter  = mCachedCenter;
			mStops   = mCachedStops;

		} else {

			Log.d(TAG, "no cached values");

			Cursor csr;
			try {
				csr = DatabaseHelper.ReadableDB().query(true, table, columns, whereclause, selectargs, null,null,null,null);
			} catch (SQLException e) {
				Log.e(TAG, "DB query failed: " + e.getMessage());
				return;
			}
			int maxcount = csr.getCount(), progresscount = 0;

			timings.addSplit("end of db read");

			// Going to calculate our centre
			int min_lat = 360000000, min_lon = 360000000, max_lat = -360000000, max_lon = -360000000;

			boolean more = csr.moveToPosition(0);
			while (more) {
				int stop_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
				int stop_lon = (int)(csr.getFloat(1) * 1000000);

				if (stop_lat < min_lat)
					min_lat = stop_lat;
				if (stop_lat > max_lat)
					max_lat = stop_lat;
				if (stop_lon < min_lon)
					min_lon = stop_lon;
				if (stop_lon > max_lon)
					max_lon = stop_lon;

				GeoPoint point = new GeoPoint(stop_lat, stop_lon);
				mStops.add(new stopDetail(point, csr.getString(2), csr.getString(3)));
				//OverlayItem overlayitem = new OverlayItem(point, csr.getString(2), csr.getString(3));
				//mOverlayItems.add(overlayitem);
				more = csr.moveToNext();

				//Log.d(TAG, " notification " + (int) ((mProgresscount / (float) mMaxcount) * 100));
				if (++progresscount % 25 == 0)
					task.notificationCallback((int) ((progresscount / (float) maxcount) * 100));

			}
			csr.close();

			// Stash some values needed for later calls
			mLatSpan = max_lat - min_lat;
			mLonSpan = max_lon - min_lon;
			mCenter = new GeoPoint(min_lat + mLatSpan/2, min_lon + mLonSpan/2);
		}
		
		if (whereclause == null && mCachedStops == null) {
			Log.d(TAG, "priming cache");
	   		mCachedLatSpan = mLatSpan;
	   		mCachedLonSpan = mLonSpan;
	   		mCachedCenter = mCenter;
			mCachedStops = mStops;
		}

		// Put in just one point, to make sure draw() is called. TODO
	    mOverlayItems.add(new OverlayItem(mCenter,"Dead","Centre"));

		timings.addSplit("found center");
   		
		populate();	// chomps up a lot of time....

 		timings.addSplit("populated");
 		timings.dumpToLog();
		Log.d(TAG, "exiting LoadDB");
	}

	private MapView mView;	// TODO
	private int findClosestStop(int screenX, int screenY) {
		TimingLogger timings = new TimingLogger(TAG, "closeststop");

		final int DIST_THRESHOLD = 512;
		int closestpt = -1;
		double closestdist = 1000000.0;
		Projection proj = mView.getProjection();
		GeoPoint scr = proj.fromPixels(screenX, screenY);
		
		for (int i=0 ; i<mStops.size(); i++) {
			stopDetail stop = mStops.get(i);
			int xdiff = scr.getLongitudeE6() - stop.pt.getLongitudeE6();
			int ydiff = scr.getLatitudeE6() - stop.pt.getLatitudeE6();
			double dist = Math.sqrt(xdiff*xdiff + ydiff*ydiff);
			if (dist < DIST_THRESHOLD && dist < closestdist) {
				closestpt = i;
				closestdist = dist;
			}
		}

		// TODO remove debug code
 		timings.addSplit("search ended");
 		timings.dumpToLog();
		if (closestpt >= 0) {
			Point pt_scr = new Point();
			proj.toPixels(mStops.get(closestpt).pt, pt_scr);
			Log.d(TAG, "closest stop is " + mStops.get(closestpt).num + " at " + closestdist + " units, " + mStops.get(closestpt).pt.getLatitudeE6() + "," + mStops.get(closestpt).pt.getLongitudeE6() + ", scr: " + pt_scr.x + ", " + pt_scr.y);
		}
		return closestpt;
	}
	
	// This must be called on the GIU thread
    private GestureDetector mGestureDetector = new GestureDetector(mContext, new GestureDetector.SimpleOnGestureListener() {
    	public boolean onSingleTapConfirmed (MotionEvent e) {
    		Log.d(TAG, "Single tap detected at " + e.getX() + "," + e.getY());
    		
    		int closestpt = findClosestStop((int)e.getX(), (int)e.getY());
    		if (closestpt >= 0) {
    			onScreenTap(closestpt, false);
    			return true;	// consumed it
    		}
    		return false;
        }
    	public void onLongPress (MotionEvent e) {
    		Log.d(TAG, "Long press detected at " + e.getX() + "," + e.getY());

    		int closestpt = findClosestStop((int)e.getX(), (int)e.getY());
    		if (closestpt >= 0)
    			onScreenTap(closestpt, true);
        }
    });

    @Override
    public boolean onTouchEvent(MotionEvent e, MapView mapView) {
    	mView = mapView;	// TODO
    	return mGestureDetector.onTouchEvent(e);
    }
	
	// Seeing we don't store all points in the overlay, we need to provide our own
	// span values, since the overlay has no clue of what we're doing.
	@Override
	public int getLonSpanE6() {
		return mLonSpan;
	}
	@Override
	public int getLatSpanE6() {
		return mLatSpan;
	}
	@Override
	public GeoPoint getCenter() {
		return mCenter;
	}
		
	// This is called when a bus stop is clicked on in the map.
	protected boolean onScreenTap(int index, boolean longpress) {
		Log.d(TAG, "OnTap(" + index + ")");
		stopDetail stop = mStops.get(index);
		mStopid = stop.num;
		final String stopname = stop.name;

		if (longpress == true) {

			Globals.tracker.trackEvent("Map longclick","Stop",mStopid,1);

			DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
				public void onClick(DialogInterface dialog, int id) {
					switch (id) {
					case DialogInterface.BUTTON_POSITIVE:
						Globals.mPreferences.AddBusstopFavourite(mStopid, stopname);
						break;
						//case DialogInterface.BUTTON_NEGATIVE:
						//// nothing
						//break;
					}
					dialog.cancel();
				}
			};

			AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
			builder.setTitle("Stop " + mStopid + ", " + stopname)
			.setMessage("Add to your list of favourites?")
			.setPositiveButton("Yes", listener)
			.setNegativeButton("No", listener)
			.create()
			.show();
		} else {
			Globals.tracker.trackEvent("Map click","Stop",mStopid,1);
			// Show route select activity
			Intent routeselect = new Intent(mContext, RouteselectActivity.class);
			String pkgstr = mContext.getApplicationContext().getPackageName();
			routeselect.putExtra(pkgstr + ".stop_id", mStopid);
			routeselect.putExtra(pkgstr + ".stop_name", stopname);
			mContext.startActivity(routeselect);
		}
		return true;
	}

	@Override
	protected OverlayItem createItem(int i) {
		return mOverlayItems.get(i);
	}
	
	@Override
	public int size() {
		return mOverlayItems.size();
	}

	/*
	 * Override this so we can add text labels to each pin.
	 * @see com.google.android.maps.ItemizedOverlay#draw(android.graphics.Canvas, com.google.android.maps.MapView, boolean)
	 */
	@Override
	public void draw(Canvas canvas, MapView view, boolean shadow) {
//		Log.d(TAG, "starting draw");
		super.draw(canvas, view, shadow);
		Log.v(TAG, "draw " + shadow + ", zoom is " + view.getZoomLevel());
//		Log.d(TAG, "top: " + view.getLeft() + "," + view.getTop() + ", bottom: " + view.getRight() + "," + view.getBottom());

		if (shadow)
			return;
		
		Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setTextAlign(Paint.Align.CENTER);
        paint.setTextSize(20);
        paint.setARGB(228,0,64,224);

		// Convert geo points to points on the canvas
		Projection proj = view.getProjection();
		Point pt_scr = new Point();

		Drawable stopmarker;
		Rect stopbounds;
		if (view.getZoomLevel() >= 14) {
			stopmarker = mContext.getResources().getDrawable(R.drawable.bluepin);
			stopbounds = new Rect(-4,-7,4,7);
		} else {
			stopmarker = mContext.getResources().getDrawable(R.drawable.bluepin);
//			stopmarker = mContext.getResources().getDrawable(R.drawable.smallpin);
			stopbounds = new Rect(-2,-3,2,3);
		}
		
        //Log.d(TAG, "drawing " + mStops.size() + " items");
		for (stopDetail stop : mStops) {
			proj.toPixels(stop.pt, pt_scr);
			
			// TODO view edges don't match where we're drawing?
			if (pt_scr.x < view.getLeft() || pt_scr.y < view.getTop() 
					|| pt_scr.x > view.getRight() || pt_scr.y > view.getBottom())
				continue;
/*
			if (stop.num.equals("1123")) {
				Log.d(TAG, "stop 1123: " + stop.pt.getLatitudeE6() + "," + stop.pt.getLongitudeE6() + ", scr: " + pt_scr.x + ", " + pt_scr.y);
			}
*/
			stopbounds.offsetTo(pt_scr.x, pt_scr.y);
			stopmarker.setBounds(stopbounds);
			stopmarker.draw(canvas);
			
            // show name above, number below the icon
			if (view.getZoomLevel() > 16) {
				canvas.drawText(stop.num, pt_scr.x, pt_scr.y+14, paint);
				if (view.getZoomLevel() > 18)
					canvas.drawText(stop.name, pt_scr.x, pt_scr.y-14, paint);
			}
		}

//		Log.d(TAG, "ending draw");
	}
}
