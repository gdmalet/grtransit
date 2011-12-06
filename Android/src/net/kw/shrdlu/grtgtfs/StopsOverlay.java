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
	private Rect mBoundingBox;
	private static Rect mCachedBoundingBox;
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
		mContext = context;
	}
	
	// This is time consuming, and should not be called on the GUI thread
	public void LoadDB(String whereclause, String [] selectargs, NotificationCallback task) {
		Log.d(TAG, "starting LoadDB");
		
		final String table = "stops";
		final String [] columns = {"stop_lat", "stop_lon", "stop_id", "stop_name"};

		if (whereclause == null && mCachedStops != null) {
			Log.d(TAG, "using cached values");
			mStops   = mCachedStops;
			mBoundingBox = mCachedBoundingBox;

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

			// Going to track the edges
			Rect boundingbox = null;

			boolean more = csr.moveToPosition(0);
			while (more) {
				int stop_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
				int stop_lon = (int)(csr.getFloat(1) * 1000000);

				GeoPoint point = new GeoPoint(stop_lat, stop_lon);
				mStops.add(new stopDetail(point, csr.getString(2), csr.getString(3)));
				more = csr.moveToNext();

	   			if (boundingbox == null)
	   				boundingbox = new Rect(stop_lat,stop_lon, stop_lat,stop_lon);
	   			else
	   				boundingbox.union(stop_lat, stop_lon);

	   			if (++progresscount % 25 == 0)
					task.notificationCallback((int) ((progresscount / (float) maxcount) * 100));
			}
			csr.close();

			// Stash values needed for later calls
			mBoundingBox = boundingbox;
		}
		
		if (whereclause == null && mCachedStops == null) {
			Log.d(TAG, "priming cache");
			mCachedStops = mStops;
			mCachedBoundingBox = mBoundingBox;
		}

		// Put in just one point, to make sure draw() is called.
	    mOverlayItems.add(new OverlayItem(new GeoPoint(mBoundingBox.centerX(), mBoundingBox.centerY()),"",""));

		populate();	// chomps up a lot of time if many points....

//		Log.d(TAG, "exiting LoadDB");
	}

	private MapView mView;	// TODO
	private int findClosestStop(int screenX, int screenY) {

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
/*
		// TODO remove debug code
		if (closestpt >= 0) {
			Point pt_scr = new Point();
			proj.toPixels(mStops.get(closestpt).pt, pt_scr);
			Log.d(TAG, "closest stop is " + mStops.get(closestpt).num + " at " + closestdist + " units, " + mStops.get(closestpt).pt.getLatitudeE6() + "," + mStops.get(closestpt).pt.getLongitudeE6() + ", scr: " + pt_scr.x + ", " + pt_scr.y);
		}
*/
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
	public Rect getBoundingBoxE6() {
		return mBoundingBox;
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
		Log.d(TAG, "view top: " + view.getLeft() + "," + view.getTop() + ", bottom: " + view.getRight() + "," + view.getBottom());

		if (shadow)
			return;
		
		Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
        paint.setTextAlign(Paint.Align.CENTER);
        paint.setTextSize(20);
        paint.setARGB(228,0,64,224);

		// Convert geo points to points on the canvas
		Projection proj = view.getProjection();
		Point pt_scr = new Point();

		int zoom = view.getZoomLevel();
		int dx = zoom/3, dy = zoom/2;
		Rect stopbounds = new Rect(0,0,dx,dy);
		Drawable stopmarker = mContext.getResources().getDrawable(R.drawable.bluepin);

		//Log.d(TAG, "drawing " + mStops.size() + " items");
		for (stopDetail stop : mStops) {
			proj.toPixels(stop.pt, pt_scr);
			
			if (pt_scr.x < 0 || pt_scr.y < 0 
					|| pt_scr.x > view.getRight() || pt_scr.y > view.getBottom() - view.getTop())
				continue;

			stopbounds.offsetTo(pt_scr.x-dx/2, pt_scr.y-dy/2);
			stopmarker.setBounds(stopbounds);
			stopmarker.draw(canvas);

			// TODO remove debug code
			if (stop.num.equals("1123") /*|| stop.num.equals("1124")*/) {
				Log.d(TAG, "bbox: " + stopbounds.left + ", " + stopbounds.top + ", " + stopbounds.right + ", " + stopbounds.bottom);
				Log.d(TAG, "stop 1123: " + stop.pt.getLatitudeE6() + "," + stop.pt.getLongitudeE6() + ", scr: " + pt_scr.x + ", " + pt_scr.y);
			}

            // show name above, number below the icon
			// TODO should use pretty popup labels
			if (zoom > 16) {
				canvas.drawText(stop.num, pt_scr.x, pt_scr.y+zoom+10, paint);
				if (zoom > 18)
					canvas.drawText(stop.name, pt_scr.x, pt_scr.y-20, paint);
			}
		}

//		Log.d(TAG, "ending draw");
	}
}
