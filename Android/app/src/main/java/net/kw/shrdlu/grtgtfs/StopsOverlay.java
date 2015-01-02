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

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.graphics.NinePatch;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.util.Log;
import android.view.GestureDetector;
import android.view.MotionEvent;

import com.google.android.gms.analytics.HitBuilders;
import com.google.android.maps.GeoPoint;
import com.google.android.maps.ItemizedOverlay;
import com.google.android.maps.MapView;
import com.google.android.maps.OverlayItem;
import com.google.android.maps.Projection;

import net.kw.shrdlu.grtgtfs.Activities.RouteselectActivity;

import java.util.ArrayList;

public class StopsOverlay extends ItemizedOverlay<OverlayItem> {
	private static final String TAG = "GrtItemizedOverlay";

	private final ArrayList<OverlayItem> mOverlayItems = new ArrayList<>(1);
	private Rect mBoundingBox;
	private static Rect mCachedBoundingBox;
	private Context mContext = null;
	private String mStopid;

	private class stopDetail {
		public final GeoPoint pt;
		public final String num, name;

		public stopDetail(GeoPoint g, String n, String a) {
			pt = g;
			num = n;
			name = a;
		}
	}

	private ArrayList<stopDetail> mStops = new ArrayList<>(3000);
	private static ArrayList<stopDetail> mCachedStops = null;

	// Used to limit which stops are displayed
	public StopsOverlay(Context context) {
		super(boundCenter(context.getResources().getDrawable(R.drawable.blank)));
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
			Rect boundingbox = null;

			boolean more = csr.moveToPosition(0);
			while (more) {
				final int stop_lat = (int) (csr.getFloat(0) * 1000000); // microdegrees
				final int stop_lon = (int) (csr.getFloat(1) * 1000000);

				final GeoPoint point = new GeoPoint(stop_lat, stop_lon);
				mStops.add(new stopDetail(point, csr.getString(2), csr.getString(3)));
				more = csr.moveToNext();

				if (boundingbox == null) {
					boundingbox = new Rect(stop_lat, stop_lon, stop_lat, stop_lon);
				} else {
					boundingbox.union(stop_lat, stop_lon);
				}

				if (++progresscount % 25 == 0) {
					task.notificationCallback((int) ((progresscount / (float) maxcount) * 10000));
				}
			}
			csr.close();

			// Stash values needed for later calls
			mBoundingBox = boundingbox;
		}

		if (whereclause == null && mCachedStops == null) {
			// Log.d(TAG, "priming cache");
			mCachedStops = mStops;
			mCachedBoundingBox = mBoundingBox;
		}

		// Put in just one point, to make sure draw() is called.
		mOverlayItems.add(new OverlayItem(new GeoPoint(mBoundingBox.centerX(), mBoundingBox.centerY()), "", ""));

		populate(); // chomps up a lot of time if many points....

		// Log.d(TAG, "exiting LoadDB");
	}

	private int findClosestStop(MapView view, int screenX, int screenY) {
		final int DIST_THRESHOLD = 512;
		int closestpt = -1;
		double closestdist = 1000000.0;
		final Projection proj = view.getProjection();
		final GeoPoint scr = proj.fromPixels(screenX, screenY);

		for (int i = 0; i < mStops.size(); i++) {
			final stopDetail stop = mStops.get(i);
			final int xdiff = scr.getLongitudeE6() - stop.pt.getLongitudeE6();
			final int ydiff = scr.getLatitudeE6() - stop.pt.getLatitudeE6();
			final double dist = Math.sqrt(xdiff * xdiff + ydiff * ydiff);
			if (dist < DIST_THRESHOLD && dist < closestdist) {
				closestpt = i;
				closestdist = dist;
			}
		}

		return closestpt;
	}

	// This must be called on the GIU thread
	private MapView mView; // TODO this is messy
	private final GestureDetector mGestureDetector = new GestureDetector(mContext,
			new GestureDetector.SimpleOnGestureListener() {
		@Override
		public boolean onSingleTapConfirmed(MotionEvent e) {
			// Log.d(TAG, "Single tap detected at " + e.getX() + "," +
			// e.getY());

			final int closestpt = findClosestStop(mView, (int) e.getX(), (int) e.getY());
			if (closestpt >= 0) {
				onScreenTap(closestpt, false);
				return true; // consumed it
			}
			return false;
		}

		@Override
		public void onLongPress(MotionEvent e) {
			// Log.d(TAG, "Long press detected at " + e.getX() + "," +
			// e.getY());

			final int closestpt = findClosestStop(mView, (int) e.getX(), (int) e.getY());
			if (closestpt >= 0) {
				onScreenTap(closestpt, true);
			}
		}
	});

	@Override
	public boolean onTouchEvent(MotionEvent e, MapView mapView) {
		mView = mapView; // TODO this is messy
		return mGestureDetector.onTouchEvent(e);
	}

	// Seeing we don't store all points in the overlay, we need to provide our
	// own
	// span values, since the overlay has no clue of what we're doing.
	public Rect getBoundingBoxE6() {
		return mBoundingBox;
	}

	// This is called when a bus stop is clicked on in the map.
    void onScreenTap(int index, boolean longpress) {
		// Log.d(TAG, "OnTap(" + index + ")");
		final stopDetail stop = mStops.get(index);
		mStopid = stop.num;
		final String stopname = stop.name;

		if (longpress) {

            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory("Map longclick")
                    .setAction("Stop")
                    .setLabel(mStopid)
                    .build());

			final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int id) {
					switch (id) {
					case DialogInterface.BUTTON_POSITIVE:
						GRTApplication.mPreferences.AddBusstopFavourite(mStopid, stopname);
						break;
						// case DialogInterface.BUTTON_NEGATIVE:
						// // nothing
						// break;
					}
					dialog.cancel();
				}
			};

			final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
			builder.setTitle("Stop " + mStopid + ", " + stopname).setMessage(R.string.favs_add_to_list)
			.setPositiveButton(R.string.yes, listener).setNegativeButton(R.string.no, listener).create().show();
		} else {
            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory("Map click")
                    .setAction("stop")
                    .setLabel(mStopid)
                    .build());

			// Show route select activity
			final Intent routeselect = new Intent(mContext, RouteselectActivity.class);
			final String pkgstr = mContext.getApplicationContext().getPackageName();
			routeselect.putExtra(pkgstr + ".stop_id", mStopid);
			routeselect.putExtra(pkgstr + ".stop_name", stopname);
			mContext.startActivity(routeselect);
		}
	}

	@Override
	protected OverlayItem createItem(int i) {
		return mOverlayItems.get(i);
	}

	@Override
	public int size() {
		return mOverlayItems.size();
	}

	/* Override this so we can add text labels to each pin.
	 * 
	 * @see com.google.android.maps.ItemizedOverlay#draw(android.graphics.Canvas, com.google.android.maps.MapView, boolean) */
	private class cachedStop {
		public final Point pt;
		public final String num, name;

		public cachedStop(Point p, String n, String a) {
			pt = p;
			num = n;
			name = a;
		}
	}

	@Override
	public void draw(Canvas canvas, MapView view, boolean shadow) {
		// Log.d(TAG, "starting draw");
		super.draw(canvas, view, shadow);

		if (shadow) {
			return;
		}

		// Some constants used in measuring.
		final int textZoomLimit = 17, textSize = 20, padding = 4;

		final Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
		paint.setTextAlign(Paint.Align.LEFT);
		paint.setTextSize(textSize);
		paint.setColor(mContext.getResources().getColor(R.color.grt_blue));

		// Convert geo points to points on the canvas
		final Projection proj = view.getProjection();
		final Point pt_scr = new Point();

		// size the stop marker
		final int zoom = view.getZoomLevel();
		final int dx = zoom / 2, dy = zoom / 2;
		final Rect stopbounds = new Rect(0, 0, dx, dy);
		final Drawable stopmarker = mContext.getResources().getDrawable(R.drawable.bluepin);

		NinePatch npTextBox = null;
		Rect textbounds = null;
		ArrayList<cachedStop> cachedStops = null;
		ArrayList<Rect> cachedRects = null;
		if (zoom > textZoomLimit) {
			final Bitmap bmTextBox = BitmapFactory.decodeResource(mContext.getResources(), R.drawable.stopnumber);
			npTextBox = new NinePatch(bmTextBox, bmTextBox.getNinePatchChunk(), "stopnumber");
			textbounds = new Rect();
			cachedStops = new ArrayList<>(32);
			cachedRects = new ArrayList<>(64);
		}

		// First loop over all visible stops, drawing them and caching boundaries.
		for (final stopDetail stop : mStops) {
			proj.toPixels(stop.pt, pt_scr);

			// Skip it if it's not visible.
			if (pt_scr.x < 0 || pt_scr.y < 0 || pt_scr.x > view.getRight() || pt_scr.y > view.getBottom() - view.getTop()) {
				continue;
			}

			stopbounds.offsetTo(pt_scr.x - dx / 2, pt_scr.y - dy / 2);
			stopmarker.setBounds(stopbounds);
			stopmarker.draw(canvas);

			if (zoom > textZoomLimit) {
				cachedStops.add(new cachedStop(new Point(pt_scr), stop.num, stop.name));
				cachedRects.add(new Rect(stopbounds));
			}
		}

		// Now try to draw a text box containing the stop number.
		if (zoom > textZoomLimit) {
			for (final cachedStop stop : cachedStops) {
				paint.getTextBounds(stop.num, 0, stop.num.length(), textbounds);
				textbounds.inset(-padding * 2, -padding * 2); // add a bit of padding

				// Try offset the balloon to various places if there's a clash
				// The origin for the box and text is the top left corner.
				final Point[] offsets = { new Point(-textbounds.right / 2 - dx / 2, dy + padding), // centred bottom
						new Point(-textbounds.right / 2 - dx / 2, -dy - dy - textSize - padding), // centred top
						new Point(dx / 2 + padding, -textSize / 2 - dy / 2), // right
						new Point(-dx * 2 - textbounds.right - padding, -textSize / 2 - dy / 2), // left
						new Point(dx / 2 + padding, +padding), // lower right
						new Point(dx / 2 + padding, -textSize - padding - dy), // upper right
						new Point(-dx * 2 - textbounds.right - padding, -textSize - padding - dy), // upper left
						new Point(-dx * 2 - textbounds.right - padding, +padding) // lower left
				};

				// See if we can draw the text in any of the spots. If this fails, just ignore it,
				// rather than cram it in, since it will be unreadable anyway.
				for (final Point point : offsets) {
					textbounds.offsetTo(stop.pt.x + point.x, stop.pt.y + point.y);

					boolean intersects = false;
					for (final Rect rect : cachedRects) {
						if ((intersects = Rect.intersects(rect, textbounds)) == true) {
							break;
						}
					}

					if (!intersects) {
						cachedRects.add(new Rect(textbounds)); // reserve
						// draw the boundary, then add text
						npTextBox.draw(canvas, textbounds, paint);
						textbounds.offset(padding + 4, -padding - 4);
						canvas.drawText(stop.num, textbounds.left, textbounds.bottom, paint);
						break;
					}
				}
			}

			// Then do it all again for the stop name.
			if (zoom > textZoomLimit + 3) {
				for (final cachedStop stop : cachedStops) {
					paint.getTextBounds(stop.name, 0, stop.name.length(), textbounds);
					textbounds.inset(-padding * 2, -padding * 2); // add a bit of padding

					// Try offset the balloon to various places if there's a clash
					// The origin for the box and text is the top left corner.
					final Point[] offsets = { new Point(-textbounds.right / 2 - dx / 2, -dy - dy - textSize - padding), // centred
							// top
							new Point(-dx - textbounds.right - padding, -textSize / 2 - dy / 2), // left
							new Point(dx / 2 + padding, -textSize / 2 - dy / 2), // right
							new Point(-dx - textbounds.right - padding, -textSize - padding - dy), // upper left
							new Point(-dx - textbounds.right - padding, +padding), // lower left
							new Point(dx / 2 + padding, +padding), // lower right
							new Point(dx / 2 + padding, -textSize - padding - dy), // upper right
							new Point(-textbounds.right / 2 - dx / 2, dy + padding) // centred bottom
					};

					// See if we can draw the text in any of the spots. If this fails, just ignore it,
					// rather than cram it in, since it will be unreadable anyway.
					for (final Point point : offsets) {
						textbounds.offsetTo(stop.pt.x + point.x, stop.pt.y + point.y);

						boolean intersects = false;
						for (final Rect rect : cachedRects) {
							if ((intersects = Rect.intersects(rect, textbounds)) == true) {
								break;
							}
						}

						if (!intersects) {
							cachedRects.add(new Rect(textbounds)); // reserve
							npTextBox.draw(canvas, textbounds, paint);
							textbounds.offset(padding + 4, -padding - 4);
							canvas.drawText(stop.name, textbounds.left, textbounds.bottom, paint);
							break;
						}
					}
				}
			}
		}
	}
}
