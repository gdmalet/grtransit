package net.kw.shrdlu.grtgtfs;

import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.util.Log;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapView;
import com.google.android.maps.Overlay;
import com.google.android.maps.Projection;

public class BusrouteOverlay extends Overlay {
	private static final String TAG = "BusrouteOverlay";
	
	private int mCount;
	private int [] mPoints = null;

	public BusrouteOverlay(Context context, String route, String headsign) {
		super();
		
        String q = String.format(
				"select shape_pt_lat, shape_pt_lon from shapes where shape_id = " +
				"(select shape_id from trips where route_id = \"%s\" and trip_headsign = \"%s\")",
        		route, headsign);

    	Cursor csr;
    	try {
    		csr = BusstopsOverlay.DB.rawQuery(q, null);
    	} catch (SQLException e) {
    		Log.e(TAG, "DB query failed: \"" + q + "\", " + e.getMessage());
    		return;
    	}

    	// TODO -- caching points in an array -- perhaps better to just cache the
    	// cursor, and reqpeat the query on draw()?
        csr.moveToPosition(0);
        mCount = csr.getCount();
   		mPoints = new int[mCount*2];
   		
   		for (int i=0; i<mCount; i++) {
   			int pt_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
   			int pt_lon = (int)(csr.getFloat(1) * 1000000);

   			mPoints[i*2] = pt_lat;
   			mPoints[(i*2)+1] = pt_lon;
   			
   			csr.moveToNext();
   		}
   		csr.close();
	}
		
	@Override
	public void draw(Canvas canvas, MapView view, boolean shadow) {
//		Log.v(TAG, "draw " + shadow);
			
		if (shadow || mPoints == null || mCount <= 0)
			return;
		
		// Convert geo points to points on the canvas
		Projection proj = view.getProjection();
		Point pt_scr = new Point(0,0);
		Path path = new Path();

		proj.toPixels(new GeoPoint(mPoints[0], mPoints[1]), pt_scr);
//		Log.v(TAG, "point (" + mPoints[0] + "," + mPoints[1] + ") -> (" + pt_scr.x + ", " + pt_scr.y + ")");
		path.moveTo(pt_scr.x, pt_scr.y);

		for (int i=1; i<mCount; i++) {
			proj.toPixels(new GeoPoint(mPoints[i*2], mPoints[(i*2)+1]), pt_scr);
//	  		Log.v(TAG, "point (" + mPoints[i*2] + "," + mPoints[(i*2)+1] + ") -> (" + pt_scr.x + ", " + pt_scr.y + ")");
	  		path.lineTo(pt_scr.x, pt_scr.y);
		}

		Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
		paint.setColor(0xff0000af);
		paint.setStyle(Paint.Style.STROKE);
		paint.setAlpha(96);
		paint.setStrokeWidth(5);
		canvas.drawPath(path, paint);
	}
}
