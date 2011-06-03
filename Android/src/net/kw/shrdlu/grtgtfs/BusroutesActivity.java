package net.kw.shrdlu.grtgtfs;

import java.util.List;

import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.Point;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.util.Log;
import android.widget.LinearLayout;
import android.widget.Toast;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;
import com.google.android.maps.OverlayItem;
import com.google.android.maps.Projection;
import com.google.android.maps.Overlay;

public class BusroutesActivity extends MapActivity {
	private static final String TAG = "RouteActivity";
	
	MapView mapView;
	
	List<Overlay> mapOverlays;
	Drawable drawable;
	BusstopsOverlay busstopsoverlay;
	private DatabaseHelper dbHelper;
	
	public static SQLiteDatabase DB = null;
	
	private class RouteOverlay extends Overlay {
		private static final String TAG = "RouteOverlay";
		private int[] mPoints = null;
		private int mCount = 0;
		
		public RouteOverlay(int[] points, int count) {
			super();
			mPoints = points;
			mCount = count;
		}
		
		@Override
		public void draw(Canvas canvas, MapView view, boolean shadow) {
			Log.v(TAG, "draw " + shadow);
			
			if (shadow || mPoints == null || mCount <= 0)
				return;
		
	        // Convert geo points to points on the canvas
	    	Projection proj = mapView.getProjection();
	    	Point pt_scr = new Point(0,0);
	    	Path path = new Path();

			proj.toPixels(new GeoPoint(mPoints[0], mPoints[1]), pt_scr);
			Log.v(TAG, "point (" + mPoints[0] + "," + mPoints[1] + ") -> (" + pt_scr.x + ", " + pt_scr.y + ")");
			path.moveTo(pt_scr.x, pt_scr.y);

			for (int i=1; i<mCount; i++) {
	    		proj.toPixels(new GeoPoint(mPoints[i*2], mPoints[(i*2)+1]), pt_scr);
//	    		Log.v(TAG, "point (" + mPoints[i*2] + "," + mPoints[(i*2)+1] + ") -> (" + pt_scr.x + ", " + pt_scr.y + ")");
	    		path.lineTo(pt_scr.x, pt_scr.y);
	    	}

			Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
			paint.setColor(0xffaf0000);
			canvas.drawPath(path, paint);
/*				
			paint.setColor(0xff00af00);
			canvas.drawLine(100, 100, 200, 100, paint);
			paint.setColor(0xff0000af);
			canvas.drawLine(200, 100, 200, 200, paint);
			canvas.drawLine(200, 200, 100, 200, paint);
			paint.setColor(0xff00afaf);
			canvas.drawLine(100, 200, 150, 150, paint);
*/			
		}
	}
	
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.mapview);
        
        mapView = (MapView) findViewById(R.id.mapview);
        mapView.setBuiltInZoomControls(true);
        
        mapOverlays = mapView.getOverlays();
        drawable = this.getResources().getDrawable(R.drawable.bluepin);

        dbHelper = new DatabaseHelper(this);
        DB = dbHelper.getReadableDatabase();

//        Intent intent = getIntent();
//        String route_id = intent.getStringExtra("route_id");
//        String headsign = intent.getStringExtra("headsign");
        String route_id = "12", headsign = "Conestoga Mall";
        
        String q = String.format(
        		"select stop_lat, stop_lon, stop_id, stop_name from stops where stop_id in " +
        		"(select stop_id from stop_times where trip_id = " +
        		"(select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\"))",
        		route_id, headsign);
  	  	Cursor csr = DB.rawQuery(q, null);

  	  	busstopsoverlay = new BusstopsOverlay(drawable, this, csr);
        mapOverlays.add(busstopsoverlay);
        csr.close();

        // Center the map
        MapController mcp = mapView.getController();
        mcp.setCenter(busstopsoverlay.getCenter());
        mcp.zoomToSpan(busstopsoverlay.getLatSpanE6(), busstopsoverlay.getLonSpanE6());

        // Now draw the route
    	String[] DbFields = {"shape_pt_lat","shape_pt_lon"};
    	try {
    		csr = DB.query("shapes", DbFields, "shape_id = \"1201\"", null, null, null, "shape_pt_sequence");
    	} catch (SQLException e) {
    		Log.e(TAG, "Querying shapes failed: " + e.getMessage());
    		return;
    	}

        csr.moveToPosition(0);
   		int count = csr.getCount();
   		int [] points = new int[count*2];
   		
   		for (int i=0; i<count; i++) {
   			int pt_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
   			int pt_lon = (int)(csr.getFloat(1) * 1000000);

   			points[i*2] = pt_lat;
   			points[(i*2)+1] = pt_lon;
   			
   			csr.moveToNext();
   		}
   		csr.close();
   		
    	// Draw a line connecting the points
		RouteOverlay routeoverlay = new RouteOverlay(points, count);
        mapOverlays.add(routeoverlay);
    	
    	MyLocationOverlay mylocation = new MyLocationOverlay(this, mapView);
        mylocation.enableMyLocation();
        mylocation.enableCompass();
        mapOverlays.add(mylocation);
    }

    @Override
    protected boolean isRouteDisplayed() {
    	return false;
    }

}
