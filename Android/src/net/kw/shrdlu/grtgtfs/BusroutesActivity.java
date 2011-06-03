package net.kw.shrdlu.grtgtfs;

import java.util.List;

import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
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

public class BusroutesActivity extends MapActivity {
	private static final String TAG = "RouteActivity";
	
	LinearLayout linearLayout;
	MapView mapView;
	
	List<Overlay> mapOverlays;
	Drawable drawable;
	GrtItemizedOverlay itemizedoverlay;
	private DatabaseHelper dbHelper;
	
	public static SQLiteDatabase DB = null;
	
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.mapview);
        
        mapView = (MapView) findViewById(R.id.mapview);
        mapView.setBuiltInZoomControls(true);
        
        mapOverlays = mapView.getOverlays();
        drawable = this.getResources().getDrawable(R.drawable.bluepin);
        itemizedoverlay = new GrtItemizedOverlay(drawable, this);
        
        // Waterloo
        itemizedoverlay.addOverlay(new OverlayItem(
        		new GeoPoint(43462580, -80518990),
        		"Waterloo", "Le Pad"));
  
		Toast t = Toast.makeText(this,
				"Drawing route",
				Toast.LENGTH_LONG);
		t.show();
		
        dbHelper = new DatabaseHelper(this);
        DB = dbHelper.getReadableDatabase();
    	Cursor csr;
/*
   		// Need to work out zoom before we can draw, else the projection is wrong.
    	String[] LimitFields = {"min(shape_pt_lat)","max(shape_pt_lat)", "min(shape_pt_lon)", "max(shape_pt_lon)"};
    	try {
    		csr = DB.query("shapes", LimitFields, "shape_id = 1201", null, null, null, null);
    	} catch (SQLException e) {
    		Log.e(TAG, "Querying shapes limits failed: " + e.getMessage());
    		return;
    	}
        
        csr.moveToPosition(0);
    	int minlat = (int)(csr.getFloat(0) * 1000000); // microdegrees
    	int maxlat = (int)(csr.getFloat(1) * 1000000);
    	int minlon = (int)(csr.getFloat(2) * 1000000);
    	int maxlon = (int)(csr.getFloat(3) * 1000000);
*/
    	
        // Now draw the route
    	String[] DbFields = {"shape_pt_lat","shape_pt_lon"};
    	try {
    		csr = DB.query("shapes", DbFields, "shape_id = \"1201\"", null, null, null, "shape_pt_sequence");
    	} catch (SQLException e) {
    		Log.e(TAG, "Querying shapes failed: " + e.getMessage());
    		return;
    	}

        csr.moveToPosition(0);
   		int minlon = 360000000, maxlon = -360000000, minlat = 360000000, maxlat = -360000000;	// track boundaries
   		int count = csr.getCount();
   		int [] points = new int[count*2];
   		
   		for (int i=0; i<count; i++) {
   			int pt_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
   			int pt_lon = (int)(csr.getFloat(1) * 1000000);
   			if (pt_lon < minlon)
   				minlon = pt_lon;
   			if (pt_lon > maxlon)
   				maxlon = pt_lon;
   			if (pt_lat < minlat)
   				minlat = pt_lat;
   			if (pt_lat > maxlat)
   				maxlat = pt_lat;

   			points[i*2] = pt_lat;
   			points[(i*2)+1] = pt_lon;
   			
   			csr.moveToNext();
   		}
   		csr.close();
   		
        // Center the map
        GeoPoint mapcenter = new GeoPoint(
        		minlat + ((maxlat-minlat)/2),
        		minlon + ((maxlon-minlon)/2));
        MapController mcp = mapView.getController();
        mcp.setCenter(mapcenter);
        mcp.zoomToSpan(maxlat-minlat, maxlon-minlon);

        // Convert geo points to points on the canvas
    	Projection proj = mapView.getProjection();
    	Point pt_scr = new Point(0,0);
    	float[] scrpoints = new float[count*2];
    	for (int i=0; i< count; i++) {
    		proj.toPixels(new GeoPoint(points[i*2], points[(i*2)+1]), pt_scr);
    		scrpoints[i*2] = pt_scr.x;
    		scrpoints[(i*2)+1] = pt_scr.y;
    	}
    	
    	// Draw a line connecting the points
    	itemizedoverlay.stashRoute(scrpoints);
   		itemizedoverlay.populateOverlay();
        mapOverlays.add(itemizedoverlay);
    	
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
