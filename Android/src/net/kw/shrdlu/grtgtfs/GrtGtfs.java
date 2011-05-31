package net.kw.shrdlu.grtgtfs;

import java.io.IOException;
import java.util.List;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;
import com.google.android.maps.OverlayItem;

import android.content.Context;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.util.Log;
import android.util.Pair;
import android.widget.LinearLayout;
import android.widget.Toast;

public class GrtGtfs extends MapActivity {
	private static final String TAG = "grtgtfs";

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
//      GeoPoint point = new GeoPoint(43462580, -80518990);
        
		Toast t = Toast.makeText(this,
				"Slurping up the bus stops",
				Toast.LENGTH_LONG);
		t.show();
		
        dbHelper = new DatabaseHelper(this);
        DB = dbHelper.getReadableDatabase();
    	String[] DbFields = {"stop_lat","stop_lon","stop_id","stop_name"};
    	Cursor csr;
    	try {
//    		csr = DB.query("stops", DbFields, null, null, null, null, null);
    		csr = DB.query("stops", DbFields, null, null, null, null, null, "100");
    	} catch (SQLException e) {
    		Log.e(TAG, "Querying stops failed: " + e.getMessage());
    		return;
    	}
	
   		boolean more = csr.moveToPosition(0);
   		int minlon = 360000000, maxlon = -360000000, minlat = 360000000, maxlat = -360000000;	// track boundaries

   		while (more) {
   			int stop_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
   			int stop_lon = (int)(csr.getFloat(1) * 1000000);
   			if (stop_lon < minlon)
   				minlon = stop_lon;
   			if (stop_lon > maxlon)
   				maxlon = stop_lon;
   			if (stop_lat < minlat)
   				minlat = stop_lat;
   			if (stop_lat > maxlat)
   				maxlat = stop_lat;
   			
   			GeoPoint point = new GeoPoint(stop_lat, stop_lon);
   			OverlayItem overlayitem = new OverlayItem(point, csr.getString(2), csr.getString(3));
   	        itemizedoverlay.addOverlay(overlayitem);
   	        more = csr.moveToNext();
   		}
   		csr.close();
   		
   		itemizedoverlay.populateOverlay();
        mapOverlays.add(itemizedoverlay);
        
        MyLocationOverlay mylocation = new MyLocationOverlay(this, mapView);
        mylocation.enableMyLocation();
        mylocation.enableCompass();
        mapOverlays.add(mylocation);

        // Center the map
        GeoPoint mapcenter = new GeoPoint(
        		minlat + ((maxlat-minlat)/2),
        		minlon + ((maxlon-minlon)/2));
        MapController mcp = mapView.getController();
        mcp.setCenter(mapcenter);
        mcp.zoomToSpan(maxlat-minlat, maxlon-minlon);
}
    
    @Override
    protected boolean isRouteDisplayed() { return false; }
}