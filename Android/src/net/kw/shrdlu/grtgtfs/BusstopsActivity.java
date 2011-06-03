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

public class BusstopsActivity extends MapActivity {
	private static final String TAG = "grtgtfs";

	MapView mapView;
	
	List<Overlay> mapOverlays;
	Drawable drawable;
	BusstopsOverlay busstopsoverlay;
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
        
        dbHelper = new DatabaseHelper(this);
        DB = dbHelper.getReadableDatabase();

        // Waterloo
//      GeoPoint point = new GeoPoint(43462580, -80518990);
        
    	String[] DbFields = {"stop_lat","stop_lon","stop_id","stop_name"};
    	Cursor csr;
    	try {
    		csr = DB.query("stops", DbFields, null, null, null, null, null);
//    		csr = DB.query("stops", DbFields, null, null, null, null, null, "100");
    	} catch (SQLException e) {
    		Log.e(TAG, "Querying stops failed: " + e.getMessage());
    		return;
    	}
	
        busstopsoverlay = new BusstopsOverlay(drawable, this, csr);
        
        // Center the map
        MapController mcp = mapView.getController();
        mcp.setCenter(busstopsoverlay.getCenter());
        mcp.zoomToSpan(busstopsoverlay.getLatSpanE6(), busstopsoverlay.getLonSpanE6());

        csr.close();
        mapOverlays.add(busstopsoverlay);

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