package net.kw.shrdlu.grtgtfs;

import java.util.List;

import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;

public class BusroutesActivity extends MapActivity {
	private static final String TAG = "RouteActivity";
	
	private MapView mapView;
	private List<Overlay> mapOverlays;
	private Drawable drawable;	
    private TextView mTitle;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.mapview);
        
        mapView = (MapView) findViewById(R.id.mapview);
        mapView.setBuiltInZoomControls(true);
        
        mapOverlays = mapView.getOverlays();
        drawable = this.getResources().getDrawable(R.drawable.bluepin);

        Intent intent = getIntent();
        String route_id = intent.getStringExtra("route_id");
        String headsign = intent.getStringExtra("headsign");
        String stop_id = intent.getStringExtra("stop_id");	// TODO show position?
//        String route_id = "12", headsign = "Conestoga Mall";
    
        mTitle = (TextView) findViewById(R.id.title);
        mTitle.setText(route_id + " - " + headsign);
        
        String q = String.format("stop_id in " +
        		"(select stop_id from stop_times where trip_id = " +
        		"(select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\"))",
        		route_id, headsign);
  		BusstopsOverlay busstopsoverlay = new BusstopsOverlay(drawable, this, q);
        mapOverlays.add(busstopsoverlay);

        // Now draw the route
		BusrouteOverlay routeoverlay = new BusrouteOverlay(this, route_id, headsign);
        mapOverlays.add(routeoverlay);    	
/*
    	MyLocationOverlay mylocation = new MyLocationOverlay(this, mapView);
        mylocation.enableMyLocation();
        mylocation.enableCompass();
        mapOverlays.add(mylocation);
        // Center the map over the bus stops
        MapController mcp = mapView.getController();
        mcp.setCenter(busstopsoverlay.getCenter());
        mcp.zoomToSpan(busstopsoverlay.getLatSpanE6(), busstopsoverlay.getLonSpanE6());
*/        
    }

    @Override
    protected boolean isRouteDisplayed() {
    	return false;
    }

}
