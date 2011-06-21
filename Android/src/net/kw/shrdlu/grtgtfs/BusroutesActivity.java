package net.kw.shrdlu.grtgtfs;

import java.util.List;

import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;

public class BusroutesActivity extends MapActivity {
	private static final String TAG = "RouteActivity";
	
	private MapActivity mContext;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
	private Drawable drawable;	
    private TextView mTitle;
    private MyLocationOverlay mMylocation;
    
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mContext = this;
        setContentView(R.layout.mapview);
        
        mMapview = (MapView) findViewById(R.id.mapview);
        mMapview.setBuiltInZoomControls(true);
        
        mapOverlays = mMapview.getOverlays();
        drawable = this.getResources().getDrawable(R.drawable.bluepin);

        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        String route_id = intent.getStringExtra(pkgstr + ".route_id");
        String headsign = intent.getStringExtra(pkgstr + ".headsign");
//        String stop_id = intent.getStringExtra(pkgstr + ".stop_id");	// TODO show position?
    
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

    	mMylocation = new MyLocationOverlay(this, mMapview);
        mMylocation.enableMyLocation();
        mMylocation.enableCompass();
        mapOverlays.add(mMylocation);
    }

    @Override
    protected boolean isRouteDisplayed() {
    	return false;
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.busstopsmenu, menu);
        return true;
    }

    // This is called when redisplaying the menu
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem item = menu.findItem(R.id.menu_about);
        item.setEnabled(false); // only put the about button on the home screen.
        item.setVisible(false);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_location: {
                // Center the map over the current location
            	GeoPoint locn = mMylocation.getMyLocation(); 
            	if (locn != null) {
            		MapController mcp = mMapview.getController();
            		mcp.animateTo(locn);
            		while (mMapview.getZoomLevel()<17)
            			if (!mcp.zoomIn())
            				break;
            	} else {
            		Toast.makeText(mContext, "No location fix!", Toast.LENGTH_LONG).show();
            	}
            	return true;
            }
        }
        return false;
    }
}
