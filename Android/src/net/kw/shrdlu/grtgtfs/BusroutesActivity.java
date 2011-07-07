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

import java.util.List;

import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
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
	private static final String TAG = "BusroutesActivity";
	
	private MapActivity mContext;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
	private Drawable drawable;	
    private View mTitleBar;
    private TextView mTitle;
    private Animation mSlideIn;
    private Animation mSlideOut;
    private ProgressBar mProgress;    
    private MyLocationOverlay mMylocation;
    private String mRoute_id, mHeadsign;
    
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
        mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
        mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
//        String stop_id = intent.getStringExtra(pkgstr + ".stop_id");	// TODO show position?
    
    	// Load animations used to show/hide progress bar
        mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mTitleBar = findViewById(R.id.title_bar);
        mTitle = (TextView) findViewById(R.id.title);
        
    	mMylocation = new MyLocationOverlay(this, mMapview);
        mMylocation.enableMyLocation();
        mMylocation.enableCompass();
        mapOverlays.add(mMylocation);
        
        // Do the rest off the main thread
        new PrepareOverlays().execute();
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
            		Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
            	}
            	return true;
            }
/*          case R.id.menu_restart: {
        		Intent restart = new Intent(mContext, BusstopsActivity.class);
        		restart.setAction(Intent.ACTION_MAIN);
        		restart.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        		startActivity(restart);
        		return true;
            }
*/          case R.id.menu_searchstops: {
        		Intent stopsearch = new Intent(getIntent());
        		stopsearch.setClass(mContext, BusstopsearchActivity.class);
        		stopsearch.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
        		stopsearch.setFlags(/*Intent.FLAG_ACTIVITY_CLEAR_TOP |*/ Intent.FLAG_ACTIVITY_NO_HISTORY);
        		startActivity(stopsearch);
                return true;
            }
            case R.id.menu_searchroutes: {
            	onSearchRequested();
                return true;
            }
        }
        return false;
    }

    /**
     * Background task to handle initial load of the overlays.
     */
    private class PrepareOverlays extends AsyncTask<Void, Void, BusstopsOverlay> {
    	static final String TAG = "LookupTask";
    	
        /**
         * Before jumping into background thread, start sliding in the
         * {@link ProgressBar}. We'll only show it once the animation finishes.
         */
        @Override
        protected void onPreExecute() {
//        	Log.v(TAG, "onPreExecute()");
            mTitleBar.startAnimation(mSlideIn);
        }

        /**
         * Perform the background query.
         */
        @Override
        protected BusstopsOverlay doInBackground(Void... foo) {
//        	Log.v(TAG, "doInBackground()");

            // TODO -- trip_headsign is wrong with route searches....
/*            String q = String.format("stop_id in " +
            		"(select stop_id from stop_times where trip_id = " +
            		"(select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\"))",
            		mRoute_id, mHeadsign);
*/           final String whereclause = "stop_id in "
            	+ "(select stop_id from stop_times where trip_id = "
            	+ "(select trip_id from trips where route_id = ? and trip_headsign = ?))";
            String [] selectargs = {mRoute_id, mHeadsign};

            BusstopsOverlay busstopsoverlay = new BusstopsOverlay(drawable, mContext, whereclause, selectargs);
            mapOverlays.add(busstopsoverlay);

            // Now draw the route
    		BusrouteOverlay routeoverlay = new BusrouteOverlay(mContext, mRoute_id, mHeadsign);
            mapOverlays.add(routeoverlay);

            return busstopsoverlay;    	
        }

        /**
         * When finished, link in the new overlay.
	     */
	    @Override
	    protected void onPostExecute(BusstopsOverlay overlay) {
//        	Log.v(TAG, "onPostExecute()");

	    	mTitleBar.startAnimation(mSlideOut);
	        mProgress.setVisibility(View.INVISIBLE);
	        
            // Center the map over the bus stops
            MapController mcp = mMapview.getController();
            GeoPoint center = overlay.getCenter();
            if (center != null) {
            	mcp.setCenter(center);
            	mcp.zoomToSpan(overlay.getLatSpanE6(), overlay.getLonSpanE6());
            	//mcp.zoomOut(); // pull back a bit to show whole route.
            } else {
            	Log.e(TAG, "no center found for map!");
            }

            mTitle.setText("Rt " + mRoute_id + " - " + mHeadsign);
	    }
	}
}
