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
import android.database.Cursor;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.Animation.AnimationListener;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;

public class RouteActivity extends MapActivity {
	private static final String TAG = "BusroutesActivity";
	
	private MapActivity mContext;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
	private Drawable mStopmarker;	
    private View mDetailArea;
    private TextView mTitle;
    private Animation mSlideOut;
    private ProgressBar mProgress;    
    private MyLocationOverlay mMylocation;
    private String mRoute_id, mHeadsign, mStop_id;
	private StopsOverlay mBusstopsOverlay = null;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mContext = this;
        setContentView(R.layout.mapview);
        
        mMapview = (MapView) findViewById(R.id.mapview);
        mMapview.setBuiltInZoomControls(true);
        
        mapOverlays = mMapview.getOverlays();
        mStopmarker = this.getResources().getDrawable(R.drawable.bluepin);

        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
        mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
        mStop_id = intent.getStringExtra(pkgstr + ".stop_id");	// TODO show position?
    
    	// Load animation used to hide progress bar
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mDetailArea = findViewById(R.id.mapview);
        mTitle = (TextView) findViewById(R.id.title);
        
    	mMylocation = new MyLocationOverlay(this, mMapview);
        mapOverlays.add(mMylocation);
        
        // Get the busstop overlay set up in the background
    	mBusstopsOverlay = new StopsOverlay(mStopmarker, mContext);
        new PrepareOverlays().execute();
    }

    @Override
    public void onResume() {
    	super.onResume();

    	// We want to track a pageView every time this Activity gets the focus.
        Globals.tracker.trackPageView("/" + this.getLocalClassName());

        mMylocation.enableMyLocation();
        mMylocation.enableCompass();    	
    }
    
    @Override
    public void onPause() {
    	super.onPause();
//    	Log.d(TAG, "onPause()");
    	mMylocation.disableMyLocation();
        mMylocation.disableCompass();    	
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
            case R.id.menu_searchstops: {
        		Intent stopsearch = new Intent(mContext, SearchStopsActivity.class);
        		stopsearch.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
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
    private class PrepareOverlays extends AsyncTask<Void, Void, Void> {
    	static final String TAG = "LookupTask";
    	
        /**
         * Before jumping into background thread, start sliding in the
         * {@link ProgressBar}. We'll only show it once the animation finishes.
         */
        @Override
        protected void onPreExecute() {
//        	Log.v(TAG, "onPreExecute()");
        }

        /**
         * Perform the background query.
         */
        @Override
        protected Void doInBackground(Void... foo) {
//        	Log.v(TAG, "doInBackground()");

            // TODO -- trip_headsign is wrong with route searches....

        	if (mRoute_id != null) {	// doing one route
	        	final String whereclause = "stop_id in "
	            	+ "(select stop_id from stop_times where trip_id = "
	            	+ "(select trip_id from trips where route_id = ? and trip_headsign = ?))";
	            final String [] selectargs = {mRoute_id, mHeadsign};
	
	            mBusstopsOverlay.LoadDB(whereclause, selectargs);
	            mapOverlays.add(mBusstopsOverlay);
	
	            // Now draw the route
	    		RouteOverlay routeoverlay = new RouteOverlay(mContext, mRoute_id, mHeadsign);
	            mapOverlays.add(routeoverlay);

        	} else {
        		// doing many routes
        		final String whereclause = "stop_id in "
        			+ "(select distinct stop_id from stop_times where trip_id in "
        			+ "(select trip_id from stop_times where stop_id = ?))";
        		String [] selectargs = {mStop_id};
	            mBusstopsOverlay.LoadDB(whereclause, selectargs);
	            mapOverlays.add(mBusstopsOverlay);
	
	            // Now draw the routes - taken from RouteselectActivity
	        	final Time t = new Time();	// TODO - this duplicates BusTimes?
	        	t.setToNow();
	        	final String datenow = String.format("%04d%02d%02d", t.year, t.month+1, t.monthDay);
	        	final String qry = "select distinct route_id, trip_headsign from trips" +
	        	" join calendar on trips.service_id = calendar.service_id where " + 
	        	" trip_id in (select trip_id from stop_times where stop_id = ?) and " +
	        	" start_date <= ? and end_date >= ?";
	       		selectargs = new String[] {mStop_id, datenow, datenow};
	        	Cursor csr = DatabaseHelper.ReadableDB().rawQuery(qry, selectargs);
	            
	    		boolean more = csr.moveToPosition(0);
	       		while (more) {
	       			RouteOverlay routeoverlay = new RouteOverlay(mContext, csr.getString(0), csr.getString(1));
	       			mapOverlays.add(routeoverlay);
	       			more = csr.moveToNext();
	       		}
	       		csr.close();
        	}
        	
            return null;    	
        }

        /**
         * When finished, link in the new overlay.
	     */
	    @Override
	    protected void onPostExecute(Void foo) {
//        	Log.v(TAG, "onPostExecute()");

            // Centre the map over the bus stops
            MapController mcp = mMapview.getController();
            GeoPoint center = mBusstopsOverlay.getCenter();
            if (center != null) {
            	mcp.setCenter(center);
            	mcp.zoomToSpan(mBusstopsOverlay.getLatSpanE6(), mBusstopsOverlay.getLonSpanE6());
            	//mcp.zoomOut(); // pull back a bit to show whole route.
            } else {
            	Log.e(TAG, "no center found for map!");
            }

        	if (mRoute_id != null) {	// doing one route
        		mTitle.setText("Rt " + mRoute_id + " - " + mHeadsign);
        	} else {
        		mTitle.setText("Routes using stop " + mStop_id);
        	}
	        mProgress.setVisibility(View.INVISIBLE);
	    	mDetailArea.startAnimation(mSlideOut);
	    }
	}    
}
