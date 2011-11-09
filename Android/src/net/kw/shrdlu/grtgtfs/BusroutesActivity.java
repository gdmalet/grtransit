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

public class BusroutesActivity extends MapActivity implements AnimationListener {
	private static final String TAG = "BusroutesActivity";
	
	private MapActivity mContext;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
	private Drawable mStopmarker;	
    private View mTitleBar;
    private TextView mTitle;
    private Animation mSlideIn;
    private Animation mSlideOut;
    private ProgressBar mProgress;    
    private MyLocationOverlay mMylocation;
    private String mRoute_id, mHeadsign, mStop_id;
	private BusstopsOverlay mBusstopsOverlay = null;

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
    
    	// Load animations used to show/hide progress bar
        mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mTitleBar = findViewById(R.id.title_bar);
        mTitle = (TextView) findViewById(R.id.title);
        
        // Listen for the "in" animation so we make the progress bar visible
        // only after the sliding has finished.
        mSlideIn.setAnimationListener(this);

    	mMylocation = new MyLocationOverlay(this, mMapview);
        mapOverlays.add(mMylocation);
        
        // Get the busstop overlay set up in the background
    	mBusstopsOverlay = new BusstopsOverlay(mStopmarker, mContext);
        new PrepareOverlays().execute();
    }

    @Override
    public void onResume() {
    	super.onResume();
//    	Log.d(TAG, "onResume()");
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
        		Intent stopsearch = new Intent(mContext, BusstopsearchActivity.class);
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
            mTitleBar.startAnimation(mSlideIn);
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
	    		BusrouteOverlay routeoverlay = new BusrouteOverlay(mContext, mRoute_id, mHeadsign);
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
	       			BusrouteOverlay routeoverlay = new BusrouteOverlay(mContext, csr.getString(0), csr.getString(1));
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

	    	mTitleBar.startAnimation(mSlideOut);
	        mProgress.setVisibility(View.INVISIBLE);
	        
            // Center the map over the bus stops
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
	    }
	}
    
    /**
     * Make the {@link ProgressBar} visible when our in-animation finishes.
     */
    public void onAnimationEnd(Animation animation) {
        mProgress.setVisibility(View.VISIBLE);
    }
    public void onAnimationRepeat(Animation animation) {
        // Not interested if the animation repeats
    }
    public void onAnimationStart(Animation animation) {
        // Not interested when the animation starts
    }

}
