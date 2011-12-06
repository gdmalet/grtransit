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

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.database.Cursor;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.util.TimingLogger;
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

public class RouteActivity extends MapActivity implements AnimationListener {
	private static final String TAG = "BusroutesActivity";
	
	private MapActivity mContext;
	private MapView mMapview;
	private List<Overlay> mapOverlays;
    private View mDetailArea;
    private TextView mTitle;
    private Animation mSlideIn, mSlideOut;
    private ProgressBar mProgress;    
    private MyLocationOverlay mMylocation;
    private String mRoute_id, mHeadsign, mStop_id;
	private StopsOverlay mBusstopsOverlay = null;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mContext = this;
        setContentView(R.layout.mapview);
        
        mMapview = (MapView) findViewById(R.id.mapview);
        mMapview.setBuiltInZoomControls(true);
        
        mapOverlays = mMapview.getOverlays();

        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
        mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
        mStop_id = intent.getStringExtra(pkgstr + ".stop_id");	// TODO show position?
    
    	// Load animation used to hide progress bar
        mProgress = (ProgressBar) findViewById(R.id.map_progress);
        mDetailArea = findViewById(R.id.mapview);
        mSlideIn  = AnimationUtils.loadAnimation(this, R.anim.slide_in);
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mSlideIn.setAnimationListener(this);
        mTitle = (TextView) findViewById(R.id.title);
        
        mMylocation = new MyLocationOverlay(this, mMapview);
        mapOverlays.add(mMylocation);
        
        // Get the busstop overlay set up in the background
    	mBusstopsOverlay = new StopsOverlay(mContext);
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

        // Hide the `Show on map' menu option
        menu.removeItem(R.id.menu_showonmap);

        return true;
    }

    // This is called when redisplaying the menu
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem item = menu.findItem(R.id.menu_about);  // TODO ? do a removeitem above?
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
    	return super.onOptionsItemSelected(item);
    }

    /**
     * Background task to handle initial load of the overlays.
     */
    private class PrepareOverlays extends AsyncTask<Void, Integer, ArrayList<RouteOverlay>>  implements NotificationCallback {
    	static final String TAG = "LookupTask";
    	
    	// A callback from LoadDB, for updating our progress bar
    	public void notificationCallback(Integer progress) {
			publishProgress(progress);
    	}
    	
        /**
         * Before jumping into background thread, start sliding in the
         * {@link ProgressBar}. We'll only show it once the animation finishes.
         */
        @Override
        protected void onPreExecute() {
//        	Log.v(TAG, "onPreExecute()");
			mDetailArea.startAnimation(mSlideIn);
        }

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
		}

		/**
         * Perform the background query.
         */
        @Override
        protected ArrayList<RouteOverlay> doInBackground(Void... foo) {
        	Log.v(TAG, "doInBackground()");
    		Log.d(TAG, "Log.isLoggable says " + Log.isLoggable(TAG, Log.VERBOSE));

            // TODO -- trip_headsign is wrong with route searches....
    		TimingLogger timings = new TimingLogger(TAG, "Routes");

    		ArrayList<RouteOverlay> overlays = new ArrayList<RouteOverlay>(16);
    		
        	if (mRoute_id != null) {	// doing one route
        		timings.addSplit("stops for one route");
//	        	final String whereclause = "stop_id in "
//	            	+ "(select stop_id from stop_times where trip_id = "
//	            	+ "(select trip_id from trips where route_id = ? and trip_headsign = ?))";
//	            final String [] selectargs = {mRoute_id, mHeadsign};

	            // It's too slow to fish out these stops, so for now show them all
	            //mBusstopsOverlay.LoadDB(whereclause, selectargs, this);
	            mBusstopsOverlay.LoadDB(null, null, this);
        		timings.addSplit(" end LoadDB");
	
	            // Now draw the route
        		timings.addSplit("drawing route");
	    		RouteOverlay routeoverlay = new RouteOverlay(mContext, mRoute_id, mHeadsign);
	            overlays.add(routeoverlay);

        	} else {
        		// doing many routes
        		timings.addSplit("stops for many routes");
//        		final String whereclause = "stop_id in "
//        			+ "(select distinct stop_id from stop_times where trip_id in "
//        			+ "(select trip_id from stop_times where stop_id = ?))";
//        		String [] selectargs = {mStop_id};
        		
	            // It's too slow to fish out these stops, so for now show them all
	            //mBusstopsOverlay.LoadDB(whereclause, selectargs, this);
	            mBusstopsOverlay.LoadDB(null, null, this);
        		timings.addSplit(" end LoadDB");
	
	            // Now draw the routes - taken from RouteselectActivity
	        	final Time t = new Time();	// TODO - this duplicates BusTimes?
	        	t.setToNow();
	        	final String datenow = String.format("%04d%02d%02d", t.year, t.month+1, t.monthDay);
	        	final String qry = "select distinct route_id, trip_headsign from trips" +
	        	" join calendar on trips.service_id = calendar.service_id where " + 
	        	" trip_id in (select trip_id from stop_times where stop_id = ?) and " +
	        	" start_date <= ? and end_date >= ?";
	       		final String [] selectargs = new String[] {mStop_id, datenow, datenow};
	        	Cursor csr = DatabaseHelper.ReadableDB().rawQuery(qry, selectargs);
        		timings.addSplit(" end db read for routes");
	            
    			int maxcount = csr.getCount(), progresscount = 0;
	    		boolean more = csr.moveToPosition(0);
	       		while (more) {
	        		timings.addSplit(" next route");
	       			RouteOverlay routeoverlay = new RouteOverlay(mContext, csr.getString(0), csr.getString(1));
		            overlays.add(routeoverlay);
	       			more = csr.moveToNext();
					publishProgress(((int) ((++progresscount / (float) maxcount) * 100)));
	       		}
	       		csr.close();
        	}
    		timings.addSplit("done routes");
        	timings.dumpToLog();
        	
            return overlays;    	
        }

        /**
         * When finished, link in the new overlay.
	     */
	    @Override
	    protected void onPostExecute(ArrayList<RouteOverlay> overlays) {
//        	Log.v(TAG, "onPostExecute()");

	    	// Overlays must be added on the GIU thread
	    	mapOverlays.add(mBusstopsOverlay);
	    	
	    	Rect boundingbox = null;
	    	
	    	// Need to calculate span and centre of overlays
	    	for (RouteOverlay overlay : overlays) {
	    		mapOverlays.add(overlay);
	    		if (boundingbox == null)
	   				boundingbox = overlay.getBoundingBoxE6();
	   			else
	   				boundingbox.union(overlay.getBoundingBoxE6());
	    	}

	    	// Centre the map over the bus stops
            MapController mcp = mMapview.getController();
            mcp.setCenter(new GeoPoint(boundingbox.centerX(), boundingbox.centerY()));
            mcp.zoomToSpan(boundingbox.right - boundingbox.left, boundingbox.bottom - boundingbox.top);

	        mProgress.setVisibility(View.INVISIBLE);
	    	mDetailArea.startAnimation(mSlideOut);

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
