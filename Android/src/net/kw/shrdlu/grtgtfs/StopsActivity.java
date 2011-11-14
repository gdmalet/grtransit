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

import android.app.AlertDialog;
import android.content.Intent;
import android.database.Cursor;
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

public class StopsActivity extends MapActivity implements AnimationListener {
	private static final String TAG = "BusstopsActivity";

	private MapActivity mContext;
    private View mTitleBar;
    private TextView mTitle;
    private Animation mSlideIn;
    private Animation mSlideOut;
    private ProgressBar mProgress;
    private MapView mMapview;
	private List<Overlay> mapOverlays;
	private Drawable mStopmarker;
	private MyLocationOverlay mMylocation;
	private String mStopId;
	private StopsOverlay mOverlay = null;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mContext = this;
        setContentView(R.layout.mapview);
        
    	// Load animations used to show/hide progress bar
        mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mTitleBar = findViewById(R.id.title_bar);
        mTitle = (TextView) findViewById(R.id.title);

        // Listen for the "in" animation so we make the progress bar visible
        // only after the sliding has finished.
        mSlideIn.setAnimationListener(this);

        mMapview = (MapView) findViewById(R.id.mapview);
        mMapview.setBuiltInZoomControls(true);
        
        mapOverlays = mMapview.getOverlays();
        mStopmarker = this.getResources().getDrawable(R.drawable.bluepin);

        mMylocation = new MyLocationOverlay(this, mMapview);
        mapOverlays.add(mMylocation);

        // See if we're entering as a result of a search. Show given stop if so,
        // else will try show current location.
        String stopstr = mContext.getApplicationContext().getPackageName() + ".stop_id";
        Intent intent = getIntent();
        mStopId = intent.getStringExtra(stopstr);
        
        // Get the busstop overlay set up in the background
    	mOverlay = new StopsOverlay(mStopmarker, mContext);
        new LoadOverlay().execute();
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
            case R.id.menu_about: {
                Globals.showAbout(this);
                return true;
            }
            case R.id.menu_searchstops: {
            	onSearchRequested();
                return true;
            }
          case R.id.menu_searchroutes: {
        		Intent routesearch = new Intent(mContext, SearchRoutesActivity.class);
        		routesearch.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
        		startActivity(routesearch);
        		return true;
            }
        }
        return false;
    }

    /**
     * Background task to handle initial load of the bus stops. This correctly shows and
     * hides the loading animation from the GUI thread before starting a
     * background query to the DB. When finished, it transitions
     * back to the GUI thread where it updates with the newly-found entries.
     */
    private class LoadOverlay extends AsyncTask<Void, Void, Void> {
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

        	mOverlay.LoadDB(null, null);
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

	        mapOverlays.add(mOverlay);
	        
            // Center the map over given bus stop, else location, else the whole area
            MapController mcp = mMapview.getController();
            GeoPoint center;
            if (mStopId == null) {
            	center = mMylocation.getMyLocation(); 
            	if (center != null) {
            		mcp.animateTo(center);
            		while (mMapview.getZoomLevel()<17)
            			if (!mcp.zoomIn())
            				break;
            	} else {
            		Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
            		center = mOverlay.getCenter();
            		if (center != null) {
            			mcp.setCenter(mOverlay.getCenter());
            			mcp.zoomToSpan(mOverlay.getLatSpanE6(), mOverlay.getLonSpanE6());
            		}
            	}
            } else {
            	final String table = "stops", where = "stop_id = ?";
            	final String [] columns = {"stop_lat", "stop_lon"}, selectargs = {mStopId};
                Cursor locn = DatabaseHelper.ReadableDB().query(table, columns, where, selectargs, null,null,null);
                if (locn.moveToFirst()) {
                	int stop_lat = (int)(locn.getFloat(0) * 1000000); // microdegrees
                	int stop_lon = (int)(locn.getFloat(1) * 1000000);       			
                	center = new GeoPoint(stop_lat, stop_lon);
                	mcp.setCenter(center);
                	mcp.setZoom(19);
                }
            	locn.close();
            }

            mTitle.setText(R.string.activity_desc);
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
