package net.kw.shrdlu.grtgtfs;

import java.util.List;

import android.app.AlertDialog;
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

public class BusstopsActivity extends MapActivity implements AnimationListener {
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
	
    /** Called when the activity is first created. */
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
        mMylocation.enableMyLocation();
        mMylocation.enableCompass();
        mapOverlays.add(mMylocation);

        // Get the busstop overlay set up in the background
        new LookupTask().execute();
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
            		Toast.makeText(mContext, "No location fix!", Toast.LENGTH_LONG).show();
            	}
            	return true;
            }
            case R.id.menu_about: {
                showAbout();
                return true;
            }
        }
        return false;
    }

    /**
     * Show an about dialog that cites data sources.
     */
    protected void showAbout() {
        View messageView = getLayoutInflater().inflate(R.layout.about, null, false);

        // When linking text, force to always use default color. This works
        // around a pressed color state bug.
        TextView textView = (TextView) messageView.findViewById(R.id.about_credits);
        int defaultColor = textView.getTextColors().getDefaultColor();
        textView.setTextColor(defaultColor);

        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setIcon(R.drawable.grticon);
        builder.setTitle(R.string.app_name);
        builder.setView(messageView);
        builder.create();
        builder.show();
    }

    /**
     * Background task to handle initial load of the bus stops. This correctly shows and
     * hides the loading animation from the GUI thread before starting a
     * background query to the DB. When finished, it transitions
     * back to the GUI thread where it updates with the newly-found entries.
     */
    private class LookupTask extends AsyncTask<Void, Void, BusstopsOverlay> {
    	static final String TAG = "LookupTask";
    	
        /**
         * Before jumping into background thread, start sliding in the
         * {@link ProgressBar}. We'll only show it once the animation finishes.
         */
        @Override
        protected void onPreExecute() {
        	Log.v(TAG, "onPreExecute()");
            mTitleBar.startAnimation(mSlideIn);
        }

        /**
         * Perform the background query.
         */
        @Override
        protected BusstopsOverlay doInBackground(Void... foo) {
        	Log.v(TAG, "doInBackground()");

            BusstopsOverlay overlay = new BusstopsOverlay(mStopmarker, mContext);

            return overlay;
        }

        /**
         * When finished, link in the new overlay.
	     */
	    @Override
	    protected void onPostExecute(BusstopsOverlay overlay) {
        	Log.v(TAG, "onPostExecute()");

	    	mTitleBar.startAnimation(mSlideOut);
	        mProgress.setVisibility(View.INVISIBLE);

	        mapOverlays.add(overlay);
	        
            // Center the map over the bus stops
            MapController mcp = mMapview.getController();
            mcp.setCenter(overlay.getCenter());
            mcp.zoomToSpan(overlay.getLatSpanE6(), overlay.getLonSpanE6());

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