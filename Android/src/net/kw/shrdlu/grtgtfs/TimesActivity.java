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

import android.app.AlertDialog;
import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.view.GestureDetector;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

public class TimesActivity extends ListActivity implements AnimationListener {
	private static final String TAG = "BustimesActivity";

	private ListActivity mContext;
    private View mListDetail;
    private Animation mSlideIn, mSlideOut;
    private ProgressBar mProgress;
	private String mRoute_id = null, mHeadsign, mStop_id;
    private TextView mTitle;
	private ServiceCalendar mServiceCalendar;
	private ArrayList<String []> mListDetails = null;

	private static boolean mCalendarChecked = false, mCalendarOK;

	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
//    	Log.v(TAG, "OnCreate()");

        mContext = this;
        mServiceCalendar = new ServiceCalendar(mContext);

        setContentView(R.layout.timeslayout);
        
    	// Load animations used to show/hide progress bar
        mProgress = (ProgressBar) findViewById(R.id.progress);
        mListDetail = findViewById(R.id.detail_area);
        mSlideIn  = AnimationUtils.loadAnimation(this, R.anim.slide_in);
        mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
        mSlideIn.setAnimationListener(this);
        mTitle = (TextView) findViewById(R.id.timestitle);

        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
        mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
        mStop_id  = intent.getStringExtra(pkgstr + ".stop_id");
        
        new ProcessBusTimes().execute();
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this Activity gets the focus.
        Globals.tracker.trackPageView("/" + this.getLocalClassName());
	}

	/*
	 * Do the processing to load the ArrayAdapter for display.
	 */
	private class ProcessBusTimes extends AsyncTask<Void, Integer, String> {
		static final String TAG = "ProcessBusTimes";

		private Integer maxcount, progresscount = 0;
		private int savedpos = -1, currentcount = 0;
		
		@Override
		protected void onPreExecute() {
			// Log.v(TAG, "onPreExecute()");
			mListDetail.startAnimation(mSlideIn);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
		}
		
		@Override
		protected String doInBackground(Void... foo) {
			// Log.v(TAG, "doInBackground()");

	    	ListView lv = getListView();
	        lv.setOnTouchListener(mGestureListener);

			// Will find where to position the list of bus departure times
			final Time t = new Time(); t.setToNow();
			final String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);
			final String datenow = String.format("%04d%02d%02d", t.year, t.month+1, t.monthDay);

			// Make sure we actually have some valid data, since schedules change often.
			if (!mCalendarChecked) {
				mCalendarOK = CheckCalendar(datenow);
			}
			if (!mCalendarOK)
				return null;

			String [] selectargs;
			String q;
			if (mRoute_id == null) {	// showing all routes
				selectargs = new String[] {mStop_id};
				q = "select departure_time as _id, trips.trip_id, route_id, trip_headsign from stop_times "
						+ "join trips on stop_times.trip_id = trips.trip_id where "
						+ "stop_id = ? order by departure_time";
			} else {
				selectargs = new String[] {mStop_id, mRoute_id, mHeadsign};
				q = "select departure_time as _id, trip_id from stop_times where stop_id = ? and trip_id in "
						+ "(select trip_id from trips where route_id = ? and trip_headsign = ?) order by departure_time";
			}
			Cursor csr = DatabaseHelper.ReadableDB().rawQuery(q, selectargs);

			// Load the array for the list
			maxcount = csr.getCount();
			mListDetails = new ArrayList<String []>(maxcount);
			String nextdeparture = "";
			boolean more = csr.moveToFirst();
			while (more) {
				
				String departure_time = csr.getString(0);
				String trip_id = csr.getString(1);

				// Get and translate the service id
				final String svsq = "select service_id from trips where trip_id = ?";
				String [] svsargs = {trip_id};
				Cursor svs = DatabaseHelper.ReadableDB().rawQuery(svsq, svsargs);
				svs.moveToFirst();
				String service_id = svs.getString(0);
				svs.close();
				String daysstr = mServiceCalendar.getDays(service_id, datenow, !Globals.mPreferences.getShowAllBusses());

				// Only add if the bus runs on this day.
				if (daysstr != null) {

					// is this where we position the list?
					if (savedpos == -1 && departure_time.compareTo(timenow) >= 0) {
						savedpos = currentcount;
						nextdeparture = departure_time;
					}

					if (mRoute_id != null) {	// showing one route
						String strs [] = {csr.getString(0), daysstr};
						mListDetails.add(strs);
					} else {
						String strs [] = {csr.getString(0), daysstr, csr.getString(2), csr.getString(3)};
						mListDetails.add(strs);
					}                

					currentcount++;
				}
				progresscount++;
				publishProgress((int) ((progresscount / (float) maxcount) * 100));
				
				more = csr.moveToNext();
			}
			csr.close();
			// Log.d(TAG, "processed " + count + " times");

			return nextdeparture;
		}
		
		@Override
		protected void onPostExecute(String nextdeparture) {
			//    	Log.v(TAG, "onPostExecute()");
			
			mProgress.setVisibility(View.INVISIBLE);
			mListDetail.startAnimation(mSlideOut);
	        
	        if (mRoute_id == null) {	// showing all routes
	        	mTitle.setText("Stop " + mStop_id + " - all routes");
	        } else {
	        	mTitle.setText(mRoute_id + " - " + mHeadsign);
	        }

	        if (!mCalendarOK) {
	    		AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
				builder.setIcon(R.drawable.grticon);
				builder.setTitle(R.string.app_name);
				builder.setMessage(R.string.calendar_expired);
				builder.create();
				builder.show();
				return;
	        }

	        if (mRoute_id != null) {	// showing one route
				TimesArrayAdapter adapter = new TimesArrayAdapter(mContext, mListDetails);
				mContext.setListAdapter(adapter);
			} else {
				TimesArrayAdapter2 adapter = new TimesArrayAdapter2(mContext, mListDetails);
				mContext.setListAdapter(adapter);
			}
			
			// Calculate the time difference
			Toast msg;
			if (savedpos >= 0) {
				final Time t = new Time(); t.setToNow();
				int hourdiff = Integer.parseInt(nextdeparture.substring(0, 2));
				hourdiff -= t.hour;
				hourdiff *= 60;
				int mindiff = Integer.parseInt(nextdeparture.substring(3, 5));
				mindiff -= t.minute;
				hourdiff += mindiff;
				if (hourdiff >= 60)
					msg = Toast.makeText(mContext, "Next bus leaves at " + nextdeparture, Toast.LENGTH_LONG);
				else {
					String plural = hourdiff > 1 ? "s" : "";
					msg = Toast.makeText(mContext, "Next bus leaves in " + hourdiff + " minute" + plural, Toast.LENGTH_LONG);
				}
				getListView().setSelectionFromTop(savedpos, 50);	// position next bus just below top
			} else {
				setSelection(currentcount); // position the list at the last bus
				msg = Toast.makeText(mContext, "No more busses today", Toast.LENGTH_LONG);
			}
			
			msg.setGravity(Gravity.TOP, 0, 0);
			msg.show();
		}
	}    

    /*
     * Make sure the calendar is current.
     * Updates mCalendarChecked if we get a result of some sort.
     */
    private boolean CheckCalendar(String datenow) {
    	boolean retval = true;	// report OK even if failure, so we just continue
    	String [] selectargs = {datenow, datenow};
    	Cursor csr = null;

    	try {
    		csr = DatabaseHelper.ReadableDB().rawQuery("select count(*) from calendar where "
        		+ "start_date <= ? and end_date >= ?", selectargs);
    	} catch (SQLException e) {
    		Log.e(TAG, "DB query failed checking calendar expiry: " + e.getMessage());
    	}
    	
        if (csr != null) {
        	if (csr.getCount() == 0 || !csr.moveToFirst() || csr.getInt(0) <= 0)
        		retval = false;

        	mCalendarChecked = true;
        	csr.close();
        }

        return retval;
    }
    
    @Override
    protected void onListItemClick(ListView l, View v, int position, long id) {
//    	Log.v(TAG, "clicked position " + position);

    	// Allow narrowing to one route, if we're showing many.
    	if (mRoute_id == null) {	// showing all routes
    		String [] items = (String[])l.getAdapter().getItem(position);
    		String route_id = items[2];
    		String headsign = items[3];

    		Intent bustimes = new Intent(mContext, TimesActivity.class);
    		String pkgstr = mContext.getApplicationContext().getPackageName();
    		bustimes.putExtra(pkgstr + ".route_id", route_id);
    		bustimes.putExtra(pkgstr + ".headsign", headsign);
    		bustimes.putExtra(pkgstr + ".stop_id", mStop_id);
    		mContext.startActivity(bustimes);
    	}
    }

    // This is only called once....
	@Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.timesmenu, menu);
        return true;
    }
	
	// This is called when redisplaying the menu
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
    	boolean showingall = Globals.mPreferences.getShowAllBusses();
        MenuItem item = menu.findItem(R.id.menu_showallbusses);
        item.setEnabled(!showingall);
        item = menu.findItem(R.id.menu_showtodaysbusses);
        item.setEnabled(showingall);
        return true;
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_showallbusses: {
            	Globals.mPreferences.setShowAllBusses(true);
            	// Just start again
                Intent intent = getIntent();
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                return true;
            }
            case R.id.menu_showtodaysbusses: {
            	Globals.mPreferences.setShowAllBusses(false);
                Intent intent = getIntent();
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                return true;
            }
            case R.id.menu_showonmap: {
            	Globals.tracker.trackEvent("Menu","Show route",
            			mRoute_id == null ? "All" : mRoute_id + " - " + mHeadsign,1);
            	// Perform action on click
            	String pkgstr = mContext.getApplicationContext().getPackageName();
            	Intent busroutes = new Intent(mContext, RouteActivity.class);
            	busroutes.putExtra(pkgstr + ".route_id", mRoute_id);
            	busroutes.putExtra(pkgstr + ".headsign", mHeadsign);
            	busroutes.putExtra(pkgstr + ".stop_id", mStop_id);
            	startActivity(busroutes);
            }            
        }
    	return super.onOptionsItemSelected(item);
    }
    
	private View.OnTouchListener mGestureListener = new View.OnTouchListener() {
		public boolean onTouch(View v, MotionEvent event) {
			return mGestureDetector.onTouchEvent(event); 
		}
	};

	// Catch flings, to show all busses coming to this stop.
	// This must be called on the GIU thread.
    private GestureDetector mGestureDetector = new GestureDetector(mContext, new GestureDetector.SimpleOnGestureListener() {
    	public boolean onFling(MotionEvent e1, MotionEvent e2, float velocityX, float velocityY) {
//    		Log.d(TAG, "fling X " + velocityX + ", Y " + velocityY);
    		// Catch a fling sort of from left to right
    		if (velocityX > 100 && Math.abs(velocityX) > Math.abs(velocityY)) {
//    			Log.d(TAG, "fling detected");
    			Globals.tracker.trackEvent("Times","fling left","",1);
    			finish();
    			return true;
    		}
    		return false;
    	}
    });

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
