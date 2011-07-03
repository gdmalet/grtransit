package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;

import android.app.AlertDialog;
import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.util.Pair;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;
import android.widget.Toast;

public class BustimesActivity extends ListActivity {
	private static final String TAG = "BustimesActivity";

	private ListActivity mContext;
	private String mRoute_id, mHeadsign, mStop_id;
    private TextView mTitle;
	private ServiceCalendar mServiceCalendar = new ServiceCalendar();
	private static boolean mShowTodayOnly = true;

	private static boolean mCalendarChecked = false, mCalendarOK;

	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mContext = this;
//    	Log.v(TAG, "OnCreate()");
    	
        setContentView(R.layout.timeslayout);

        final Button button = (Button) findViewById(R.id.timesbutton);
        button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                // Perform action on click
                String pkgstr = mContext.getApplicationContext().getPackageName();
        		Intent busroutes = new Intent(mContext, BusroutesActivity.class);
        		busroutes.putExtra(pkgstr + ".route_id", mRoute_id);
        		busroutes.putExtra(pkgstr + ".headsign", mHeadsign);
        		busroutes.putExtra(pkgstr + ".stop_id", mStop_id);
        		startActivity(busroutes);
            }
        });
        
        String pkgstr = mContext.getApplicationContext().getPackageName();
        Intent intent = getIntent();
        mRoute_id = intent.getStringExtra(pkgstr + ".route_id");
        mHeadsign = intent.getStringExtra(pkgstr + ".headsign");
        mStop_id  = intent.getStringExtra(pkgstr + ".stop_id");
        
        mTitle = (TextView) findViewById(R.id.timestitle);
        mTitle.setText(mRoute_id + " - " + mHeadsign);

        if (!mCalendarChecked || (mCalendarChecked && mCalendarOK))
        	ProcessBusTimes();
        
        if (mCalendarChecked && !mCalendarOK) {
    		AlertDialog.Builder builder = new AlertDialog.Builder(this);
			builder.setIcon(R.drawable.grticon);
			builder.setTitle(R.string.app_name);
			builder.setMessage(R.string.calendar_expired);
			builder.create();
			builder.show();
        }
	}
	
	/*
	 * Do the processing to load the ArrayAdapter for display.
	 */
	void ProcessBusTimes() {
    	// Will find where to position the list of bus departure times
    	Time t = new Time();
    	t.setToNow();
    	String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);
    	String datenow = String.format("%04d%02d%02d", t.year, t.month+1, t.monthDay);

        // Make sure we actually have some valid data, since schedules change often.
        if (!mCalendarChecked) {
        	mCalendarOK = CheckCalendar(datenow);
        }
        if (!mCalendarOK)
        	return;
        
        final String q = "select departure_time as _id, trip_id from stop_times where stop_id = ? and trip_id in "
        	+ "(select trip_id from trips where route_id = ? and trip_headsign = ?) order by departure_time";
        String [] selectargs = {mStop_id, mRoute_id, mHeadsign};
        Cursor csr = BusstopsOverlay.DB.rawQuery(q, selectargs);
        startManagingCursor(csr);

    	// Load the array for the list
    	ArrayList<Pair<String,String>> details = new ArrayList<Pair<String,String>>(csr.getCount());
        Pair<String,String> pair;
        int pos = -1, count = 0;
        String nextdeparture = null;
        boolean more = csr.moveToFirst();
        while (more) {
        	String departure_time = csr.getString(0);
        	String trip_id = csr.getString(1);
        	
            // Get and translate the service id
    		final String svsq = "select service_id from trips where trip_id = ?";
    		String [] svsargs = {trip_id};
            Cursor svs = BusstopsOverlay.DB.rawQuery(svsq, svsargs);

            svs.moveToFirst();
            String service_id = svs.getString(0);
            svs.close();
        	String daysstr = mServiceCalendar.getDays(service_id, datenow, mShowTodayOnly);

        	// Only add if the bus runs on this day.
        	// TODO - short circuit if we found the last bus?
        	if (daysstr != null) {
        		pair = new Pair<String,String>(departure_time, daysstr);
        		details.add(pair);
        	
        		// is this where we position the list?
        		if (pos == -1 && departure_time.compareTo(timenow) >= 0) {
        			pos = count;
        			nextdeparture = departure_time;
        		}
      	        count++;
        	}
  	        more = csr.moveToNext();
        }
        csr.close();

        BustimesArrayAdapter adapter = new BustimesArrayAdapter(this, details);
    	setListAdapter(adapter);

    	// Calculate the time difference
    	Toast msg;
    	if (pos >= 0) {
    		int hourdiff = Integer.parseInt(nextdeparture.substring(0, 2));
    		hourdiff -= t.hour;
    		hourdiff *= 60;
    		int mindiff = Integer.parseInt(nextdeparture.substring(3, 5));
    		mindiff -= t.minute;
    		hourdiff += mindiff;
    		if (hourdiff >= 60)
    			msg = Toast.makeText(mContext, "Next bus leaves at " + nextdeparture, Toast.LENGTH_LONG);
    		else
    			msg = Toast.makeText(mContext, "Next bus leaves in " + hourdiff + " minutes", Toast.LENGTH_LONG);
    		getListView().setSelectionFromTop(pos, 50);	// position next bus just below top
    	} else {
        	setSelection(count); // position the list at the last bus
    		msg = Toast.makeText(mContext, "No more busses today", Toast.LENGTH_LONG);
    	}
		msg.setGravity(Gravity.TOP, 0, 0);
		msg.show();
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
    		csr = BusstopsOverlay.DB.rawQuery("select count(*) from calendar where "
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
        MenuItem item = menu.findItem(R.id.menu_showallbusses);
        item.setEnabled(mShowTodayOnly);
        item = menu.findItem(R.id.menu_showtodaysbusses);
        item.setEnabled(!mShowTodayOnly);
        return true;
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_showallbusses: {
            	mShowTodayOnly = false;
            	// Just start again
                Intent intent = getIntent();
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                return true;
            }
            case R.id.menu_showtodaysbusses: {
            	mShowTodayOnly = true;
                Intent intent = getIntent();
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                return true;
            }
        }
        return false;
    }
}
