package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;

import com.google.android.maps.MapView;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
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
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

public class BustimesActivity extends ListActivity {
	private static final String TAG = "BustimesActivity";

	private ListActivity mContext;
	private String mRoute_id, mHeadsign, mStop_id;
    private TextView mTitle;
	private ServiceCalendar mServiceCalendar = new ServiceCalendar();
	
	private static boolean mShowTodayOnly = true;

	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mContext = this;
    	Log.v(TAG, "OnCreate()");
    	
        setContentView(R.layout.timeslayout);

        final Button button = (Button) findViewById(R.id.timesbutton);
        button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                // Perform action on click
        		Intent busroutes = new Intent(mContext, BusroutesActivity.class);
        		busroutes.putExtra("route_id", mRoute_id);
        		busroutes.putExtra("headsign", mHeadsign);
        		busroutes.putExtra("stop_id", mStop_id);
        		startActivity(busroutes);
            }
        });
        
        Intent intent = getIntent();
        mRoute_id = intent.getStringExtra("route_id");
        mHeadsign = intent.getStringExtra("headsign");
        mStop_id = intent.getStringExtra("stop_id");
        
        mTitle = (TextView) findViewById(R.id.timestitle);
        mTitle.setText(mRoute_id + " - " + mHeadsign);
        
        String q = String.format(
        		"select departure_time as _id, trip_id from stop_times where stop_id = \"%s\" and trip_id in (select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\") order by departure_time",
        		mStop_id, mRoute_id, mHeadsign);
        Cursor csr = BusstopsOverlay.DB.rawQuery(q, null);
        startManagingCursor(csr);

    	// Will find where to position the list of bus departure times
    	Time t = new Time();
    	t.setToNow();
    	String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);
    	
    	// Todo -- should allow this to be selected.
    	String datenow = String.format("%04d%02d%02d", t.year, t.month+1, t.monthDay);

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
    		q = String.format("select service_id from trips where trip_id = \"%s\"", trip_id);
            Cursor svs = BusstopsOverlay.DB.rawQuery(q, null);
            svs.moveToFirst();
            String service_id = svs.getString(0);
            svs.close();
        	String daysstr = mServiceCalendar.getDays(service_id, datenow, mShowTodayOnly);

        	// Only add if the bus runs on this day.
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
        	setSelection(pos); // position the list at the next bus
    		int hourdiff = Integer.parseInt(nextdeparture.substring(0, 2));
    		hourdiff -= t.hour;
    		hourdiff *= 60;
    		int mindiff = Integer.parseInt(nextdeparture.substring(3, 5));
    		mindiff -= t.minute;
    		hourdiff += mindiff;
    		msg = Toast.makeText(mContext, "Next bus leaves in " + hourdiff + " minutes", Toast.LENGTH_LONG);
    	} else {
    		msg = Toast.makeText(mContext, "No more busses today", Toast.LENGTH_LONG);
    	}
		msg.setGravity(Gravity.TOP, 0, 0);
		msg.show();
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
