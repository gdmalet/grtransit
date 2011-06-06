package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
import android.view.Gravity;
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

        BustimesCursorAdapter adapter = new BustimesCursorAdapter(this, csr);
    	setListAdapter(adapter);

    	// Find where to position the list
    	Time t = new Time();
    	t.setToNow();
    	String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);
        int count = csr.getCount(), pos;
    	String csrtime = null;
        csr.moveToFirst();
        for (pos=0; pos<count; pos++) {
        	csrtime = csr.getString(0);
        	Log.v(TAG, "compare \"" + csrtime + "\" to \"" + timenow + "\"");
        	if (csrtime.compareTo(timenow) >= 0)
        		break;
        	csr.moveToNext();
        }
    	
    	// Calculate the time difference
    	Toast msg;
    	if (pos < count) {
        	setSelection(pos); // position the list at the next bus
    		int hourdiff = Integer.parseInt(csrtime.substring(0, 2));
    		hourdiff -= t.hour;
    		hourdiff *= 60;
    		int mindiff = Integer.parseInt(csrtime.substring(3, 5));
    		mindiff -= t.minute;
    		hourdiff += mindiff;
    		msg = Toast.makeText(mContext, "Next bus leaves in " + hourdiff + " minutes", Toast.LENGTH_LONG);
    	} else {
    		msg = Toast.makeText(mContext, "No more busses today", Toast.LENGTH_LONG);
    	}
		msg.setGravity(Gravity.TOP, 0, 0);
		msg.show();
    }
}
