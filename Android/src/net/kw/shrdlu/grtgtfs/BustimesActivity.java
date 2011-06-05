package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.text.format.Time;
import android.util.Log;
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
        csr.moveToFirst();
        for (pos=0; pos<count; pos++) {
        	String csrtime = csr.getString(0);
        	Log.v(TAG, "compare \"" + csrtime + "\" to \"" + timenow + "\"");
        	if (csrtime.compareTo(timenow) >= 0)
        		break;
        	csr.moveToNext();
        }
    	setSelection(pos);
    	Toast.makeText(mContext, "set selection to " + pos, Toast.LENGTH_LONG).show();
    }
/*
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		Log.v(TAG, "clicked position " + position + ", id " + id);
		
		Intent busroutes = new Intent(this, BusroutesActivity.class);
		busroutes.putExtra("route_id", mRoute_id);
		busroutes.putExtra("headsign", mHeadsign);
		busroutes.putExtra("stop_id", mStop_id);
		startActivity(busroutes);
	}
*/	
}
