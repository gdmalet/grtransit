package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.util.Log;

public class BustimesActivity extends ListActivity {
	private static final String TAG = "BustimesActivity";
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    	Log.v(TAG, "OnCreate()");
    	
        setContentView(R.layout.timeslayout);
//        setContentView(R.layout.testlayout);

        Intent intent = getIntent();
        String route_id = intent.getStringExtra("route_id");
        String headsign = intent.getStringExtra("headsign");
        String stop_id = intent.getStringExtra("stop_id");
        
        String q = String.format(
        		"select departure_time as _id, trip_id from stop_times where stop_id = \"%s\" and trip_id in (select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\") order by departure_time",
        		stop_id, route_id, headsign);
        Cursor csr = GrtGtfs.DB.rawQuery(q, null);
        startManagingCursor(csr);
        
        ListCursorAdapter adapter = new ListCursorAdapter(this, csr);
    	setListAdapter(adapter);
    }

}
