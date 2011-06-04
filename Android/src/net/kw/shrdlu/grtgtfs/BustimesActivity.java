package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ListView;

public class BustimesActivity extends ListActivity {
	private static final String TAG = "BustimesActivity";

	private String mRoute_id, mHeadsign, mStop_id;

	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    	Log.v(TAG, "OnCreate()");
    	
        setContentView(R.layout.timeslayout);

        Intent intent = getIntent();
        mRoute_id = intent.getStringExtra("route_id");
        mHeadsign = intent.getStringExtra("headsign");
        mStop_id = intent.getStringExtra("stop_id");
        
        String q = String.format(
        		"select departure_time as _id, trip_id from stop_times where stop_id = \"%s\" and trip_id in (select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\") order by departure_time",
        		mStop_id, mRoute_id, mHeadsign);
        Cursor csr = BusstopsOverlay.DB.rawQuery(q, null);
        startManagingCursor(csr);
        
        BustimesCursorAdapter adapter = new BustimesCursorAdapter(this, csr);
    	setListAdapter(adapter);
    }

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		Log.v(TAG, "clicked position " + position + ", id " + id);
		
		Intent busroutes = new Intent(this, BusroutesActivity.class);
		busroutes.putExtra("route_id", mRoute_id);
		busroutes.putExtra("headsign", mHeadsign);
		busroutes.putExtra("stop_id", mStop_id);
		startActivity(busroutes);
	}
}
