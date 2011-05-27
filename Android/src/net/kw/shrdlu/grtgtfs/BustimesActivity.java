package net.kw.shrdlu.grtgtfs;

import ca.uwaterloo.android.UWWeather.ListArrayAdapter;
import android.app.ListActivity;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.widget.CursorAdapter;
import android.widget.ListView;
import android.widget.ResourceCursorAdapter;
import android.widget.SimpleCursorAdapter;

public class BustimesActivity extends ListActivity {
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.timesview);
        
        Intent intent = getIntent();
        String route_id = intent.getStringExtra("route_id");
        String headsign = intent.getStringExtra("headsign");
        String stop_id = intent.getStringExtra("stop_id");
        
        String q = String.format(
        		"select departure_time as _id from stop_times where stop_id = \"%s\" and trip_id in (select trip_id from trips where route_id = \"%s\" and trip_headsign = \"%s\") order by departure_time",
        		stop_id, route_id, headsign);
        Cursor csr = GrtGtfs.DB.rawQuery(q, null);
        
    	setListAdapter(new ListCursorAdapter(this, csr));

        // TODO - not advised on main thread.
//        String [] froms = { "_id" };
//        int [] tos = { R.id.time };
//        SimpleCursorAdapter ca = new SimpleCursorAdapter(this, R.layout.rowlayout, csr, froms, tos);
//        setListAdapter(ca);

//        ListView lv = (ListView) findViewById(R.id.bustimes);
//        lv.
    }

}
