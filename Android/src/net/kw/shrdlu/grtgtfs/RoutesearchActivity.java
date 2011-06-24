package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.app.SearchManager;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

public class RoutesearchActivity extends ListActivity {
	private static final String TAG = "RoutesearchActivity";

	private RoutesearchActivity mContext;
    private TextView mTitle;
	private Cursor mCsr;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
       	setContentView(R.layout.timeslayout);

    	Log.v(TAG, "OnCreate()");
    	mContext = this;
    	
        Intent intent = getIntent();
        if (intent.getAction().equals(Intent.ACTION_SEARCH)) {
            String query = intent.getStringExtra(SearchManager.QUERY);

            mTitle = (TextView) findViewById(R.id.timestitle);
            mTitle.setText("Routes matching `" + query + "'");
        
            // Hide the `Show' button used for showing routes.
            Button btn = (Button) findViewById(R.id.timesbutton);
            btn.setVisibility(View.GONE);
            String q = String.format(
            		"select distinct route_id as _id, trip_headsign as descr from trips "
            		+ "where route_id like \"%s%%\" or trip_headsign like \"%%%s%%\" "
            		+ "order by cast(route_id as integer)",
            		query, query);
//			TODO Or use the routes file?            
//        		"select route_id as _id, route_long_name as descr from routes where route_id like \"%s%%\" or route_long_name like \"%%%s%%\"",
//        		query, query);
            mCsr = BusstopsOverlay.DB.rawQuery(q, null);
            startManagingCursor(mCsr);

	        SearchCursorAdapter adapter = new SearchCursorAdapter(this, mCsr);
	    	setListAdapter(adapter);
        } else {
        	// Called from another activity, so put up search box
        	onSearchRequested();
        }
    }
	
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		Log.v(TAG, "clicked position " + position + ", route number " + id);
		
        String pkgstr = mContext.getApplicationContext().getPackageName();
        String routestr = pkgstr + ".route_id";
        String headsign = pkgstr + ".headsign";

		Intent route = new Intent(mContext, BusroutesActivity.class);
		route.putExtra(routestr, Integer.toString((int)id));
		mCsr.moveToPosition(position);
		route.putExtra(headsign, mCsr.getString(1));	// TODO - this is not actually the headsign if using routes file...
        route.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
		startActivity(route);
	}
}
