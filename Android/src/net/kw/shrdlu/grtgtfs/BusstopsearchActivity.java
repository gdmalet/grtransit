package net.kw.shrdlu.grtgtfs;

import android.app.ListActivity;
import android.app.SearchManager;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

public class BusstopsearchActivity extends ListActivity {
	private static final String TAG = "BusstopsearchActivity";

	private BusstopsearchActivity mContext;
    private TextView mTitle;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
       	setContentView(R.layout.timeslayout);

//    	Log.v(TAG, "OnCreate()");
    	mContext = this;
    	
        Intent intent = getIntent();
        if (intent.getAction().equals(Intent.ACTION_SEARCH)) {
            String query = intent.getStringExtra(SearchManager.QUERY);

            mTitle = (TextView) findViewById(R.id.timestitle);
            mTitle.setText("Stops matching `" + query + "'");
        
            // Hide the `Show' button used for showing routes.
            Button btn = (Button) findViewById(R.id.timesbutton);
            btn.setVisibility(View.GONE);
        
//          String q = String.format(
//    		"select stop_id as _id, stop_name as descr from stops where stop_id like \"%s%%\" or stop_name like \"%%%s%%\"",
//    		query, query);
//          Cursor csr = BusstopsOverlay.DB.rawQuery(q, null);

            final String table = "stops";
            final String [] columns = {"stop_id as _id", "stop_name as descr"};
            final String whereclause = "stop_id like ? || '%' or stop_name like '%' || ? || '%'";
            String [] selectargs = {query, query};
            Cursor csr = BusstopsOverlay.DB.query(table, columns, whereclause, selectargs, null, null, null, null);
            startManagingCursor(csr);

	        SearchCursorAdapter adapter = new SearchCursorAdapter(this, csr);
	    	setListAdapter(adapter);
        } else {
        	// Called from another activity, so put up search box
        	onSearchRequested();
        }	
    }
	
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
//		Log.v(TAG, "clicked position " + position + ", stop number " + id);
		
        String stopstr = mContext.getApplicationContext().getPackageName() + ".stop_id";
		Intent busstop = new Intent(mContext, BusstopsActivity.class);
		busstop.putExtra(stopstr, Integer.toString((int)id));
        busstop.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
		startActivity(busstop);
	}
}
