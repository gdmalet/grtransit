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
        
            final String table = "stops";
            final String [] columns = {"stop_id as _id", "stop_name as descr"};
            final String whereclause = "stop_id like ? || '%' or stop_name like '%' || ? || '%'";
            String [] selectargs = {query, query};
            Cursor csr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, null, null);
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
		busstop.putExtra(stopstr, l.getItemIdAtPosition(position));
        busstop.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
		startActivity(busstop);
	}
}
