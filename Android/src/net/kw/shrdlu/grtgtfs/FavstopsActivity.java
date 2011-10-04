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

import java.util.ArrayList;

import android.app.AlertDialog;
import android.app.ListActivity;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.os.Bundle;
import android.util.Log;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

public class FavstopsActivity extends ListActivity {
	private static final String TAG = "FavstopsActivity";

	private ListActivity mContext;
	private ArrayList<Pair<String,String>> mDetails;
	private String mStopid;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    	Log.v(TAG, "OnCreate()");
        mContext = this;

        // Some global initialisation
        if (Globals.mPreferences == null) {
        	Globals.mPreferences = new Preferences(mContext);
    		Globals.dbHelper = new DatabaseHelper(mContext);	// this can trigger a time-consuming update
        }
        
        setContentView(R.layout.timeslayout);
        TextView v = (TextView) findViewById(R.id.timestitle);
        v.setText(R.string.favourites_title);

        // Hide the `Show' button used for showing routes.
        Button btn = (Button) findViewById(R.id.timesbutton);
        btn.setVisibility(View.GONE);

        ProcessStops();
	}
	
	/* Separate the processing of stops, so we can re-do it when we need
	 * to refresh the screen on a new intent.
	 */
	protected void ProcessStops() {
		mDetails = Globals.mPreferences.GetBusstopFavourites();

		// Must do all this without doing a database read, which allows database upgrade
		// to happen in the background on a service thread, without us blocking, until
		// we really have to.
        if (!mDetails.isEmpty()) {
        	
            BustimesArrayAdapter adapter = new BustimesArrayAdapter(this, mDetails);
        	setListAdapter(adapter);
        
	        // register to get long clicks on bus stop list
	        getListView().setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
				public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
	        	    Log.i(TAG, "onItemLongClickClick position " + position);
	        	    onListItemLongClick(parent, view, position, id);
	        		return true;	// to say we consumed the click
				}
	        });
        }
	}
	
	// This is called when we use CLEAR_TOP, to flush the stack and redraw the list,
	// which is necessary when the list changes.
	@Override
	protected void onNewIntent(Intent intent) {
		View v = findViewById(R.id.detail_area);
		v.invalidate();
		ProcessStops();
	}
	
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.busstopsmenu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.menu_location: {
        		Intent stops = new Intent(getIntent());
        		stops.setClass(mContext, BusstopsActivity.class);
        		stops.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
        		stops.setFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        		startActivity(stops);
        		return true;
            }
            case R.id.menu_about: {
                Globals.showAbout(this);
                return true;
            }
            case R.id.menu_searchstops: {
        		Intent stopsearch = new Intent(getIntent());
        		stopsearch.setClass(mContext, BusstopsearchActivity.class);
        		stopsearch.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
        		stopsearch.setFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        		startActivity(stopsearch);
        		return true;
            }
          case R.id.menu_searchroutes: {
        		Intent routesearch = new Intent(getIntent());
        		routesearch.setClass(mContext, RoutesearchActivity.class);
        		routesearch.setAction(Intent.ACTION_MAIN); // anything other than SEARCH
        		routesearch.setFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
        		startActivity(routesearch);
        		return true;
            }
        }
        return false;
    }

	// Called from the listener above for a long click
	protected void onListItemLongClick(AdapterView<?> parent, View v, int position, long id) {
		Log.v(TAG, "long clicked position " + position);
		
		final Pair<String,String> pair = (Pair<String,String>)parent.getItemAtPosition(position);
		mStopid = pair.first;
		final String stop_name = pair.second;
		final int aryposn = position;	// so we can access it in the listener class.
		
		DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int id) {
				switch (id) {
				case DialogInterface.BUTTON_POSITIVE:
					Globals.mPreferences.RemoveBusstopFavourite(mStopid);
					mDetails.remove(aryposn);
					// activities in the stack may contain out of date lists, so flush and start again.
					mContext.startActivity(new Intent(mContext, FavstopsActivity.class));
					break;
//				case DialogInterface.BUTTON_NEGATIVE:
//					// nothing
//					break;
				}
				dialog.cancel();
			}
		};

		AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
		builder.setTitle("Stop " + mStopid + ", " + stop_name); 
		builder.setMessage("Remove from your list of favourites?")
		.setPositiveButton("Yes", listener)
		.setNegativeButton("No", listener);
		builder.create();
		builder.show();
	}
		
	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		Log.v(TAG, "clicked position " + position);
		
		final Pair<String,String> pair = (Pair<String,String>)l.getItemAtPosition(position);
		mStopid = pair.first;
		final String stop_name = pair.second;
		
		Intent routeselect = new Intent(mContext, RouteselectActivity.class);
		String pkgstr = mContext.getApplicationContext().getPackageName();
		routeselect.putExtra(pkgstr + ".stop_id", mStopid);
		routeselect.putExtra(pkgstr + ".stop_name", stop_name);
		mContext.startActivity(routeselect);
	}
}
