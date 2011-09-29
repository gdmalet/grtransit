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
	private Cursor mCsr;
	private String mStopid;
	
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    	Log.v(TAG, "OnCreate()");
        mContext = this;

        // Some global initialisation
        if (Globals.mPreferences == null) {
        	Globals.mPreferences = new Preferences(mContext);
    		Globals.dbHelper = new DatabaseHelper(mContext);
        }
        
        setContentView(R.layout.timeslayout);
        TextView v = (TextView) findViewById(R.id.timestitle);
        v.setText(R.string.favourites_title);

        // Hide the `Show' button used for showing routes.
        Button btn = (Button) findViewById(R.id.timesbutton);
        btn.setVisibility(View.GONE);
        
        String [] stops = Globals.mPreferences.GetBusstopFavourites();

        if (stops != null) {
        	
        	// Build a string suitable for a select statement
        	String stopsqry = new String();
        	for (String s : stops) {
        		if (stopsqry.length() > 0)
        			stopsqry += ",";
        		stopsqry += "'" + s + "'";        		
        	}

        	Cursor csr = DatabaseHelper.ReadableDB().rawQuery(
            		"select stop_id as _id, stop_name from stops where stop_id in (" + stopsqry + ")", null);
            
        	// Load the array for the list
        	mDetails = new ArrayList<Pair<String,String>>(csr.getCount());
            Pair<String,String> pair;

            boolean more = csr.moveToFirst();
            while (more) {
            	String stop_id = csr.getString(0);
            	String stop_name = csr.getString(1);
        		pair = new Pair<String,String>(stop_id, stop_name);
        		mDetails.add(pair);
      	        more = csr.moveToNext();
            }

            csr.close();
            
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

    // This is used when a route number is clicked on in the dialog, after a stop is clicked.
	private DialogInterface.OnClickListener mClick = new DialogInterface.OnClickListener() {
		  public void onClick(DialogInterface dialog, int which) {
			  if (mCsr.moveToPosition(which)) {
				  String route = mCsr.getString(0);
//				  Log.v(TAG, "clicked position " + which + ": route " + route);

				  int split = route.indexOf(" - ");
				  String route_id = route.substring(0,split);
				  String headsign = route.substring(split+3);

				  Intent bustimes = new Intent(mContext, BustimesActivity.class);
				  String pkgstr = mContext.getApplicationContext().getPackageName();
				  bustimes.putExtra(pkgstr + ".route_id", route_id);
				  bustimes.putExtra(pkgstr + ".headsign", headsign);
				  bustimes.putExtra(pkgstr + ".stop_id", mStopid);
				  mContext.startActivity(bustimes);
			  }
		  }
	};

	// Called from the listener above for a long click
	protected void onListItemLongClick(AdapterView<?> parent, View v, int position, long id) {
		Log.v(TAG, "long clicked position " + position);
		
		final Pair<String,String> pair = (Pair<String,String>)parent.getItemAtPosition(position);
		final String stop_id = pair.first;;
		final String stop_name = pair.second;
		final int aryposn = position;	// so we can access it in the listener class.
		
		DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int id) {
				switch (id) {
				case DialogInterface.BUTTON_POSITIVE:
					Globals.mPreferences.RemoveBusstopFavourite(stop_id);
					mDetails.remove(aryposn);
//					mContext.invalidate();
// TODO redraw
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
		
        Pair<String,String> pair = (Pair<String,String>)l.getItemAtPosition(position);
		mStopid = pair.first;;
		String stop_name = pair.second;
		
		// Find which routes use the given stop.
		final String table = "trips";
		final String [] select = {"distinct route_id || \" - \" || trip_headsign as _id"};
		final String where = "trip_id in (select trip_id from stop_times where stop_id = ?)";
		final String [] selectargs = {mStopid};
		mCsr = DatabaseHelper.ReadableDB().query(table, select, where, selectargs, null,null,null);
		startManagingCursor(mCsr);
		
		AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
		builder.setTitle("Routes using stop " + mStopid + ", " + stop_name); 
		builder.setCursor(mCsr, mClick, "_id");
		builder.show();
	}
}
