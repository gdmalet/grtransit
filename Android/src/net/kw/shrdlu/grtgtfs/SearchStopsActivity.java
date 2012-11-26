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

import android.app.AlertDialog;
import android.app.ListActivity;
import android.app.SearchManager;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;

public class SearchStopsActivity extends ListActivity {
	private static final String TAG = "BusstopsearchActivity";

	private SearchStopsActivity mContext;
	private TextView mTitle;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.timeslayout);

		// Log.v(TAG, "OnCreate()");
		mContext = this;

		final ListView lv = getListView();
		final TextView tv = new TextView(mContext);
		tv.setText(R.string.longpress_adds_stop);
		lv.addFooterView(tv);

		final Intent intent = getIntent();
		if (intent.getAction().equals(Intent.ACTION_SEARCH)) {
			final String query = intent.getStringExtra(SearchManager.QUERY);

			mTitle = (TextView) findViewById(R.id.timestitle);
			mTitle.setText("Stops matching `" + query + "'");

			Globals.tracker.trackEvent("Search", "Stop", query, 1);

			// register to get long clicks on bus stop list
			getListView().setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
				@Override
				public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
					onListItemLongClick(parent, view, position, id);
					return true; // we consumed the click
				}
			});

			new FindStops().execute(query);

		} else {
			// Called from another activity, so put up search box
			onSearchRequested();
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this Activity gets the focus.
		Globals.tracker.trackPageView("/" + this.getLocalClassName());
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		// Log.v(TAG, "clicked position " + position);

		final Cursor csr = (Cursor) l.getItemAtPosition(position);
		if (csr == null) {
			return;
		}
		final String stop_id = csr.getString(0);
		final String stop_name = csr.getString(1);
		final String pkgstr = mContext.getApplicationContext().getPackageName();

		// Intent busstop = new Intent(mContext, BusstopsActivity.class);
		// busstop.putExtra(pkgstr + ".stop_id", stop_id);
		// startActivity(busstop);

		final Intent routeselect = new Intent(mContext, RouteselectActivity.class);
		routeselect.putExtra(pkgstr + ".stop_id", stop_id);
		routeselect.putExtra(pkgstr + ".stop_name", stop_name);
		startActivity(routeselect);
	}

	// Called from the listener above for a long click
	public void onListItemLongClick(AdapterView<?> parent, View v, int position, long id) {
		// Log.v(TAG, "long clicked position " + position);

		final Cursor csr = (Cursor) parent.getItemAtPosition(position);
		final String stop_id = csr.getString(0);
		final String stop_name = csr.getString(1);

		final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int id) {
				switch (id) {
				case DialogInterface.BUTTON_POSITIVE:
					Globals.mPreferences.AddBusstopFavourite(stop_id, stop_name);
					// mContext.startActivity(new Intent(mContext, FavstopsActivity.class));
					break;
				// case DialogInterface.BUTTON_NEGATIVE:
				// // nothing
				// break;
				}
				dialog.cancel();
			}
		};

		final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
		builder.setTitle("Stop " + stop_id + ", " + stop_name);
		builder.setMessage("Add to your list of favourites?").setPositiveButton("Yes", listener)
				.setNegativeButton("No", listener).create().show();
	}

	private class FindStops extends AsyncTask<String, Void, Cursor> {
		// static final String TAG = "FindStops";

		@Override
		protected Cursor doInBackground(String... args) {
			// Log.v(TAG, "doInBackground()");

			final String query = args[0];

			final String table = "stops";
			final String[] columns = { "stop_id as _id", "stop_name as descr" };
			final String whereclause = "stop_id like '%' || ? || '%' or stop_name like '%' || ? || '%'";
			final String[] selectargs = { query, query };
			final Cursor csr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, null,
					null);
			startManagingCursor(csr);

			return csr;
		}

		@Override
		protected void onPostExecute(Cursor csr) {
			// Log.v(TAG, "onPostExecute()");

			final ListCursorAdapter adapter = new ListCursorAdapter(mContext, R.layout.stop_numanddesc, csr);
			setListAdapter(adapter);

		}
	}

}
