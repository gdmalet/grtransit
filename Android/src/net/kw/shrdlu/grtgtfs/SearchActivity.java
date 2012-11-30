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
import android.os.AsyncTask;
import android.os.Bundle;
import android.view.View;
import android.widget.TextView;

public class SearchActivity extends ListActivity {
	private static final String TAG = "SearchActivity";

	private SearchActivity mContext;
	private TextView mTitle;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.searchlayout);

		mContext = this;

		final Intent intent = getIntent();
		final String action = intent.getAction();
		if (action != null && action.equals(Intent.ACTION_SEARCH)) {
			final String query = intent.getStringExtra(SearchManager.QUERY);

			// mTitle = (TextView) findViewById(R.id.timestitle);
			// mTitle.setText("Stops matching `" + query + "'");

			Globals.tracker.trackEvent("Search", "Stop", query, 1);

			new FindStops().execute(query);

		} else {
			// Called from another activity, so put up search box
			onSearchRequested();
		}
	}

	// Since we have SingleTop, this gets called when the user hits search after entering a query.
	@Override
	protected void onNewIntent(Intent intent) {
		setIntent(intent);
		final String query = intent.getStringExtra(SearchManager.QUERY);
		Globals.tracker.trackEvent("Search", "Stop", query, 1);
		new FindStops().execute(query);
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this Activity gets the focus.
		Globals.tracker.trackPageView("/" + this.getLocalClassName());
	}

	// Called when a button is clicked
	public void onButtonClick(View v) {
		final Intent intent = getIntent();

		switch (v.getId()) {
		case R.id.button_searchstops: {
			Globals.tracker.trackEvent("Button", "Search stops", "", 1);
			intent.setClass(mContext, SearchStopsActivity.class);
			startActivity(intent);
			return;
		}
		case R.id.button_searchroutes: {
			Globals.tracker.trackEvent("Button", "Search routes", "", 1);
			intent.setClass(mContext, SearchRoutesActivity.class);
			startActivity(intent);
			return;
		}
		}
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
