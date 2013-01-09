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
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

public class SearchActivity extends MenuListActivity {
	private static final String TAG = "SearchActivity";

	private EditText mSearchText;
	private Cursor mCsr;

	private int mSearchType = R.id.button_searchstops; // default
	private String mQuery;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;
		setContentView(R.layout.searchlayout);
		super.onCreate(savedInstanceState);

		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB /* 11 */) {
			// Remove search option from the fake actionbar.
			Button search = (Button) findViewById(R.id.button_search);
			if (search != null) {
				search.setClickable(false);
				search.setVisibility(View.GONE);
			}
		}

		mSearchText = (EditText) findViewById(R.id.searchtext);

		mTitle.setText(R.string.title_search);

		mSearchText.addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {
				// Log.d(TAG, "afterTextChanged");
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {
				// Log.d(TAG, "beforeTextChanged");
			}

			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				// Log.d(TAG, "onTextChanged");
				mQuery = new StringBuffer(s).toString();
				DoSearch(mSearchType, getIntent());
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB /* 11 */) {
			menu.removeItem(R.id.menu_search);
		}

		return true;
	}

	// Called when a button is clicked
	public void onButtonClick(View v) {
		final Intent intent = getIntent();

		switch (v.getId()) {
		case R.id.button_searchstops: {
			GRTApplication.tracker.trackEvent("Button", "Search stops", "", 1);
			DoSearch(v.getId(), intent);
			return;
		}
		case R.id.button_searchroutes: {
			GRTApplication.tracker.trackEvent("Button", "Search routes", "", 1);
			DoSearch(v.getId(), intent);
			return;
		}
		}
	}

	void DoSearch(int Id, Intent intent) {

		if (mQuery == null) {
			return;
		}

		mSearchType = Id; // Remember what we're doing: stops or routes
		final ListView lv = getListView();

		if (Id == R.id.button_searchstops) {

			// register to get long clicks on list
			getListView().setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
				@Override
				public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
					onListItemLongClick(parent, view, position, id);
					return true; // we consumed the click
				}
			});

			if (lv.getFooterViewsCount() < 1) {
				final TextView tv = new TextView(mContext);
				tv.setId(R.id.about_credits); // something that is not used in this layout
				tv.setText(R.string.longpress_adds_stop);
				lv.addFooterView(tv);
			}
			new FindStops().execute();

		} else if (Id == R.id.button_searchroutes) {

			if (lv.getFooterViewsCount() > 0) {
				final TextView tv = (TextView) findViewById(R.id.about_credits);
				if (tv != null) {
					lv.removeFooterView(tv);
				}
			}
			new FindRoutes().execute();

		} else {
			Log.e(TAG, "Search type is not stops or routes!?");
		}
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		// Log.v(TAG, "clicked position " + position);

		final Cursor csr = (Cursor) l.getItemAtPosition(position);
		if (csr == null) {
			return;
		}

		final String what = csr.getString(0);
		final String name = csr.getString(1);
		final String pkgstr = mContext.getApplicationContext().getPackageName();
		final Intent intent;

		if (mSearchType == R.id.button_searchstops) {

			intent = new Intent(mContext, RouteselectActivity.class);
			intent.putExtra(pkgstr + ".stop_id", what);
			intent.putExtra(pkgstr + ".stop_name", name);
			startActivity(intent);

		} else if (mSearchType == R.id.button_searchroutes) {

			intent = new Intent(mContext, RouteActivity.class);
			intent.putExtra(pkgstr + ".route_id", what);
			intent.putExtra(pkgstr + ".headsign", name); // TODO - this is not actually the headsign if using routes file...
			// route.setFlags(Intent.FLAG_ACTIVITY_SINGLE_TOP);
			startActivity(intent);

		} else {
			Log.e(TAG, "Search type is not stops or routes!?");
		}
	}

	// Called from the listener above for a long click
	public void onListItemLongClick(AdapterView<?> parent, View v, int position, long id) {
		// Log.v(TAG, "long clicked position " + position);

		if (mSearchType != R.id.button_searchstops) {
			return; // only makes sense for stops
		}

		final Cursor csr = (Cursor) parent.getItemAtPosition(position);
		if (csr == null) {
			return;
		}
		final String stop_id = csr.getString(0);
		final String stop_name = csr.getString(1);

		final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int id) {
				switch (id) {
				case DialogInterface.BUTTON_POSITIVE:
					GRTApplication.mPreferences.AddBusstopFavourite(stop_id, stop_name);
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

	private class FindStops extends AsyncTask<Void, Void, Void> {
		// static final String TAG = "FindStops";

		@Override
		protected Void doInBackground(Void... foo) {

			final String table = "stops";
			final String[] columns = { "stop_id as _id", "stop_name as descr" };
			final String whereclause = "stop_id like '%' || ? || '%' or stop_name like '%' || ? || '%'";
			final String[] selectargs = { mQuery, mQuery };
			mCsr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, null, null);
			startManagingCursor(mCsr);

			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {
			mTitle.setText("Stops matching `" + mQuery + "'");
			ListCursorAdapter adapter = new ListCursorAdapter(mContext, R.layout.stop_numanddesc, mCsr);
			setListAdapter(adapter);
			// adapter.notifyDataSetChanged();
		}
	}

	private class FindRoutes extends AsyncTask<Void, Void, Void> {
		// static final String TAG = "FindStops";

		@Override
		protected Void doInBackground(Void... foo) {
			// TODO Need to deal with search for `7A', where the 7 is the route_id,
			// and the A is the start of the trip_headsign.

			final String table = "trips";
			final String[] columns = { "distinct route_id as _id", "trip_headsign as descr" };
			final String whereclause = "route_id like '%' || ? || '%' or trip_headsign like '%' || ? || '%'";
			final String[] selectargs = { mQuery, mQuery };
			final String orderby = "cast(route_id as integer)";
			mCsr = DatabaseHelper.ReadableDB().query(table, columns, whereclause, selectargs, null, null, orderby, null);
			startManagingCursor(mCsr);

			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {
			mTitle.setText("Routes matching `" + mQuery + "'");
			ListCursorAdapter adapter = new ListCursorAdapter(mContext, R.layout.route_numanddesc, mCsr);
			setListAdapter(adapter);
			// adapter.notifyDataSetChanged();
		}
	}

	// Called when a button is clicked on the title bar
	@Override
	public void onTitlebarClick(View v) {
		TitlebarClick.onTitlebarClick(mContext, v);
	}
}
