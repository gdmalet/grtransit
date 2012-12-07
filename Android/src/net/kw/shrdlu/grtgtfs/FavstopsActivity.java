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
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.StrictMode;
import android.text.format.Time;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.ProgressBar;
import android.widget.TextView;

public class FavstopsActivity extends ListActivity implements AnimationListener {
	private static final String TAG = "FavstopsActivity";

	// Need one instance of this
	private static Globals mGlobals = null;

	private ListActivity mContext;
	private ArrayList<String[]> mDetails;
	private Animation mSlideIn, mSlideOut;
	private String mStopid;
	private View mListDetail;
	private ProgressBar mProgress;
	private FavstopsArrayAdapter mAdapter;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		// Do this before instantiating Globals, as that may do something we'd like
		// to see by having StrictMode on already.
		if (Globals.CheckDebugBuild(this) && Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD /* 9 */) {
			API9ReflectionWrapper.setStrictMode();
		} else {
			// Log.d(TAG,"Not setting up strict mode.");
		}

		super.onCreate(savedInstanceState);
		// Log.v(TAG, "OnCreate()");

		mContext = this;
		if (mGlobals == null) {
			mGlobals = new Globals(mContext);
		}

		setContentView(R.layout.timeslayout);

		// Load the necessary to show/hide progress bar
		mProgress = (ProgressBar) findViewById(R.id.progress);
		mListDetail = findViewById(R.id.detail_area);
		mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);

		final ListView lv = getListView();
		final TextView tv = new TextView(mContext);

		tv.setText(R.string.longpress_removes_stop);
		lv.addFooterView(tv);

		// ProcessStops(); // will be done in onResume()
	}

	/* Wrap calls to functions that may not be in the version of the OS that we're running. This class is only instantiated if
	 * we refer to it, at which point Dalvik would discover the error. So don't refer to it if we know it will fail.... */
	private static class API9ReflectionWrapper {
		public static void setStrictMode() {
			StrictMode.setThreadPolicy(new StrictMode.ThreadPolicy.Builder()
			// .detectDiskReads()
			// .detectDiskWrites()
					.detectNetwork()
					// .penaltyFlashScreen()
					.build());
			StrictMode.setVmPolicy(new StrictMode.VmPolicy.Builder().detectLeakedSqlLiteObjects()
			// .detectLeakedClosableObjects()
					.penaltyLog().penaltyDeath().build());
		}
	}

	/* Separate the processing of stops, so we can re-do it when we need to refresh the screen on a new intent. */
	static boolean mShownalert = false;

	protected void ProcessStops() {

		mDetails = new ArrayList<String[]>();
		final ArrayList<String[]> favstops = Globals.mPreferences.GetBusstopFavourites();
		// Convert from stop/description to required 4-entry layout.
		synchronized (mDetails) {
			for (final String[] stop : favstops) {
				// Just do what we can for now
				mDetails.add(new String[] { stop[0], stop[1], "", getString(R.string.loading_times), "?" });
			}
		}
		mAdapter = new FavstopsArrayAdapter(this, R.layout.favouritesrow, mDetails);
		setListAdapter(mAdapter);

		// Must do all this without doing a database read, which allows database upgrade
		// to happen in the background on a service thread, without us blocking, until
		// we really have to.
		if (!mDetails.isEmpty()) {

			// register to get long clicks on bus stop list
			getListView().setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
				@Override
				public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
					// Log.i(TAG, "onItemLongClickClick position " + position);
					onListItemLongClick(parent, view, position, id);
					return true; // to say we consumed the click
				}
			});

			// Load times of next bus for each stop.
			new LoadTimes().execute();

		} else if (!mShownalert) {
			mShownalert = true;

			TextView textView;
			final View messageView = mContext.getLayoutInflater().inflate(R.layout.about, null, false);

			textView = (TextView) messageView.findViewById(R.id.about_header);
			textView.setVisibility(TextView.GONE);
			textView = (TextView) messageView.findViewById(R.id.about_credits);
			textView.setText(R.string.no_favourites);
			textView.setHorizontallyScrolling(false); // make text wrap.

			final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
			// builder.setIcon(R.drawable.grticon);
			builder.setTitle(R.string.title_favourites).setView(messageView).create().show();
		}
	}

	// If we're popping back down the stack, the favourites list could have been added to
	// since we were last here, so make sure it is reloaded before display.
	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this activity gets the focus - but if the activity was
		// previously destroyed we could have lost our global data, so this is a bit of a hack to avoid a crash!
		if (Globals.tracker == null) {
			Log.e(TAG, "null tracker!");
			startActivity(new Intent(this, FavstopsActivity.class));
		} else {
			Globals.tracker.trackPageView("/" + this.getLocalClassName());
		}

		findViewById(R.id.detail_area).invalidate();
		ProcessStops();
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		Globals.tracker.dispatch(); // perhaps unnecessary?
		Globals.tracker.stopSession();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.busstopsmenu, menu);

		return true;
	}

	// Should have a super class that defines and handles these menus, and
	// then derive this and other activities that use the same menus from that.
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_mylocation: {
			Globals.tracker.trackEvent("Menu", "My location", "", 1);
			startActivity(new Intent(mContext, StopsActivity.class));
			return true;
		}
		case R.id.menu_preferences: {
			Globals.tracker.trackEvent("Menu", "Preferences", "", 1);
			final Intent prefs = new Intent(mContext, PrefsActivity.class);
			startActivity(prefs);
			return true;
		}
		case R.id.menu_closeststops: {
			Globals.tracker.trackEvent("Menu", "Closest stops", "", 1);
			final Intent stops = new Intent(mContext, ClosestStopsActivity.class);
			startActivity(stops);
			return true;
		}
		case R.id.menu_about: {
			Globals.tracker.trackEvent("Menu", "Show about", "", 1);
			Globals.showAbout(this);
			return true;
		}
		}
		return super.onOptionsItemSelected(item);
	}

	// Called from the listener above for a long click
	protected void onListItemLongClick(AdapterView<?> parent, View v, int position, long id) {
		// Log.v(TAG, "long clicked position " + position);

		final String[] strs = (String[]) parent.getItemAtPosition(position);
		mStopid = strs[0];
		final String stop_name = strs[1];
		final int aryposn = position; // so we can access it in the listener class.

		final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int id) {
				switch (id) {
				case DialogInterface.BUTTON_POSITIVE:
					Globals.mPreferences.RemoveBusstopFavourite(mStopid);
					synchronized (mDetails) {
						mDetails.remove(aryposn);
					}
					// activities in the stack may contain out of date lists, so flush and start again.
					mContext.startActivity(new Intent(mContext, FavstopsActivity.class));
					break;
				// case DialogInterface.BUTTON_NEGATIVE:
				// // nothing
				// break;
				}
				dialog.cancel();
			}
		};

		final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
		builder.setTitle("Stop " + mStopid + ", " + stop_name);
		builder.setMessage("Remove from your list of favourites?").setPositiveButton("Yes", listener)
				.setNegativeButton("No", listener);
		builder.create();
		builder.show();
	}

	@Override
	protected void onListItemClick(ListView l, View v, int position, long id) {
		// Log.v(TAG, "clicked position " + position);

		final String[] strs = (String[]) l.getItemAtPosition(position);
		if (strs == null) {
			return;
		}
		mStopid = strs[0];
		final String stop_name = strs[1];

		Globals.tracker.trackEvent("Favourites", "Select stop", mStopid, 1);

		final Intent routeselect = new Intent(mContext, RouteselectActivity.class);
		final String pkgstr = mContext.getApplicationContext().getPackageName();
		routeselect.putExtra(pkgstr + ".stop_id", mStopid);
		routeselect.putExtra(pkgstr + ".stop_name", stop_name);
		mContext.startActivity(routeselect);
	}

	/* Do the processing to load the ArrayAdapter for display. */
	private class LoadTimes extends AsyncTask<Void, Integer, Void> {

		@Override
		protected void onPreExecute() {
			mListDetail.startAnimation(mSlideIn);
			mProgress.setVisibility(View.VISIBLE);
			mProgress.setProgress(5); // make partially visible
		}

		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
			mAdapter.notifyDataSetChanged();
		}

		@Override
		protected Void doInBackground(Void... foo) {

			// Find time of next bus for each stop.
			final Time t = new Time(); // TODO - this duplicates BusTimes?
			t.setToNow();
			final String datenow = String.format("%04d%02d%02d", t.year, t.month + 1, t.monthDay);

			Integer progresscount = 0;
			synchronized (mDetails) {
				for (final String[] pref : mDetails) {
					final String stopid = pref[0];
					// final String stopdescr = pref[1];

					// Log.d(TAG, "Searching for busses for stop " + stopid + " " + stopdescr);
					final String[] nextbus = ServiceCalendar.getNextDepartureTime(stopid, datenow);
					if (nextbus != null) {
						// Log.d(TAG, "Next bus for stop " + stopid + ": " + nextbus[0] + " " + nextbus[1] + " - " +
						// nextbus[2]);
						pref[2] = nextbus[0]; // time
						pref[3] = nextbus[2]; // route headsign
						pref[4] = nextbus[1]; // route number
					} else {
						// Log.d(TAG, "Next bus for stop " + stopid + ": --none--");
						pref[2] = " -- -- --"; // time
						pref[3] = getString(R.string.no_more_busses); // route details
						pref[4] = "-";
					}
					publishProgress(++progresscount * 100 / mDetails.size());
				}
			}
			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {
			mProgress.setVisibility(View.INVISIBLE);
			mListDetail.startAnimation(mSlideOut);

			final TextView v = (TextView) findViewById(R.id.listtitle);
			v.setText(R.string.title_favourites);
		}
	}

	@Override
	public void onAnimationEnd(Animation animation) {
	}

	@Override
	public void onAnimationRepeat(Animation animation) {
		// Not interested if the animation repeats
	}

	@Override
	public void onAnimationStart(Animation animation) {
		// Not interested when the animation starts
	}

	// Called when a button is clicked on the title bar
	public void onTitlebarClick(View v) {
		TitlebarClick.onTitlebarClick(mContext, v);
	}

}
