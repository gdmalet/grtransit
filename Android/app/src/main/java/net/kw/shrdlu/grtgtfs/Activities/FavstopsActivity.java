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

package net.kw.shrdlu.grtgtfs.Activities;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.sqlite.SQLiteDatabase;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.Time;
import android.view.LayoutInflater;
import android.view.View;
import android.view.Window;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.google.android.gms.analytics.HitBuilders;

import net.kw.shrdlu.grtgtfs.BuildConfig;
import net.kw.shrdlu.grtgtfs.DatabaseHelper;
import net.kw.shrdlu.grtgtfs.GRTApplication;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.Realtime;
import net.kw.shrdlu.grtgtfs.ServiceCalendar;

import java.util.ArrayList;
import java.util.concurrent.Semaphore;

public class FavstopsActivity extends MenuListActivity {
    private static final String TAG = "FavstopsActivity";
    /* Separate the processing of stops, so we can re-do it when we need to refresh the screen on a new intent. */
    private static boolean mShownalert = false;
    final View.OnLongClickListener mLongClickListener = new View.OnLongClickListener() {
        @Override
        public boolean onLongClick(View view) {
            onListItemLongClick(view);
            return true; // to say we consumed the click
        }
    };
    private final SQLiteDatabase DB = DatabaseHelper.ReadableDB();
    private LinearLayout layout;
    private ArrayList<String[]> mDetails;
    private String mStopid;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        mContext = this;

        // Will use the action bar progress bar
        requestWindowFeature(Window.FEATURE_PROGRESS);

        setContentView(R.layout.emptylayout);
        super.onCreate(savedInstanceState);

        layout = (LinearLayout) findViewById(R.id.vg_area);

		/* Make sure we can access a database */
        if (DB == null) {
            final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int id) {
                    mContext.finish();
                }
            };
            final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
            builder.setTitle(R.string.db_is_awol)
                    .setMessage(R.string.db_not_avail)
                    .setNegativeButton(R.string.exit, listener)
                    .create()
                    .show();
        }

        // ProcessStops(); // will be done in onResume()
    }

    void ProcessStops() {

        mDetails = new ArrayList<>();
        final ArrayList<String[]> favstops = GRTApplication.mPreferences.GetBusstopFavourites();
        // Convert from stop/description to required 4-entry layout.
        synchronized (mDetails) {
            for (final String[] stop : favstops) {
                // Just do what we can for now
                mDetails.add(new String[]{stop[0], stop[1], "", getString(R.string.loading_times), "?", "", ""});
            }
        }

        // TODO synchronising on non-final var... and accessing that var outside the lock, above & below.
        // See http://stackoverflow.com/questions/21458625/when-a-lock-holds-a-non-final-object-can-the-objects-reference-still-be-change/21460055#21460055

        // Must do all this without doing a database read, which allows database upgrade
        // to happen in the background on a service thread, without us blocking, until
        // we really have to.

        // Load times of next bus for each stop.
        if (!mDetails.isEmpty())
            new LoadTimes().execute();

        // Tell 'em to search for stops to put on the favs screen.
        if (mDetails.isEmpty() && !mShownalert) {
            mShownalert = true;

            TextView textView;
            final View messageView = mContext.getLayoutInflater().inflate(R.layout.about, null, false);

            textView = (TextView) messageView.findViewById(R.id.about_header);
            textView.setVisibility(TextView.GONE);
            textView = (TextView) messageView.findViewById(R.id.about_credits);
            textView.setText(R.string.no_favourites);
            textView.setHorizontallyScrolling(false); // make text wrap.

            final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
            builder.setTitle(R.string.title_favourites).setView(messageView).create().show();

        } else if (GRTApplication.mPreferences.getVersionFlag() != BuildConfig.VERSION_CODE) {
            GRTApplication.mPreferences.setVersionFlag();
            // Show alert on new version of the app
            TextView textView;
            final View messageView = mContext.getLayoutInflater().inflate(R.layout.about, null, false);

            textView = (TextView) messageView.findViewById(R.id.about_header);
            textView.setVisibility(TextView.GONE);
            textView = (TextView) messageView.findViewById(R.id.about_credits);
            textView.setText(R.string.newversion_text);
            textView.setHorizontallyScrolling(false); // make text wrap.
            final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
            builder.setTitle(R.string.newversion).setView(messageView).create().show();
        }
    }

    // If we're popping back down the stack, the favourites list could have been added to
    // since we were last here, so make sure it is reloaded before display.
    @Override
    protected void onResume() {
        super.onResume();

		/* Give up if there's no database */
        if (DB == null) {
            return;
        }

        ProcessStops();
    }

    // Called from the listener above for a long click
    public void onListItemLongClick(View view) {
        LinearLayout v = (LinearLayout) view;

        TextView tv = (TextView) v.getChildAt(0);
        final String stopid = String.valueOf(tv.getText());
        tv = (TextView) v.getChildAt(1);
        final String stopname = String.valueOf(tv.getText());

        final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int id) {
                switch (id) {
                    case DialogInterface.BUTTON_POSITIVE:
                        GRTApplication.mPreferences.RemoveBusstopFavourite(stopid);

                        for (final String[] strs : mDetails) {
                            if (strs[0].equals(stopid)) {
                                mDetails.remove(strs);
                                break;
                            }
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
        builder.setTitle("Stop " + stopid + ", " + stopname);
        builder.setMessage(R.string.favs_remove_from_list).setPositiveButton(R.string.yes, listener)
                .setNegativeButton(R.string.no, listener);
        builder.create();
        builder.show();
    }

    //@Override
    public void onListItemClick(View view) {
        LinearLayout v = (LinearLayout) view;
        TextView tv;
        String stopname, stopid;

        final Intent newintent = new Intent(mContext, TimesActivity.class);
        final String pkgstr = mContext.getApplicationContext().getPackageName();

        if (v.getChildCount() == 2) {
            // We're dealing with a stopid and description
            tv = (TextView) v.getChildAt(0);
            stopid = String.valueOf(tv.getText());
            tv = (TextView) v.getChildAt(1);
            stopname = String.valueOf(tv.getText());

            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory(mContext.getLocalClassName())
                    .setAction("Select stop")
                    .setLabel(stopid)
                    .build());

        } else {
            // it's a route: relativelayout, routeid, description.
            // We're dealing with a stopid and description
            tv = (TextView) v.getChildAt(1);
            final String routeid = String.valueOf(tv.getText());
            tv = (TextView) v.getChildAt(2);
            final String routename = String.valueOf(tv.getText());

            // Need to fish out the parent stop row to get the details
            LinearLayout parent = (LinearLayout) v.getParent();      // favstop
            // childAt(0) is the divider
            LinearLayout stop = (LinearLayout) parent.getChildAt(1); // stoplabelrow

            tv = (TextView) stop.getChildAt(0);
            stopid = String.valueOf(tv.getText());
            tv = (TextView) stop.getChildAt(1);
            stopname = String.valueOf(tv.getText());

            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory(mContext.getLocalClassName())
                    .setAction("Select route")
                    .setLabel(routeid)
                    .build());

            newintent.putExtra(pkgstr + ".route_id", routeid);
            newintent.putExtra(pkgstr + ".headsign", routename);
        }

        newintent.putExtra(pkgstr + ".stop_id", stopid);
        newintent.putExtra(pkgstr + ".stop_name", stopname);
        mContext.startActivity(newintent);
    }

    /* Do the processing to load the ArrayAdapter for display. */
    private class LoadTimes extends AsyncTask<Void, Integer, Void> {

        final LayoutInflater inflater = LayoutInflater.from(mContext);
        // Need to protect access to the stopdata and rowdata that are shared
        // between threads, so make sure the foreground thread has consumed the
        // data before the foreground thread clobbers everything for the next loop.
        private final Semaphore lockbg = new Semaphore(1, true);
        private final Semaphore lockfg = new Semaphore(0, true);
        String[] stopdata;
        ArrayList<String[]> routedata = new ArrayList<>();

        @Override
        protected void onPreExecute() {
            layout.removeAllViews();
            setProgressBarVisibility(true);
        }

        @Override
        protected void onProgressUpdate(Integer... parms) {
            LinearLayout stoprow;
            stoprow = (LinearLayout) inflater.inflate(R.layout.stoplabelrow, null);
            stoprow.findViewById(R.id.stoplabelrow).setOnLongClickListener(mLongClickListener);

            try {
                lockfg.acquire();
                TextView tv = (TextView) stoprow.findViewById(R.id.label);
                tv.setText(stopdata[0]);
                tv = (TextView) stoprow.findViewById(R.id.desc);
                tv.setText(stopdata[1]);
                layout.addView(stoprow);

                for (final String[] routerowdata : routedata) {
                    final LinearLayout routerow = (LinearLayout) inflater.inflate(R.layout.routetimerow, layout, false);
                    tv = (TextView) routerow.findViewById(R.id.stoptime);
                    tv.setText(ServiceCalendar.formattedTime(routerowdata[0]));
                    tv = (TextView) routerow.findViewById(R.id.stopminutes);
                    tv.setText(routerowdata[1]);

                    if (!routerowdata[2].equals("")) {
                        tv = (TextView) routerow.findViewById(R.id.stoprealtime);
                        Integer timediff = Integer.parseInt(routerowdata[2]);
                        if (timediff >= 0)
                            tv.setText("+" + routerowdata[2]);
                        else
                            tv.setText(routerowdata[2]);
                        if (timediff < 0 || timediff > 3)
                            tv.setTextColor(getResources().getColor(android.R.color.holo_red_light));  // @android:color/holo_green_light
                    }

                    tv = (TextView) routerow.findViewById(R.id.label);
                    tv.setText(routerowdata[3]);
                    tv = (TextView) routerow.findViewById(R.id.desc);
                    tv.setText(routerowdata[4]);

                    LinearLayout favstop = (LinearLayout) stoprow.findViewById(R.id.favstop);
                    favstop.addView(routerow);
                }
                lockbg.release();
            } catch (InterruptedException ie) {
                // so the screen might be slightly wrong; oh well.
            }

            setProgress(parms[0]);
        }

        @Override
        protected Void doInBackground(Void... foo) {

            // Find time of next bus for each stop.
            final Time t = new Time(); // TODO - this duplicates BusTimes?
            t.setToNow();
            final String datenow = String.format("%04d%02d%02d", t.year, t.month + 1, t.monthDay);

            Integer progresscount = 0;
            for (final String[] pref : mDetails) {

                final String[] nextbus = ServiceCalendar.getNextDepartureTime(pref[0], datenow);
                try {
                    lockbg.acquire();

                    stopdata = new String[]{pref[0], pref[1]}; // stop number & description
                    routedata.clear();

                    if (nextbus != null) {
                        pref[2] = nextbus[0]; // time
                        pref[3] = nextbus[2]; // route headsign
                        pref[4] = nextbus[1]; // route number

                        Integer timediff = ServiceCalendar.TimediffNow(nextbus[0]);
                        pref[5] = timediff.toString() + "m";

                        // do nothing if it's too far away
                        if (timediff.intValue() < 60 && GRTApplication.mPreferences.fetchRealtime()) {
                            pref[6] = "";
                            Realtime rt = new Realtime(pref[0], nextbus[1]);
                            if (rt.getMap() != null) {
                                String minutes = rt.getTripDetail(nextbus[3], "Minutes");
                                if (minutes != null) {
                                    timediff -= Integer.parseInt(minutes);
                                    pref[6] = timediff.toString();
                                }
                            }
                        }

                    } else {
                        // TODO check this
                        pref[2] = " -- -- --"; // time
                        pref[3] = getString(R.string.no_more_busses); // route details
                        pref[4] = "-";
                    }
                    String[] details = new String[]{pref[2], pref[5], pref[6], pref[4], pref[3]};
                    routedata.add(details);
                    lockfg.release();
                } catch (InterruptedException ie) {
                    // so the screen might be slightly wrong; oh well.
                }
                // must release lock before doing this
                publishProgress(++progresscount * 10000 / mDetails.size());
            }
            return null;
        }

        @Override
        protected void onPostExecute(Void foo) {
            getActionBar().setTitle(R.string.title_favourites);
            getActionBar().setSubtitle(null);
            setProgress(10000); // max -- makes it slide away

            final TextView tv = new TextView(mContext);
            tv.setText(R.string.longpress_removes_stop);
            layout.addView(tv);
        }
    }
}
