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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Locale;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.json.JSONArray;
import org.json.JSONTokener;

import android.app.ListActivity;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

public class RiderAlertsActivity extends ListActivity implements AnimationListener {
	private static final String TAG = "RiderAlerts";

	// The search string to get recent tweets from GRT Rider Alerts
	private final String TwitterURL = "http://api.twitter.com/1/statuses/user_timeline.json";
	private final String TwitterQry = "?screen_name=GRTRiderAlerts&count=20&include_entities=false&trim_user=true";

	private ListActivity mContext;
	private View mListDetail;
	private Animation mSlideIn, mSlideOut;
	private ProgressBar mProgress;
	private TextView mTitle;
	private ListArrayAdapter mAdapter;
	private ArrayList<String[]> mListDetails;;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		mContext = this;

		setContentView(R.layout.timeslayout);

		// Load animations used to show/hide progress bar
		mProgress = (ProgressBar) findViewById(R.id.progress);
		mListDetail = findViewById(R.id.detail_area);
		mSlideIn = AnimationUtils.loadAnimation(this, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(this, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);
		mTitle = (TextView) findViewById(R.id.listtitle);
		mListDetails = new ArrayList<String[]>();

		mTitle.setText(R.string.twitter_querying_feed);

		new ProcessTweets().execute();
	}

	@Override
	protected void onResume() {
		super.onResume();
		// We want to track a pageView every time this Activity gets the focus.
		Globals.tracker.trackPageView("/" + this.getLocalClassName());
	}

	/* Do the processing to load the ArrayAdapter for display. */
	private class ProcessTweets extends AsyncTask<Void, Integer, Void> {
		// static final String TAG = "ProcessBusStops";

		@Override
		protected void onPreExecute() {
			mListDetail.startAnimation(mSlideIn);
			mProgress.setVisibility(View.VISIBLE);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
		}

		@Override
		protected Void doInBackground(Void... foo) {

			mListDetails = readTwitterFeed();

			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {

			mProgress.setVisibility(View.INVISIBLE);
			mListDetail.startAnimation(mSlideOut);

			mTitle.setText(R.string.title_rider_alerts);

			if (mListDetails == null) {
				Toast.makeText(mContext, R.string.twitter_fetch_failed, Toast.LENGTH_LONG).show();
			} else if (mListDetails.isEmpty()) {
				Toast.makeText(mContext, R.string.twitter_fetch_nothing, Toast.LENGTH_LONG).show();
			} else {
				mAdapter = new ListArrayAdapter(mContext, R.layout.tweetlayout, mListDetails);
				mContext.setListAdapter(mAdapter);
			}
		}
	}

	/* Wrap calls to functions that may not be in the version of the OS that we're running. This class is only instantiated if
	 * we refer to it, at which point Dalvik would discover the error. So don't refer to it if we know it will fail.... */
	private static class API9ReflectionWrapper {
		public static String getDisplayName(Calendar cal, int field, int style, Locale locale) {
			return cal.getDisplayName(field, style, locale);
		}
	}

	/**
	 * Get the latest twitter info. Some of this copied from http://www.vogella.com/articles/AndroidJSON/article.html
	 */
	public ArrayList<String[]> readTwitterFeed() {
		StringBuilder builder = new StringBuilder();
		HttpClient client = new DefaultHttpClient();
		HttpGet httpGet = new HttpGet(TwitterURL + TwitterQry);
		try {
			HttpResponse response = client.execute(httpGet);
			StatusLine statusLine = response.getStatusLine();
			int statusCode = statusLine.getStatusCode();
			if (statusCode == 200) {
				HttpEntity entity = response.getEntity();
				InputStream content = entity.getContent();
				BufferedReader reader = new BufferedReader(new InputStreamReader(content));
				String line;
				while ((line = reader.readLine()) != null) {
					builder.append(line);
				}

				// Result is an array of tweets
				JSONArray arr = (JSONArray) new JSONTokener(builder.toString()).nextValue();

				ArrayList<String[]> tweets = new ArrayList<String[]>();

				// Need to grok dates of form "created_at": "Thu, 15 Nov 2012 18:27:17 +0000"
				SimpleDateFormat dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss ZZZZZ yyyy");
				dateFormat.setLenient(true);

				for (int i = 0; i < arr.length(); i++) {
					String text = new String(arr.getJSONObject(i).get("text").toString());
					String tweettime = new String(arr.getJSONObject(i).get("created_at").toString());

					// Extract & reformat the date
					Date created = null;
					GregorianCalendar cal = new GregorianCalendar();
					try {
						created = dateFormat.parse(tweettime);
						cal.setTime(created);
						String day, mon;

						if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD /* 9 */) {
							day = API9ReflectionWrapper.getDisplayName(cal, Calendar.DAY_OF_WEEK, Calendar.LONG,
									Locale.getDefault());
							mon = API9ReflectionWrapper
									.getDisplayName(cal, Calendar.MONTH, Calendar.SHORT, Locale.getDefault());
						} else { // bah
							SimpleDateFormat sdf = new SimpleDateFormat("EEEEEEEE", Locale.getDefault());
							day = sdf.format(new Date());
							sdf = new SimpleDateFormat("MMM", Locale.getDefault());
							mon = sdf.format(new Date());

							// final String[] Days = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
							// "Saturday" };
							// final String[] Months = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
							// "Nov", "Dec" };
							// day = Days[Calendar.DAY_OF_WEEK];
							// mon = Months[Calendar.MONTH];
						}
						tweettime = String.format("%s %02d:%02d - %s %d", day, cal.get(Calendar.HOUR_OF_DAY),
								cal.get(Calendar.MINUTE), mon, cal.get(Calendar.DAY_OF_MONTH));

					} catch (Exception e) {
						Log.d(TAG, "Exception: " + e.getMessage() + ", parsing tweet date `" + tweettime + "'");
						tweettime = "--:--";
					}

					tweets.add(new String[] { tweettime, text });
				}

				return tweets;

			} else {
				Log.d(TAG, "Failed to download twitter info");
			}
		} catch (ClientProtocolException e) {
			Log.d(TAG, "ClientProtocolException: " + e.getMessage() + ", Failed to download twitter info");
		} catch (IOException e) {
			Log.d(TAG, "IOException: " + e.getMessage() + ", Failed to download twitter info");
		} catch (Exception e) {
			Log.d(TAG, "Exception: " + e.getMessage() + ", Failed to download twitter info");
		}

		return null;
	}

	/**
	 * Make the {@link ProgressBar} visible when our in-animation finishes.
	 */
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
