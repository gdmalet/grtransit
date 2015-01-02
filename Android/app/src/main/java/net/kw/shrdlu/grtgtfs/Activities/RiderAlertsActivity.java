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

import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Base64;
import android.util.Log;
import android.view.Window;
import android.widget.Toast;

import net.kw.shrdlu.grtgtfs.Activities.MenuListActivity;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.ListArrayAdapter;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.TwitterCredentials;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Locale;

public class RiderAlertsActivity extends MenuListActivity {
	private static final String TAG = "RiderAlertsActivity";

	// The search string to get recent tweets from GRT Rider Alerts
	private final String TwitterURL = "https://api.twitter.com/1.1/statuses/user_timeline.json";
	private final String TwitterQry = "?screen_name=GRT_ROW&count=20&trim_user=true";

	private final String TwitterOauth = "https://api.twitter.com/oauth2/token";
	private static String AccessToken = null;

	private ListArrayAdapter mAdapter;
	private ArrayList<String[]> mListDetails;


	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;

        // Will use the action bar progress bar
        requestWindowFeature(Window.FEATURE_PROGRESS);

        setContentView(R.layout.timeslayout);
		super.onCreate(savedInstanceState);

		mListDetails = new ArrayList<>();

        getActionBar().setTitle(R.string.twitter_querying_feed);
        getActionBar().setSubtitle(null);

		new ProcessTweets().execute();
	}

	/* Do the processing to load the ArrayAdapter for display. */
	private class ProcessTweets extends AsyncTask<Void, Integer, Void> {
		// static final String TAG = "ProcessBusStops";

		@Override
		protected void onPreExecute() {
            setProgressBarVisibility(true);
		}

		// Update the progress bar.
		@Override
		protected void onProgressUpdate(Integer... parms) {
			setProgress(parms[0]);
		}

		@Override
		protected Void doInBackground(Void... foo) {

			if (AccessToken == null) {
				String ConsumerKeyEnc, ConsumerSecretEnc, BearerToken, BearerToken64;
				ConsumerKeyEnc = Uri.encode(TwitterCredentials.ConsumerKey);
				ConsumerSecretEnc = Uri.encode(TwitterCredentials.ConsumerSecret);
				BearerToken = ConsumerKeyEnc + ":" + ConsumerSecretEnc;
				BearerToken64 = Base64.encodeToString(BearerToken.getBytes(), Base64.URL_SAFE | Base64.NO_WRAP);

				// Get the Twitter OAuth token
				final HttpClient httpclient = new DefaultHttpClient();
				final HttpPost httppost = new HttpPost(TwitterOauth);
				httppost.setHeader("Authorization", "Basic " + BearerToken64);

				publishProgress(15); // fake it

				try {
					final List<NameValuePair> nameValuePairs = new ArrayList<>(1);
					nameValuePairs.add(new BasicNameValuePair("grant_type", "client_credentials"));
					final UrlEncodedFormEntity entity = new UrlEncodedFormEntity(nameValuePairs);
					entity.setContentType("application/x-www-form-urlencoded;charset=UTF-8");
					httppost.setEntity(entity);

					// Execute HTTP Post Request
					final HttpResponse response = httpclient.execute(httppost);

					final StatusLine statusLine = response.getStatusLine();
					final int statusCode = statusLine.getStatusCode();
					if (statusCode == 200) {
						final HttpEntity responseEntity = response.getEntity();
						final String s = EntityUtils.toString(responseEntity);
						final JSONObject object = (JSONObject) new JSONTokener(s).nextValue();
						final String type = object.getString("token_type");
						final String token = object.getString("access_token");

						if (type.contentEquals("bearer") && !token.isEmpty()) {
							AccessToken = token;	// stash it
						}
					}
				} catch (final IOException e) {
				} catch (final JSONException e) {
					e.printStackTrace();
				}
			}

			publishProgress(2500); // fake it
			if (AccessToken != null) {
				mListDetails = readTwitterFeed();
			}
			publishProgress(9000); // fake it

			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {
            getActionBar().setTitle(R.string.title_rider_alerts);
            getActionBar().setSubtitle(null);
            setProgress(10000); // max -- makes it slide away

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

	/**
	 * Get the latest twitter info. Some of this copied from http://www.vogella.com/articles/AndroidJSON/article.html
	 */
	public ArrayList<String[]> readTwitterFeed() {
		final StringBuilder builder = new StringBuilder();
		final HttpClient client = new DefaultHttpClient();
		final HttpGet httpGet = new HttpGet(TwitterURL + TwitterQry);
		httpGet.setHeader("Authorization", "Bearer " + AccessToken);

		try {
			final HttpResponse response = client.execute(httpGet);

			final StatusLine statusLine = response.getStatusLine();
			final int statusCode = statusLine.getStatusCode();
			if (statusCode == 200) {
				final HttpEntity entity = response.getEntity();
				final InputStream content = entity.getContent();
				final BufferedReader reader = new BufferedReader(new InputStreamReader(content));
				String line;
				while ((line = reader.readLine()) != null) {
					builder.append(line);
				}
                reader.close();
                content.close();

				// Result is an array of tweets
				final JSONArray arr = (JSONArray) new JSONTokener(builder.toString()).nextValue();

				final ArrayList<String[]> tweets = new ArrayList<>();

				// Need to grok dates of form "created_at": "Thu, 15 Nov 2012 18:27:17 +0000"
				final SimpleDateFormat dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss ZZZZZ yyyy");
				dateFormat.setLenient(true);

				for (int i = 0; i < arr.length(); i++) {
					final String text = arr.getJSONObject(i).get("text").toString();
					String tweettime = arr.getJSONObject(i).get("created_at").toString();

					// Extract & reformat the date
					Date created;
					final GregorianCalendar cal = new GregorianCalendar();
					try {
						created = dateFormat.parse(tweettime);
						cal.setTime(created);
						String day, mon;

                        day = cal.getDisplayName(Calendar.DAY_OF_WEEK, Calendar.LONG, Locale.getDefault());
                        mon = cal.getDisplayName(Calendar.MONTH, Calendar.SHORT, Locale.getDefault());

						tweettime = String.format("%s %02d:%02d - %s %d", day, cal.get(Calendar.HOUR_OF_DAY),
								cal.get(Calendar.MINUTE), mon, cal.get(Calendar.DAY_OF_MONTH));

					} catch (final Exception e) {
						Log.d(TAG, "Exception: " + e.getMessage() + ", parsing tweet date `" + tweettime + "'");
						tweettime = "--:--";
					}

					tweets.add(new String[] { tweettime, text });
				}

				return tweets;

			} else {
				Log.d(TAG, "Failed to download twitter info");
			}
		} catch (final ClientProtocolException e) {
			Log.d(TAG, "ClientProtocolException: " + e.getMessage() + ", Failed to download twitter info");
		} catch (final IOException e) {
			Log.d(TAG, "IOException: " + e.getMessage() + ", Failed to download twitter info");
		} catch (final Exception e) {
			Log.d(TAG, "Exception: " + e.getMessage() + ", Failed to download twitter info");
		}

		return null;
	}
}
