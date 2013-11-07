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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.util.zip.GZIPInputStream;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.util.EntityUtils;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.TextView;

public class StartupActivity extends Activity {
	private static final String TAG = "StartupActivity";

	protected Activity mContext;
	protected TextView mTitle;
	protected ProgressBar mProgress;

	private static String DBVersionURL = "http://baleka.org/gdmalet/android/grtransit/GRT.db.version";
	private static String DBDatabaseURL = "http://baleka.org/gdmalet/android/grtransit/GRT.db.gz";

	private static String DB_PATH;
	private static NewDBVersion DBV = null;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		mContext = this;
		setContentView(R.layout.timeslayout);
		super.onCreate(savedInstanceState);

		/* Keep test files clear of production files. */
		if (GRTApplication.isDebugBuild) {
			DBVersionURL += ".dbg";
			DBDatabaseURL += ".dbg";
		}

		mTitle = (TextView) findViewById(R.id.listtitle);
		mProgress = (ProgressBar) findViewById(R.id.progress);
		mProgress.setVisibility(View.VISIBLE);
		mProgress.setProgress(5); // make partially visible

		mTitle.setText(R.string.db_opening);

		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB /* 11 */) {
			// Switch title bar icon to the one without the < hint.
			final Button logo = (Button) findViewById(R.id.titlelogo);
			final Drawable d = getResources().getDrawable(R.drawable.grticon_nohome);
			logo.setClickable(false);
			logo.setBackgroundDrawable(d);
		} else {
			APIReflectionWrapper.API11.prepActionBar(mContext);
		}

		DB_PATH = DatabaseHelper.GetDBPath();

		if (GRTApplication.mPreferences.autoDbUpdate() == true) {
			new LatestDB().execute();
		} else {
			startFavstops();
		}
	}

	/*
	 * Start FavstopsActivity ... this can be called from multiple code paths, so put it in one place. */
	public void startFavstops()
	{
		mContext.startActivity(new Intent(mContext, FavstopsActivity.class));
		finish(); // take this activity off the stack.
	}

	/* Do networking stuff off the main thread. */
	private class LatestDB extends AsyncTask<Void, Void, Void> {

		private int newdbv = -1, olddbv = -1;

		@Override
		protected Void doInBackground(Void... foo) {
			DBV = new NewDBVersion();
			newdbv = DBV.getDBVersion();
			olddbv = DatabaseHelper.GetDBVersion();

			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {

			/* If versions match, or we can't check the new version, just continue. */
			if ((newdbv > 0 && olddbv > 0 && newdbv == olddbv) || (newdbv < 0)) {
				startFavstops();
				return;
			}

			final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int id) {
					switch (id) {
					case DialogInterface.BUTTON_POSITIVE:
						new DBCopier().execute();
						break;
					case DialogInterface.BUTTON_NEGATIVE:
						if (olddbv < 0) {
							mContext.finish();
							return;
						}
						startFavstops();
						return;
					}
					dialog.cancel();
				}
			};

			final String sizestr = mContext.getString(R.string.db_download_now, DBV.getSize());
			final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
			builder.setTitle(R.string.db_new_avail);
			builder.setMessage(sizestr).setPositiveButton(R.string.yes, listener)
			.setNegativeButton(R.string.no, listener);
			builder.create();
			builder.show();
		}
	}

	/**
	 * Check what the latest version of the database on the website is. Reads a file that contains 3 fields: integer version,
	 * string (of a float) approximate size (in megs), and the md5sum of the uncompressed db.
	 */
	private class NewDBVersion {

		protected int DBVersion = -1;
		protected float DBsize = 0.0f;
		protected String DBmd5 = null;

		public NewDBVersion()
		{
			final HttpClient client = new DefaultHttpClient();
			final HttpGet httpGet = new HttpGet(DBVersionURL);

			try {
				final HttpResponse response = client.execute(httpGet);
				final StatusLine statusLine = response.getStatusLine();
				final int statusCode = statusLine.getStatusCode();

				if (statusCode == 200) {
					final HttpEntity responseEntity = response.getEntity();
					final String s = EntityUtils.toString(responseEntity);

					DBVersion = Integer.parseInt(s.substring(0, s.indexOf(' ')));

					DBsize = Float.parseFloat(s.substring(s.indexOf(' ') + 1, s.lastIndexOf(' ')));

					if (s.endsWith("\n")) {
						DBmd5 = s.substring(s.lastIndexOf(' ') + 1, s.length() - 1);
					} else {
						DBmd5 = s.substring(s.lastIndexOf(' ') + 1);
					}
				}
			} catch (final ClientProtocolException e) {
				// TODO Auto-generated catch block
			} catch (final IOException e) {
				// TODO Auto-generated catch block
			} catch (final NumberFormatException e) {
				// TODO Auto-generated catch block
			} catch (final Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		public int getDBVersion()
		{
			return DBVersion;
		}

		public float getSize()
		{
			return DBsize;
		}

		public String getDBmd5()
		{
			return DBmd5;
		}
	}

	/**
	 * Copies database from local assets-folder to the system folder, from where it can be accessed and handled. This is done by
	 * transferring bytestream. Note that this class must be public static, since it's embedded in the outer class. If it's not
	 * static, starting the service will fail.
	 **/
	private class DBCopier extends AsyncTask<Void, Integer, Void> {

		private boolean alliswell = false;

		@Override
		protected void onPreExecute() {
			mTitle.setText(R.string.db_downloading);
		}

		@Override
		protected void onProgressUpdate(Integer... parms) {
			mProgress.setProgress(parms[0]);
		}

		@Override
		protected Void doInBackground(Void... foo) {

			DatabaseHelper.CloseDB();

			byte[] digest = null;

			try {
				final HttpClient client = new DefaultHttpClient();
				final HttpGet httpGet = new HttpGet(DBDatabaseURL);

				final HttpResponse response = client.execute(httpGet);

				final StatusLine statusLine = response.getStatusLine();
				final int statusCode = statusLine.getStatusCode();
				if (statusCode == 200) {
					final FileOutputStream myOutput = new FileOutputStream(DB_PATH + ".new");
					final byte[] buffer = new byte[8 * 1024];

					final HttpEntity entity = response.getEntity();
					final InputStream content = entity.getContent();

					// Remote file is zipped, but md5sum is of the uncompressed file.
					final GZIPInputStream zis = new GZIPInputStream(content);
					final MessageDigest md = MessageDigest.getInstance("MD5");
					final DigestInputStream dis = new DigestInputStream(zis, md);

					int count, DBtotal = 0;
					final float tot = DBV.getSize() * 1024 * 1024 * 5.0f; // assume roughly 5 to 1 compression
					while ((count = dis.read(buffer, 0, buffer.length)) > 0) {
						DBtotal += count;
						myOutput.write(buffer, 0, count);
						publishProgress((int) ((DBtotal / tot) * 100.0f));
					}

					myOutput.flush();
					myOutput.close();

					digest = md.digest();
				}

				// Did it get munged on the way?
				final StringBuffer sb = new StringBuffer();
				for (final byte element : digest) {
					// Force in a leading zero if required, and watch out for sign extensions....
					sb.append(Integer.toHexString((element & 0xFF) | 0x100).substring(1, 3));
				}
				if (!sb.toString().equals(DBV.getDBmd5())) {
					throw new IOException();
				}

				final File o = new File(DB_PATH);
				final File n = new File(DB_PATH + ".new");
				o.delete();
				n.renameTo(o);

				alliswell = true;

			} catch (final FileNotFoundException e) {
				Log.e(TAG, "FileNotFoundException exception");
				e.printStackTrace();
			} catch (final IOException e) {
				Log.e(TAG, "IOException exception");
				e.printStackTrace();
			} catch (final Exception e) {
				Log.e(TAG, "unknown exception exception");
				e.printStackTrace();
			}

			return null;
		}

		@Override
		protected void onPostExecute(Void foo) {

			if (alliswell) {
				startFavstops();
				return;
			}

			final DialogInterface.OnClickListener listener = new DialogInterface.OnClickListener() {
				@Override
				public void onClick(DialogInterface dialog, int id) {
					switch (id) {
					case DialogInterface.BUTTON_NEGATIVE:
						mContext.finish();
						return;
					}
					dialog.cancel();
					startFavstops();
					return;
				}
			};

			final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
			builder.setTitle(R.string.db_is_corrupt)
			.setMessage(R.string.corrupt_exit)
			.setPositiveButton(R.string.cntinue, listener)
			.setNegativeButton(R.string.exit, listener)
			.create()
			.show();
		}
	}
}
