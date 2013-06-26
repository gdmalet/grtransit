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
import java.io.OutputStream;
import java.util.concurrent.Semaphore;

import android.app.IntentService;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.os.Build;
import android.os.Environment;
import android.util.Log;
import android.widget.Toast;

public class DatabaseHelper {
	private static final String TAG = "DatabaseHelper"; // getClass().getSimpleName();

	private static String DB_PATH = null;
	private static final int DB_VERSION = 17; /* As of 25th June 2013 */
	private static final String DB_NAME = "GRT.db";
	private static Context mContext;
	private static boolean mMustCopy = false;
	private static SQLiteDatabase DB = null;
	private static final Semaphore mDBisOpen = new Semaphore(1);

	/**
	 * Constructor Takes and keeps a reference of the passed context in order to access to the application assets and resources.
	 * 
	 * @param context
	 */
	public DatabaseHelper(Context context) {
		mContext = context;
		final String DB_OLD_PATH = context.getApplicationInfo().dataDir + "/databases/";
		final String BAD_DB_PATH = "/sdcard/data/Android/" + mContext.getApplicationContext().getPackageName();

		while (true) {
			try {
				// Log.d(TAG, "constructor about to acquire semaphore");
				mDBisOpen.acquire();
				// Log.d(TAG, " ... constructor got semaphore");
				break;
			} catch (final InterruptedException e1) {
				// Log.w(TAG, "constructor interrupted exception?"); // just
				// loop and try again?
			}
		}

		if (DB_PATH != null) {
			Log.e(TAG, "constructor has already been called!?");
		}

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.FROYO /* 8 */) {
			// Returns something like
			// /mnt/sdcard/Android/data/net.kw.shrdlu.grtgtfs/files/
			DB_PATH = APIReflectionWrapper.API8.getDBPath(mContext);
		} else { // bah, make similar path
			DB_PATH = Environment.getExternalStorageDirectory().getPath() + "/Android/data/"
					+ mContext.getApplicationContext().getPackageName();
		}

		final File f = new File(DB_PATH);
		if (!f.exists() && !f.mkdirs()) {
			Log.e(TAG, "can't create sdcard dirs, using phone storage :-(");
			DB_PATH = DB_OLD_PATH;
		}
		if (f.exists() && !f.canWrite()) {
			Log.e(TAG, "can't write to sdcard dirs, using phone storage :-(");
			DB_PATH = DB_OLD_PATH;
		}

		// Delete the old database if it exists, and recreate on the sdcard.
		if (!DB_PATH.equals(DB_OLD_PATH)) {
			final File olddb = new File(DB_OLD_PATH + DB_NAME);
			if (olddb.exists() && !olddb.delete()) {
				Log.e(TAG, "failed to delete old db...!?");
			}
		}
		// And another one, due to a mistake in setting DB_PATH in v0.96.
		new File(BAD_DB_PATH + "/" + DB_NAME).delete();
		new File(BAD_DB_PATH).delete();

		// Do this once, as we don't need them separate anymore.
		DB_PATH += "/" + DB_NAME;
		/* // The database is on the sdcard. // String sdstate = Environment.getExternalStorageState(); if
		 * (!sdstate.equals(Environment.MEDIA_MOUNTED)) { AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
		 * 
		 * builder.setTitle("The external database is unavailable.") .setMessage("The sdcard is in state `" + sdstate +
		 * "'.\nPlease retry when it is available.") .create() .show();
		 * 
		 * return; } */
		try {
			DB = SQLiteDatabase.openDatabase(DB_PATH, null, SQLiteDatabase.OPEN_READONLY);
		} catch (final SQLiteException e) {
			// Log.d(TAG, "open database failed - will copy");
			mMustCopy = true; // presumably no database exists
		}

		if (DB != null) {
			int version = 0;
			try {
				final Cursor csr = DB.rawQuery("PRAGMA user_version", null);
				csr.moveToPosition(0);
				version = csr.getInt(0);
				csr.close();
				if (version != DB_VERSION) {
					mMustCopy = true;
					// Log.d(TAG, "must copy db version " + version + " to new "
					// + DB_VERSION);
				}
			} catch (final Exception e) {
				mMustCopy = true; // something went haywire
			}
		}

		if (mMustCopy) {
			if (DB != null) {
				DB.close();
				DB = null;
			}
			Toast.makeText(mContext, "The database will be (re)created. This may take some time.", Toast.LENGTH_LONG).show();

			// Copy the database in the background.
			// Log.d(TAG, "starting database upgrade service");
			mContext.startService(new Intent(mContext, DBcopier.class));
		} else {
			mDBisOpen.release(); // otherwise the service does that
		}
		// Log.v(TAG, "clean exit of constructor");
	}

	/**
	 * Copies database from local assets-folder to the system folder, from where it can be accessed and handled. This is done by
	 * transferring bytestream. Note that this class must be public static, since it's embedded in the outer class. If it's not
	 * static, starting the service will fail.
	 **/
	public static class DBcopier extends IntentService {
		private static final String TAG = "DBcopier";

		// Must have a default constructor
		public DBcopier() {
			super("DBcopier");
			// Log.v(TAG, "constructor");
		}

		@Override
		protected void onHandleIntent(Intent intent) {
			// Log.v(TAG, "Copying new database " + DB_NAME);

			// Open the empty db as the output stream
			OutputStream myOutput;
			try {
				myOutput = new FileOutputStream(DB_PATH);

				// transfer bytes from the inputfile to the outputfile
				final byte[] buffer = new byte[8 * 1024];
				int i;
				for (i = 0;; i++) {
					// Loop and pick up all pieces of the file, an concatenate
					// into one.
					final String input = String.format("databases/" + DB_NAME + ".%02d", i);

					// Open local db piece as the input stream
					InputStream myInput;
					try {
						myInput = mContext.getAssets().open(input);
					} catch (final IOException e) {
						break; // no more files
					}

					int length;
					while ((length = myInput.read(buffer)) > 0) {
						myOutput.write(buffer, 0, length);
					}

					myInput.close();
				}

				// Close the streams
				myOutput.flush();
				myOutput.close();

				// Set the version to match, so we don't keep copying, even if
				// the db is the wrong version to start.
				DB = SQLiteDatabase.openDatabase(DB_PATH, null, SQLiteDatabase.OPEN_READWRITE);
				DB.execSQL("PRAGMA user_version = " + DB_VERSION);
				DB.close();

				// Log.v(TAG, " ... database " + DB_NAME +
				// " copy complete: re-opening db & releasing lock");
				DB = SQLiteDatabase.openDatabase(DB_PATH, null, SQLiteDatabase.OPEN_READONLY);
				mDBisOpen.release();

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
		}
	}

	public static SQLiteDatabase ReadableDB() {
		// Log.d(TAG,"in ReadableDB()");

		// If the DB is not open yet, we need to wait for the constructor, which
		// is perhaps
		// copying over a new database.
		while (DB == null) {
			try {
				// Log.d(TAG, "... ReadableDB about to acquire semaphore");
				mDBisOpen.acquire();
				// Log.d(TAG, " ... ReadableDB got semaphore");
			} catch (final InterruptedException e1) {
				// Log.w(TAG, "interrupted exception?"); // just loop and try
				// again?
			}
		}

		return DB;
	}
}
