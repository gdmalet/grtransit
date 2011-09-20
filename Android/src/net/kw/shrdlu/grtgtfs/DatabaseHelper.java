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
import android.util.Log;
import android.widget.Toast;

public class DatabaseHelper {
	private static final String TAG = "DatabaseHelper"; // getClass().getSimpleName();

	//The Android's default system path of your application database.
	private static String DB_PATH;
	private static int DB_VERSION = 4;	/* As of ??? new version 5th September 2011 */
	private static String DB_NAME = "GRT.db";
	private final Context mContext;
	private boolean mMustCopy = false;
	private SQLiteDatabase DB = null;
	private final Semaphore mDBisOpen = new Semaphore(0);
	
	/**
	 * Constructor
	 * Takes and keeps a reference of the passed context in order to access to the application assets and resources.
	 * @param context
	 */
	public DatabaseHelper(Context context) {
		mContext = context;
		
		DB_PATH = context.getApplicationInfo().dataDir + "/databases/";

		try {
			DB = SQLiteDatabase.openDatabase(DB_PATH+DB_NAME, null, SQLiteDatabase.OPEN_READONLY);
		} catch (SQLiteException e) {
			mMustCopy = true;	// presumably no database exists
		}
		
		if (DB != null) {
			int version = 0;
			try {
				Cursor csr = DB.rawQuery("PRAGMA user_version", null);
				csr.moveToPosition(0);
				version = csr.getInt(0);
				csr.close();
				if (version != DB_VERSION)
					mMustCopy = true;
			} catch (Exception e) {
				mMustCopy = true;	// something went haywire
			}
		}
		
		if (mMustCopy) {
			if (DB != null) {
				DB.close();
				DB = null;
			}
			Toast.makeText(mContext,
					"The database will be upgraded. This may take some time.", Toast.LENGTH_LONG).show();

			// Copy the database in the background.
			mContext.startService(new Intent(mContext, DBcopier.class));
		}
	}	

	/**
	 * Copies database from local assets-folder to the
	 * system folder, from where it can be accessed and handled.
	 * This is done by transferring bytestream.
	 **/
	private class DBcopier extends IntentService {
		private static final String TAG = "DBcopier";

		// Must have a default constructor
		public DBcopier() {
			super("DBcopier");
			Log.v(TAG, "constructor");
		}

		@Override
		protected void onHandleIntent(Intent intent) {
			Log.v(TAG, "Copying new database " + DB_NAME);

			// Open the empty db as the output stream
			OutputStream myOutput;
			try {
				myOutput = new FileOutputStream(DB_PATH + DB_NAME);

				//transfer bytes from the inputfile to the outputfile
				byte[] buffer = new byte[8*1024];
				int i;
				for (i=0; ; i++) {
					// Loop and pick up all pieces of the file, an concatenate into one.
					String input = String.format("databases/" + DB_NAME + ".%02d", i);

					// Open local db piece as the input stream
					InputStream myInput;
					try {
						myInput = mContext.getAssets().open(input);
					} catch (IOException e) {
						break;	// no more files
					}

					int length;
					while ((length = myInput.read(buffer)) > 0 ){
						myOutput.write(buffer, 0, length);
					}
					
					myInput.close();
				}

				//Close the streams
				myOutput.flush();
				myOutput.close();
		
				Log.v(TAG, " ... database " + DB_NAME + " copy complete: re-opening db & releasing lock");
				DB = SQLiteDatabase.openDatabase(DB_PATH+DB_NAME, null, SQLiteDatabase.OPEN_READONLY);
				mDBisOpen.release();

			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public SQLiteDatabase ReadableDB() {
		Log.d(TAG,"in ReadableDB()");
		
		while (DB == null) {
			try {
				Log.d(TAG, "about to aquire semaphore");
				mDBisOpen.acquire();
				Log.d(TAG, " ... got semaphore");
			} catch (InterruptedException e1) {
				Log.w(TAG, "interrupted exception?"); // just loop and try again?
			}
		}
		
		return DB;
	}
}
