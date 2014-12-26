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

import android.app.AlertDialog;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.os.Build;
import android.os.Environment;
import android.util.Log;

public class DatabaseHelper {
	private static final String TAG = "DatabaseHelper"; // getClass().getSimpleName();

	private static String DB_PATH = null;
	private static int DB_VERSION = -1;
	private static final String DB_NAME = "GRT.db";
	private static Context mContext;
	private static SQLiteDatabase DB = null;

	/**
	 * Constructor Takes and keeps a reference of the passed context in order to access the application assets and resources.
	 * 
	 * @param context
	 */
	public DatabaseHelper(Context context) {
		mContext = context;

		final String DB_OLD_PATH = context.getApplicationInfo().dataDir + "/databases/";

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.FROYO /* 8 */) {
			// Returns something like
			// /mnt/sdcard/Android/data/net.kw.shrdlu.grtgtfs/files/
			DB_PATH = APIReflectionWrapper.API8.getDBPath(mContext);
		} else { // bah, make similar path
			DB_PATH = Environment.getExternalStorageDirectory().getPath() + "/Android/data/"
					+ mContext.getApplicationContext().getPackageName();
		}
		if (DB_PATH == null) {
			DB_PATH = DB_OLD_PATH;
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

			// The database is on the sdcard.
			final String sdstate = Environment.getExternalStorageState();
			if (!sdstate.equals(Environment.MEDIA_MOUNTED)) {
				final AlertDialog.Builder builder = new AlertDialog.Builder(mContext);
				builder.setTitle("The external database is unavailable.")
				.setMessage("The sdcard is in state `" + sdstate
						+ "'.\nPlease retry when it is available.").create().show();
				return;
			}
		}

		// Do this once, as we don't need them separate anymore.
		DB_PATH += "/" + DB_NAME;
	}

	/* Return path to the database. */
	public static String GetDBPath()
	{
		return DB_PATH;
	}

	/* Force close the DB so we can recreate it */
	public static void CloseDB()
	{
		if (DB != null) {
			DB.close();
			DB = null;
		}
	}
	/* Return a handle for reading the database. */
	public static SQLiteDatabase ReadableDB() {

		if (DB == null) {
			try {
				DB = SQLiteDatabase.openDatabase(DB_PATH, null, SQLiteDatabase.OPEN_READONLY);

				// Stash the version
				final Cursor csr = DB.rawQuery("PRAGMA user_version", null);
				csr.moveToPosition(0);
				DB_VERSION = csr.getInt(0);
				csr.close();

			} catch (final SQLiteException e) {
				// bah
			}
		}

		return DB;
	}

	/* Return version of current DB */
	public static int GetDBVersion() {
		if (DB == null) {
			ReadableDB();
		}
		return DB_VERSION;
	}
}
