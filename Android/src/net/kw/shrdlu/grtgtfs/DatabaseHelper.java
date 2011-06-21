package net.kw.shrdlu.grtgtfs;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteException;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;

public class DatabaseHelper extends SQLiteOpenHelper {
	private static final String TAG = "DatabaseHelper";

	//The Android's default system path of your application database.
	private static String DB_PATH;
	private static String DB_NAME = "GRT.db";
	private final Context mContext;
	private boolean mustCopy = false;
	
	/**
	 * Constructor
	 * Takes and keeps a reference of the passed context in order to access to the application assets and resources.
	 * @param context
	 */
	public DatabaseHelper(Context context) {
		super(context, DB_NAME, null, 1);
		this.mContext = context;
		
		DB_PATH = context.getApplicationInfo().dataDir + "/databases/";
	}	

	/**
	 * Copies your database from your local assets-folder to the just created empty database in the
	 * system folder, from where it can be accessed and handled.
	 * This is done by transferring bytestream.
	 * */
	private void copyDatabase() throws IOException {
		Log.v(TAG, "Copying new database " + DB_NAME);
/*
		Toast.makeText(mContext,
				"Preparing database for first use -- please be patient",
				Toast.LENGTH_LONG)
				.show();
*/	
		// Open the empty db as the output stream
		OutputStream myOutput = new FileOutputStream(DB_PATH + DB_NAME);

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
	}


	@Override
	public void onCreate(SQLiteDatabase db) {
		Log.d(TAG,"in OnCreate()");

		// Wait for the open() to do the copy, else the db will be empty.
    	try {
    		db.rawQuery("select count(*) from stops", null);
    	} catch (SQLException e) {
    		Log.v(TAG, "Querying stops failed, will copy db: " + e.getMessage());
    		mustCopy = true;
    	}
	}

	@Override
	public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
		Log.d(TAG,"in OnUpgrade()");

	}

	// This one is optional.
	@Override
	public void onOpen(SQLiteDatabase db) throws SQLiteException {
		Log.d(TAG,"in OnOpen()");

		if (mustCopy) {
			try {
				copyDatabase();
			} catch (IOException e) {
				Log.e(TAG, "onCreate failed when creating database");
				e.printStackTrace();
				throw new SQLiteException();
			}
			mustCopy = false;
		}
	}

	// Add your public helper methods to access and get content from the database.
	// You could return cursors by doing "return myDataBase.query(....)" so it'd be easy
	// to you to create adapters for your views.

}