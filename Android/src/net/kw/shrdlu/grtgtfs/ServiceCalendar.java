package net.kw.shrdlu.grtgtfs;

import java.util.HashMap;

import android.database.Cursor;
import android.text.format.Time;
import android.util.Log;
import android.util.TimeFormatException;

public class ServiceCalendar {
	private static final String TAG = "ServiceCalendar";
	private static final String mDBQuery = "select * from calendar where service_id = ?";
	private static final String mDBQueryDate  = "select * from calendar_dates where service_id = ? and date = ?";

	// Cache some results, to save db lookups
	private static final HashMap<String,String> truemap = new HashMap<String, String>(32);
	private static final HashMap<String,String> falsemap = new HashMap<String, String>(32);
	
	// Match day number to a string and an abbreviation
	private static final String[] mWeekDays = {
		"sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"
	};
	private static final String[] mWeekDaysAbbrev = {
		"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
	};
	
	public ServiceCalendar() {
//		Log.v(TAG, "ServiceCalendar()");
	}
	
	// Return string showing days this bus runs
	private String getDays(Cursor csr) {
		String days = new String();
		
		for (int i=0; i<7; i++)
			if (csr.getInt(csr.getColumnIndex(mWeekDays[i])) == 1)
				days += mWeekDaysAbbrev[i] + " ";
		
		return days;
	}

	// Do the actual work of the getDays() call, but it makes
	// sure we close the cursor on exit.
	private String process_db(String service_id, String date, boolean limit, Cursor csr) {

		if (!csr.moveToFirst())
			return null;
		
		// Make sure it's in a current schedule period
		String start = csr.getString(csr.getColumnIndex("start_date"));
		String end = csr.getString(csr.getColumnIndex("end_date"));
		if (date.compareTo(start) < 0 || date.compareTo(end) > 0)
			// return "Schedule data has expired!";
			return null;

		// If we're not limiting the display, return what we have
		if (!limit)
			return getDays(csr);
		
		// Check for exceptions
		String [] selectargs = {service_id, date};
		Cursor exp = BusstopsOverlay.DB.rawQuery(mDBQueryDate, selectargs);
		if (exp.moveToFirst()) {
			int exception = exp.getInt(exp.getColumnIndex("exception_type"));
			if (exception == 2)			// service removed for this day
				return null;
			if (exception == 1)
				return getDays(csr);	// service added for this day
			Log.e(TAG, "bogus exception type " + exception + " for service " + service_id + "!");
			return null;
		}
		exp.close();
		
		// Check if the bus runs on the given day of the week.
		Time t = new Time();
		try {
			t.parse(date);
			t.normalize(false);
		} catch (TimeFormatException e) {
			Log.e(TAG, "got bogus date \"" + date + "\"");
			return null;
		}
		int weekday = t.weekDay;	// 0--6
		if (csr.getInt(csr.getColumnIndex(mWeekDays[weekday])) == 1)
			return getDays(csr);
		
		return null;	// doesn't run on given date.
	}
	
	// Return a string showing the days a bus runs, or null if it doesn't
	// run on the given date. Limit to correct days of week, or not.
	public String getDays(String service_id, String date, boolean limit) {
		
		String retstr;
		
		// First check the cache
		if (limit) {
			if (truemap.containsKey(service_id+date)) {
				retstr = truemap.get(service_id+date);
//				Log.v(TAG, "Retrieved " + service_id+":"+date + " -> " + retstr + " from truecache");
				return retstr;
			}
		} else {
			if (falsemap.containsKey(service_id+date)) {
				retstr = falsemap.get(service_id+date);
//				Log.v(TAG, "Retrieved " + service_id+":"+date + " -> " + retstr + " from falsecache");
				return retstr;
			}
		}
		
		String [] selectargs = {service_id};
		Cursor csr = BusstopsOverlay.DB.rawQuery(mDBQuery, selectargs);
		retstr = process_db(service_id, date, limit, csr);
		csr.close();
		
		// Save in cache
		if (limit)
			truemap.put(service_id+date, retstr);
		else
			falsemap.put(service_id+date, retstr);
		
		return retstr;
	}
}
