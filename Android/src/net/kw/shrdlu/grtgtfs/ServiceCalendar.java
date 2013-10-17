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
import java.util.HashMap;

import android.database.Cursor;
import android.text.format.Time;
import android.util.Log;
import android.util.TimeFormatException;

public class ServiceCalendar {
	private static final String TAG = "ServiceCalendar";
	private static final String mDBQuery = "select * from calendar where service_id = ?";
	private static final String mDBQueryDate = "select * from calendar_dates where service_id = ? and date = ?";

	// Cache some results, to save db lookups
	private static final HashMap<String, String> truemap = new HashMap<String, String>(32);
	private static final HashMap<String, String> falsemap = new HashMap<String, String>(32);
	private static final HashMap<String, String> trip2servicemap = new HashMap<String, String>(64);

	// Match day number to a string and an abbreviation
	private static final String[] mWeekDays = { "sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday" };
	private static final String[] mWeekDaysAbbrev = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

	public ServiceCalendar() {
		// Log.v(TAG, "ServiceCalendar()");
	}

	// Return string showing days this bus runs.
	// Cursor points to a row in the calendar table for this service_id.
	private static String getDays(Cursor csr) {
		String days = new String();

		for (int i = 0; i < 7; i++) {
			if (csr.getInt(csr.getColumnIndex(mWeekDays[i])) == 1) {
				days += mWeekDaysAbbrev[i] + " ";
			}
		}

		return days;
	}

	// Do the actual work of the getDays() call, but it makes
	// sure we close the cursor on exit.
	private static String process_db(String service_id, String date, boolean limit, Cursor csr) {

		if (!csr.moveToFirst()) {
			return null;
		}

		// Make sure it's in a current schedule period
		final String start = csr.getString(csr.getColumnIndex("start_date"));
		final String end = csr.getString(csr.getColumnIndex("end_date"));
		if (date.compareTo(start) < 0 || date.compareTo(end) > 0) {
			// return "Schedule data has expired!";
			return null;
		}

		// If we're not limiting the display, return what we have
		if (!limit) {
			return getDays(csr);
		}

		// Check for exceptions
		final String[] selectargs = { service_id, date };
		final Cursor exp = DatabaseHelper.ReadableDB().rawQuery(mDBQueryDate, selectargs);
		if (exp.moveToFirst()) {
			final int exception = exp.getInt(exp.getColumnIndex("exception_type"));
			exp.close();
			if (exception == 2) {
				return null;
			}
			if (exception == 1) {
				return getDays(csr); // service added for this day
			}
			Log.e(TAG, "bogus exception type " + exception + " for service " + service_id + "!");
			return null;
		} else {
			exp.close();
		}

		// Check if the bus runs on the given day of the week.
		final Time t = new Time();
		try {
			t.parse(date);
			t.normalize(false);
		} catch (final TimeFormatException e) {
			Log.e(TAG, "got bogus date \"" + date + "\"");
			return null;
		}
		final int weekday = t.weekDay; // 0--6
		if (csr.getInt(csr.getColumnIndex(mWeekDays[weekday])) == 1) {
			return getDays(csr);
		}

		return null; // doesn't run on given date.
	}

	// Return a string showing the days a bus runs, or null if it doesn't
	// run on the given date. Limit to correct days of week, or not.
	public static String getTripDaysofWeek(String trip_id, String date, boolean limit) {

		String retstr = null;

		// Get and translate the service id
		String service_id = null;
		if (trip2servicemap.containsKey(trip_id)) {
			service_id = trip2servicemap.get(trip_id);
		} else {
			final String svsq = "select service_id from trips where trip_id = ?";
			final String[] svsargs = { trip_id };
			final Cursor svs = DatabaseHelper.ReadableDB().rawQuery(svsq, svsargs);
			svs.moveToFirst();
			service_id = svs.getString(0);
			svs.close();
			if (service_id != null && !service_id.equals("")) {
				trip2servicemap.put(trip_id, service_id);
			}
		}
		if (service_id == null) {
			return null;
		}

		// First check the cache
		if (limit) {
			if (truemap.containsKey(service_id + date)) {
				retstr = truemap.get(service_id + date);
				// Log.v(TAG, "Retrieved " + service_id+":"+date + " -> " + retstr + " from truecache");
				return retstr;
			}
		} else {
			if (falsemap.containsKey(service_id + date)) {
				retstr = falsemap.get(service_id + date);
				// Log.v(TAG, "Retrieved " + service_id+":"+date + " -> " + retstr + " from falsecache");
				return retstr;
			}
		}

		final String[] selectargs = { service_id };
		final Cursor csr = DatabaseHelper.ReadableDB().rawQuery(mDBQuery, selectargs);
		retstr = process_db(service_id, date, limit, csr);
		csr.close();

		// Save in cache
		if (limit) {
			truemap.put(service_id + date, retstr);
		} else {
			falsemap.put(service_id + date, retstr);
		}

		return retstr;
	}

	/* Return a list of times that all busses for all routes depart a given stop, sorted by time. List is departure_time,
	 * route_id, trip_headsign. */
	public static ArrayList<String[]> getRouteDepartureTimes(String stopid, String date, boolean limittotoday,
			NotificationCallback task) {

		final String q = "select distinct departure_time as _id, trips.trip_id, routes.route_short_name, trip_headsign from stop_times "
				+ "join trips on stop_times.trip_id = trips.trip_id " + "join routes on routes.route_id = trips.route_id  "
				+ "where stop_id = ? order by departure_time";
		final String[] selectargs = new String[] { stopid };
		final Cursor csr = DatabaseHelper.ReadableDB().rawQuery(q, selectargs);

		// Load the array for the list
		final int maxcount = csr.getCount();
		int progresscount = 0;
		final ArrayList<String[]> listdetails = new ArrayList<String[]>(maxcount);

		boolean more = csr.moveToFirst();
		while (more) {

			final String trip_id = csr.getString(1);
			final String daysstr = ServiceCalendar.getTripDaysofWeek(trip_id, date, limittotoday);

			// Only add if the bus runs on this day.
			if (daysstr != null) {
				listdetails.add(new String[] { csr.getString(0), daysstr, csr.getString(2), csr.getString(3) });
			}

			if (task != null) {
				task.notificationCallback((int) ((++progresscount / (float) maxcount) * 100));
			}

			more = csr.moveToNext();
		}
		csr.close();

		return listdetails;
	}

	/* Return a list of times that all busses for a given route depart a given stop, sorted by time. List is departure_times,
	 * days of week the bus runs, and trip_id. */
	public static ArrayList<String[]> getRouteDepartureTimes(String stopid, String routeid, String headsign, String date,
			boolean limittotoday, NotificationCallback task) {

		final String q = "select distinct departure_time as _id, trip_id from stop_times where stop_id = ? and trip_id in "
				+ "(select trip_id from trips where route_id = ? and trip_headsign = ?) order by departure_time";
		final String[] selectargs = new String[] { stopid, routeid, headsign };
		final Cursor csr = DatabaseHelper.ReadableDB().rawQuery(q, selectargs);

		// Load the array for the list
		final int maxcount = csr.getCount();
		int progresscount = 0;
		final ArrayList<String[]> listdetails = new ArrayList<String[]>(maxcount);

		boolean more = csr.moveToFirst();
		while (more) {

			final String trip_id = csr.getString(1);
			final String daysstr = ServiceCalendar.getTripDaysofWeek(trip_id, date, limittotoday);

			// Only add if the bus runs on this day.
			if (daysstr != null) {
				listdetails.add(new String[] { csr.getString(0), daysstr, csr.getString(1) });
			}

			if (task != null) {
				task.notificationCallback((int) ((++progresscount / (float) maxcount) * 100));
			}

			more = csr.moveToNext();
		}
		csr.close();

		return listdetails;
	}

	/* Return the time and route details of the next bus for any route, or null if there isn't one today. */
	public static String[] getNextDepartureTime(String stopid, String date) {

		final ArrayList<String[]> listdetails = getRouteDepartureTimes(stopid, date, true, null);

		if (listdetails == null) {
			return null;
		}

		final Time t = new Time();
		t.setToNow();
		final String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);

		// Find when the next bus leaves
		for (int i = 0; i < listdetails.size(); i++) {
			final String departure_time = listdetails.get(i)[0];
			if (departure_time.compareTo(timenow) >= 0) {
				return new String[] { departure_time, listdetails.get(i)[2], listdetails.get(i)[3] };
			}
		}

		// No more busses today.
		return null;
	}

	/* Return the time of the next bus for a given route, or null if there isn't one today. */
	public static String getNextDepartureTime(String stopid, String routeid, String headsign, String date) {

		final ArrayList<String[]> listdetails = getRouteDepartureTimes(stopid, routeid, headsign, date, true, null);

		if (listdetails == null) {
			return null;
		}

		final Time t = new Time();
		t.setToNow();
		final String timenow = String.format("%02d:%02d:%02d", t.hour, t.minute, t.second);

		// Find when the next bus leaves
		for (int i = 0; i < listdetails.size(); i++) {
			final String departure_time = listdetails.get(i)[0];
			if (departure_time.compareTo(timenow) >= 0) {
				return departure_time;
			}
		}

		// No more busses today.
		return null;
	}

	/* Return a properly formatted time. Assumes nn:nn[:nn] input somewhere in the string, may return just that, or convert and
	 * add annoying American `am/pm' suffix. */
	public static String formattedTime(String time) {
		final int i = time.indexOf(':'); // Take note of where first colon is
		final int j = time.lastIndexOf(':'); // and the last.

		if (i < 0) {
			return time; // strange?
		}

		String newtime;

		// If the string contains seconds, which are always zero, truncate them.
		if (j == i + 3 && time.substring(j, j + 3).contentEquals(":00")) {
			newtime = time.substring(0, j) + time.substring(j + 3);
		} else {
			newtime = time;
		}

		if (!GRTApplication.mPreferences.showAMPMTimes()) {
			return newtime.replaceFirst(":", "h");
		}

		final String AM = "am", PM = "pm";

		// Hopefully we actually have a time
		if (i > 0) {
			int hours;
			try {
				hours = Integer.parseInt(newtime.substring(i - 2, i));
			} catch (final NumberFormatException e) {
				Log.d(TAG, "NumberFormatException: " + e.getMessage() + ", for time `" + newtime + "'");
				return newtime;
			}
			String prefix = AM;

			if (hours >= 12 && hours < 24) {
				prefix = PM;
				if (hours > 12) {
					hours -= 12;
				}
			}
			if (hours >= 24) {
				if (hours == 24) {
					hours = 12;
				} else {
					hours -= 24;
				}
			}

			// Reformat to drop leading zero, add prefix
			final int where = newtime.indexOf(" ", i); // to put the suffix
			if (where > 0) {
				newtime = String.format("%s%d%s%s%s", newtime.subSequence(0, i - 2), hours, newtime.substring(i, where),
						prefix, newtime.substring(where));
			} else { // stick it on the end
				newtime = String.format("%s%d%s%s", newtime.subSequence(0, i - 2), hours, newtime.substring(i), prefix);
			}
		}

		return newtime;
	}
}
