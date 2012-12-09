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

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Intent;
import android.view.MenuItem;
import android.view.View;

/*
 *  Called when a button is clicked on the title bar.
 */
public class TitlebarClick {

	public static void onTitlebarClick(Activity context, View v) {
		switch (v.getId()) {
		case R.id.button_search: {
			Globals.tracker.trackEvent("Button", "Search", "", 1);
			final Intent search = new Intent(context, SearchActivity.class);
			context.startActivity(search);
			return;
		}
		case R.id.button_overflow: {
			Globals.tracker.trackEvent("Button", "Overflow", "", 1);
			context.openOptionsMenu();
			return;
		}
		}
	}

	public static boolean onOptionsItemSelected(Activity context, MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_refresh: {
			Globals.tracker.trackEvent("Menu", "Refresh", "", 1);
			context.finish();
			context.startActivity(context.getIntent());
			return true;
		}
		case R.id.menu_alerts: {
			Globals.tracker.trackEvent("Menu", "Alerts", "", 1);
			context.startActivity(new Intent(context, RiderAlertsActivity.class));
			return true;
		}
		case R.id.menu_showmap: {
			Globals.tracker.trackEvent("Menu", "Show map", "", 1);
			context.startActivity(new Intent(context, StopsActivity.class));
			return true;
		}
		case R.id.menu_preferences: {
			Globals.tracker.trackEvent("Menu", "Preferences", "", 1);
			final Intent prefs = new Intent(context, PrefsActivity.class);
			context.startActivity(prefs);
			return true;
		}
		case R.id.menu_closeststops: {
			Globals.tracker.trackEvent("Menu", "Closest stops", "", 1);
			final Intent stops = new Intent(context, ClosestStopsActivity.class);
			context.startActivity(stops);
			return true;
		}
		case R.id.menu_about: {
			Globals.tracker.trackEvent("Menu", "Show about", "", 1);
			final View messageView = context.getLayoutInflater().inflate(R.layout.about, null, false);
			final AlertDialog.Builder builder = new AlertDialog.Builder(context);
			builder.setIcon(R.drawable.grticon);
			builder.setTitle(R.string.app_name);
			builder.setView(messageView);
			builder.create();
			builder.show();
			return true;
		}
		}
		return false;
	}
}
