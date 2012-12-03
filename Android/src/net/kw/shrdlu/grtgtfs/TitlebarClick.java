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
import android.content.Intent;
import android.view.View;

/*
 *  Called when a button is clicked on the title bar.
 */
public class TitlebarClick {

	public static void onTitlebarClick(Activity context, View v) {
		switch (v.getId()) {
		case R.id.button_alerts: {
			Globals.tracker.trackEvent("Button", "Alerts", "", 1);
			final Intent alerts = new Intent(context, RiderAlertsActivity.class);
			context.startActivity(alerts);
			return;
		}
		case R.id.button_refresh: {
			Globals.tracker.trackEvent("Button", "Refresh", "", 1);
			context.finish();
			context.startActivity(context.getIntent());
			return;
		}
		case R.id.button_search: {
			Globals.tracker.trackEvent("Button", "Search", "", 1);
			final Intent search = new Intent(context, SearchActivity.class);
			context.startActivity(search);
			return;
		}
		}
	}
}
