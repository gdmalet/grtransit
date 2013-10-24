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
import java.util.Calendar;
import java.util.Locale;

import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.os.StrictMode;

/**
 * Wrap calls to functions that may not be in the version of the OS that we're running. This class is only instantiated if we
 * refer to it, at which point Dalvik would discover the error. So don't refer to it if we know it will fail....
 */

public class APIReflectionWrapper {

	public static class API8 {

		public static String getDBPath(Context mContext) {
			final File f = mContext.getExternalFilesDir(null);
			if (f != null) {
				return f.getPath();
			} else {
				return null;
			}
		}
	}

	public static class API9 {

		public static void setStrictMode() {
			StrictMode.setThreadPolicy(new StrictMode.ThreadPolicy.Builder().detectNetwork().build());
			// .detectDiskReads() -- too noisy
			// .detectDiskWrites() -- too noisy
			StrictMode.setVmPolicy(new StrictMode.VmPolicy.Builder().detectLeakedSqlLiteObjects().penaltyLog().penaltyDeath()
					.build());
			// .detectLeakedClosableObjects() -- API11
			// .detectActivityLeaks() -- API11
		}

		public static String getDisplayName(Calendar cal, int field, int style, Locale locale) {
			return cal.getDisplayName(field, style, locale);
		}
	}

	public static class API11 {

		public static void prepActionBar(Activity context) {
			final ActionBar ab = context.getActionBar();
			if (ab != null) {
				ab.setHomeButtonEnabled(true);
				ab.setTitle(R.string.app_name);

				// FavstopsActivity is the home screen.
				if (context.getLocalClassName().equals("FavstopsActivity") ||
						context.getLocalClassName().equals("StartupActivity")) {
					ab.setDisplayOptions(0, ActionBar.DISPLAY_HOME_AS_UP);
				} else {
					ab.setDisplayOptions(ActionBar.DISPLAY_HOME_AS_UP, ActionBar.DISPLAY_HOME_AS_UP);
				}
			}
		}
	}
}
