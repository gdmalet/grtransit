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
import android.view.View;
import android.widget.TextView;

public class Globals {

		public static Preferences mPreferences = null;
		public static DatabaseHelper dbHelper = null;

	    /**
	     * Show an about dialog that cites data sources.
	     */
	    public static void showAbout(Activity context) {
	    	
	        View messageView = context.getLayoutInflater().inflate(R.layout.about, null, false);

	        // When linking text, force to always use default color. This works
	        // around a pressed color state bug.
	        TextView textView = (TextView) messageView.findViewById(R.id.about_credits);
	        int defaultColor = textView.getTextColors().getDefaultColor();
	        textView.setTextColor(defaultColor);

	        AlertDialog.Builder builder = new AlertDialog.Builder(context);
	        builder.setIcon(R.drawable.grticon);
	        builder.setTitle(R.string.app_name);
	        builder.setView(messageView);
	        builder.create();
	        builder.show();
	    }

}
