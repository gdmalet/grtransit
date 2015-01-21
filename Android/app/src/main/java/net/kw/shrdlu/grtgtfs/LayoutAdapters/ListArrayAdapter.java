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

/**
 * An adapter that is used when drawing the main window list of details.
 */
package net.kw.shrdlu.grtgtfs.LayoutAdapters;

import android.app.Activity;
import android.text.Html;
import android.text.util.Linkify;
import android.text.util.Linkify.TransformFilter;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.ServiceCalendar;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ListArrayAdapter extends ArrayAdapter/* <ArrayList<String[]>> */{
	// private static final String TAG = "ListArrayAdapter";

	private final ArrayList<String[]> mDetails;
	private final LayoutInflater mInflater;
	private final int mLayout;

	// For Twitter linkify
	final Pattern mUserPattern = Pattern.compile("@([A-Za-z0-9_-]+)");
	final Pattern mHashPattern = Pattern.compile("#([A-Za-z0-9_-]+)");
	final String mUserScheme = "http://twitter.com/";
	final String mHashScheme = "http://twitter.com/search?q=%23";

	public ListArrayAdapter(Activity context, int layout, ArrayList<String[]> details) {
		super(context, layout, details);

		mDetails = details;
		mInflater = LayoutInflater.from(context);
		mLayout = layout;
	}

	static class ViewHolder {
		TextView stoptime;
		TextView desc;
        int listposition;
	}

    public static int getViewPostion(View view) {
        ViewHolder holder = (ViewHolder) view.getTag();
        return holder.listposition;
    }

	@Override
	public View getView(int position, View view, ViewGroup parent) {

		ViewHolder holder;

		// Reuse the convertView if we already have one.... Android will create
		// only enough to fill the screen.
		if (view == null) {
			view = mInflater.inflate(mLayout, parent, false);

			// Save the view when we look them up.
			holder = new ViewHolder();
			holder.stoptime = (TextView) view.findViewById(R.id.stoptime);
			holder.desc = (TextView) view.findViewById(R.id.desc);
            holder.listposition = position;

			view.setTag(holder);

		} else {
			// Log.d(TAG, "reusing view " + view);
			holder = (ViewHolder) view.getTag();
		}

		holder.stoptime.setText(ServiceCalendar.formattedTime(mDetails.get(position)[0]));

		// Linkify twitter text.
		if (mLayout == R.layout.tweetlayout) {

			// Might be things like &amp; in there....
			holder.desc.setText(Html.fromHtml(mDetails.get(position)[1]));

			// Transform filter returns just the text captured by the first regular expression group.
			final TransformFilter TFilter = new TransformFilter() {
				@Override
				public final String transformUrl(final Matcher match, String url) {
					return match.group(1);
				}
			};

			Linkify.addLinks(holder.desc, Linkify.ALL); // the defaults
			Linkify.addLinks(holder.desc, mUserPattern, mUserScheme, null, TFilter);
			Linkify.addLinks(holder.desc, mHashPattern, mHashScheme, null, TFilter);
		} else {
			holder.desc.setText(mDetails.get(position)[1]);
		}

		return view;
	}
}
