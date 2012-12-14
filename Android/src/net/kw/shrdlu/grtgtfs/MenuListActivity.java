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

import android.app.ListActivity;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.Animation.AnimationListener;
import android.view.animation.AnimationUtils;
import android.widget.ProgressBar;
import android.widget.TextView;

public class MenuListActivity extends ListActivity implements AnimationListener {
	private static final String TAG = "MenuListActivity";

	// Need one instance of this
	protected static Globals mGlobals = null;

	protected ListActivity mContext;
	protected ProgressBar mProgress;
	protected Animation mSlideIn, mSlideOut;
	protected TextView mTitle;
	protected View mListDetail;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (mGlobals == null) {
			mGlobals = new Globals(mContext);
		}

		mProgress = (ProgressBar) findViewById(R.id.progress);
		mTitle = (TextView) findViewById(R.id.listtitle);
		mListDetail = findViewById(R.id.detail_area);

		mSlideIn = AnimationUtils.loadAnimation(mContext, R.anim.slide_in);
		mSlideOut = AnimationUtils.loadAnimation(mContext, R.anim.slide_out);
		mSlideIn.setAnimationListener(this);

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB /* 11 */) {
			APIReflectionWrapper.API11.prepActionBar(mContext);
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		Log.v(TAG, "localclassname is " + getLocalClassName());
		Globals.tracker.trackPageView("/" + getLocalClassName());
	}

	// Called when a button is clicked on the title bar
	public void onTitlebarClick(View v) {
		TitlebarClick.onTitlebarClick(mContext, v);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.busstopsmenu, menu);

		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB /* 11 */) {
			// Remove search from the menu, as we put it on the title bar.
			menu.removeItem(R.id.menu_search);
		}

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		if (!TitlebarClick.onOptionsItemSelected(mContext, item)) {
			return super.onOptionsItemSelected(item);
		}
		return true;
	}

	@Override
	public void onAnimationEnd(Animation animation) {
	}

	@Override
	public void onAnimationRepeat(Animation animation) {
	}

	@Override
	public void onAnimationStart(Animation animation) {
	}
}
