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

package net.kw.shrdlu.grtgtfs.Activities;

import android.app.ActionBar;
import android.app.Activity;
import android.app.ListActivity;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarActivity;
import android.support.v7.app.ActionBarDrawerToggle;
import android.util.Log;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;

import net.kw.shrdlu.grtgtfs.GRTApplication;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.NavDrawerItem;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.NavDrawerListAdapter;
import net.kw.shrdlu.grtgtfs.NavOptions;
import net.kw.shrdlu.grtgtfs.R;

import java.util.ArrayList;

import static android.widget.ListView.OnItemClickListener;

public class MenuListActivity extends Activity {
	private static final String TAG = "MenuListActivity";

	Activity mContext;
	View mListDetail;

    // For the navigation drawer
    private DrawerLayout mDrawerLayout;
    private ListView mDrawerListView;
    private ActionBarDrawerToggle mDrawerToggle;
    private final ArrayList<NavDrawerItem> mDrawerItems = new ArrayList<>();
    private NavDrawerListAdapter mNavAdapter;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

        mListDetail = findViewById(R.id.detail_area);

        // Set what's shown on a new screen, before children change things
        getActionBar().setIcon(R.drawable.grticon_leftspace);
        getActionBar().setTitle(R.string.app_name);

        // Load up the navigation drawer
        mDrawerLayout = (DrawerLayout) findViewById(R.id.drawer_layout);
        mDrawerListView = (ListView) findViewById(R.id.left_drawer);

        mNavAdapter = new NavDrawerListAdapter(mContext, R.layout.drawer_list_item, mDrawerItems);
        mDrawerListView.setAdapter(mNavAdapter);

        mDrawerToggle = new ActionBarDrawerToggle(mContext, mDrawerLayout,
                R.string.drawer_open, R.string.drawer_close) {

            CharSequence savedtitle, savedsubtitle;

            /** Called when a drawer has settled in a completely closed state. */
            public void onDrawerClosed(View view) {
                super.onDrawerClosed(view);
                final ActionBar ab = getActionBar();
                if (ab != null) {
                    ab.setSubtitle(savedsubtitle);
                    ab.setTitle(savedtitle);
                }
                invalidateOptionsMenu(); // creates call to onPrepareOptionsMenu()
            }

            /** Called when a drawer has settled in a completely open state. */
            public void onDrawerOpened(View drawerView) {
                super.onDrawerOpened(drawerView);
                final ActionBar ab = getActionBar();
                if (ab != null) {
                    savedtitle = ab.getTitle();
                    savedsubtitle = ab.getSubtitle();
                    ab.setTitle(R.string.app_name);
                    ab.setSubtitle(null);
                }
                invalidateOptionsMenu(); // creates call to onPrepareOptionsMenu()
            }
        };

        // Set the drawer toggle as the DrawerListener
        mDrawerLayout.setDrawerListener(mDrawerToggle);

        mDrawerListView.setOnItemClickListener(new OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
                mDrawerLayout.closeDrawers();
                if (!onNavOptionSelected(mDrawerItems.get(position).getId()))
                    NavOptions.onNavOptionSelected(mContext, mDrawerItems.get(position).getId());
            }
        });

        // Display the hamburger in the home screen, else the < home symbol.
        getActionBar().setHomeButtonEnabled(true);
        getActionBar().setDisplayHomeAsUpEnabled(true);
        String lcn = mContext.getLocalClassName();
        if (lcn.equals(GRTApplication.LocalClassNameHome)) {   // home screen
            mDrawerToggle.setDrawerIndicatorEnabled(true);
        } else {
            mDrawerToggle.setDrawerIndicatorEnabled(false);
        }
    }

	@Override
	protected void onResume() {
		super.onResume();

		// We want to track a pageView every time this activity gets the focus - but if the activity was
		// previously destroyed we could have lost our global data, so this is a bit of a hack to avoid a crash!
		if (GRTApplication.tracker == null) {
			Log.e(TAG, "null tracker!");
			startActivity(new Intent(this, FavstopsActivity.class));
		}
	}

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
        // Sync the toggle state after onRestoreInstanceState has occurred.
        mDrawerToggle.syncState();
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        // Pass any configuration change to the drawer toggls
        mDrawerToggle.onConfigurationChanged(newConfig);
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		final MenuInflater inflater = getMenuInflater();

        // This can be called multiple times, for difference activities, or the
        // action bar & nav drawer. So try process things only once, else the menus are doubled up.

        // Borrow this menu for a moment to expand the nav menu first.
        if (mDrawerItems.size() == 0) {
            inflater.inflate(R.menu.navdrawermenu, menu);
            for (int i = 0; i < menu.size(); i++) {
                MenuItem item = menu.getItem(i);
                mDrawerItems.add(new NavDrawerItem(item.getIcon(), item.getTitle(), item.getItemId()));
            }
            mNavAdapter.notifyDataSetChanged();
            menu.clear();
        }

        inflater.inflate(R.menu.actionbarmenu, menu);
        return super.onCreateOptionsMenu(menu);
	}

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean drawerOpen = mDrawerLayout.isDrawerOpen(mDrawerListView);
        MenuItem item;
        item = menu.findItem(R.id.menu_search);
        if (item != null)
            item.setVisible(!drawerOpen);
        item = menu.findItem(R.id.menu_refresh);
        if (item != null)
            item.setVisible(!drawerOpen);
        return super.onPrepareOptionsMenu(menu);
    }

    // A child activity can override this if it wants to do something with a navigation
    // drawer option, else we'll just call NavOptions.onNavOptionSelected.
    boolean onNavOptionSelected(int itemid) {
        return false;
    }

    // on user selecting something from the action bar....
	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
        // if we're already on the home screen & user hit home, just toggle the drawer state
        int itemid = item.getItemId();
        String lcn = mContext.getLocalClassName();
        if (itemid == android.R.id.home) {
            if (lcn.equals(GRTApplication.LocalClassNameHome)) {
                if (mDrawerLayout.isDrawerOpen(Gravity.LEFT))
                    mDrawerLayout.closeDrawers();
                else
                    mDrawerLayout.openDrawer(Gravity.LEFT);
                return true;
            }
        }
        // Otherwise deal with the options.
        return NavOptions.onNavOptionSelected(mContext, itemid) || super.onOptionsItemSelected(item);
    }
}
