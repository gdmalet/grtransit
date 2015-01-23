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
import android.content.Intent;
import android.content.res.Configuration;
import android.location.Location;
import android.os.Bundle;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarDrawerToggle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.Toast;

import com.google.android.gms.analytics.HitBuilders;
import com.google.android.maps.GeoPoint;
import com.google.android.maps.MapActivity;
import com.google.android.maps.MapController;
import com.google.android.maps.MapView;
import com.google.android.maps.MyLocationOverlay;
import com.google.android.maps.Overlay;

import net.kw.shrdlu.grtgtfs.GRTApplication;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.NavDrawerItem;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.NavDrawerListAdapter;
import net.kw.shrdlu.grtgtfs.NavOptions;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.StopsOverlay;

import java.util.ArrayList;
import java.util.List;

public class MenuMapActivity extends MapActivity {
	private static final String TAG = "MenuMapActivity";

	MapActivity mContext;
    MapView mMapview;
	List<Overlay> mapOverlays;
	MyLocationOverlay mMylocation;
	StopsOverlay mStopsOverlay = null;

        // For the navigation drawer
    private DrawerLayout mDrawerLayout;
    private ListView mDrawerListView;
    private ActionBarDrawerToggle mDrawerToggle;
    final ArrayList<NavDrawerItem> mDrawerItems = new ArrayList<>();
    NavDrawerListAdapter mNavAdapter;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Make sure we get the correct API key to match the build key.
		if (GRTApplication.isDebugBuild) {
			setContentView(R.layout.mapview_debug);
		} else {
			setContentView(R.layout.mapview);
		}

		mMapview = (MapView) findViewById(R.id.mapview);
		mMapview.setBuiltInZoomControls(true);

		mapOverlays = mMapview.getOverlays();

        View mDetailArea = findViewById(R.id.mapview);

		getActionBar().setTitle(R.string.loading_stops);
        getActionBar().setSubtitle(null);

		mMylocation = new MyLocationOverlay(this, mMapview);
		mapOverlays.add(mMylocation);

		mStopsOverlay = new StopsOverlay(mContext);

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

        mDrawerListView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
                mDrawerLayout.closeDrawers();
                NavOptions.onNavOptionSelected(mContext, mDrawerItems.get(position).getId());
            }
        });

        getActionBar().setHomeButtonEnabled(true);
        getActionBar().setDisplayHomeAsUpEnabled(true);
        mDrawerToggle.setDrawerIndicatorEnabled(false);
	}

	@Override
	public void onResume() {
		super.onResume();
		// We want to track a pageView every time this activity gets the focus - but if the activity was
		// previously destroyed we could have lost our global data, so this is a bit of a hack to avoid a crash!
		if (GRTApplication.tracker == null) {
			Log.e(TAG, "null tracker!");
			startActivity(new Intent(this, FavstopsActivity.class));
		}

		mMylocation.enableMyLocation();
		mMylocation.enableCompass();
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
	public void onPause() {
		super.onPause();
		// Log.d(TAG, "onPause()");
		mMylocation.disableMyLocation();
		mMylocation.disableCompass();
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


	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
            case R.id.menu_showmap: {
            GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                    .setCategory(mContext.getLocalClassName())
                    .setAction("Menu")
                    .setLabel("My location")
                    .build());
			// Center the map over the current location
			GeoPoint locn = mMylocation.getMyLocation();
			if (locn == null) {
				final Location l = mMylocation.getLastFix();
				if (l != null) {
					Toast.makeText(mContext, R.string.last_location_fix, Toast.LENGTH_LONG).show();
					locn = new GeoPoint((int) (l.getLatitude() * 1000000), (int) (l.getLongitude() * 1000000));
				}
			}
			if (locn != null) {
				final MapController mcp = mMapview.getController();
				mcp.animateTo(locn);
				while (mMapview.getZoomLevel() < 17) {
					if (!mcp.zoomIn()) {
						break;
					}
				}
			} else {
				Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
			}
			return true;
		}
		default: {
			return NavOptions.onNavOptionSelected(mContext, item.getItemId());
		}
		}
	}

    // This is required for the virtual base class MapActivity
	@Override
	protected boolean isRouteDisplayed() {
		return false;
	}

}
