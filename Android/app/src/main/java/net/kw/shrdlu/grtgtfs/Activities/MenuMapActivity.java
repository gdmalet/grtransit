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

import android.Manifest;
import android.app.ActionBar;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.os.Bundle;
import android.support.v4.app.ActivityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarDrawerToggle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;

import com.google.android.gms.analytics.HitBuilders;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.UiSettings;
import com.google.android.gms.maps.MapFragment;
import com.google.android.gms.maps.OnMapReadyCallback;

import net.kw.shrdlu.grtgtfs.GRTApplication;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.NavDrawerItem;
import net.kw.shrdlu.grtgtfs.LayoutAdapters.NavDrawerListAdapter;
import net.kw.shrdlu.grtgtfs.NavOptions;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.StopsOverlay;

import java.util.ArrayList;

import static android.content.pm.PackageManager.PERMISSION_GRANTED;

public class MenuMapActivity extends Activity {
	private static final String TAG = "MenuMapActivity";

	MenuMapActivity mContext = this;
    MapFragment mMapFragment;
    GoogleMap mMap;
    StopsOverlay mStopsOverlay = null;

        // For the navigation drawer
    private DrawerLayout mDrawerLayout;
    private ListView mDrawerListView;
    private ActionBarDrawerToggle mDrawerToggle;
    private final ArrayList<NavDrawerItem> mDrawerItems = new ArrayList<>();
    private NavDrawerListAdapter mNavAdapter;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

        setContentView(R.layout.mapview);

        mMapFragment = (MapFragment) getFragmentManager().findFragmentById(R.id.mapview);
        mMapFragment.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                 if (googleMap != null) {
                     mMap = googleMap;
                     UiSettings settings = mMap.getUiSettings();
                     settings.setZoomControlsEnabled(false);
                     settings.setMyLocationButtonEnabled(false);
                     settings.setCompassEnabled(true);
                     if (mContext.checkCallingPermission(Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
                         ActivityCompat.requestPermissions(mContext, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, 42);
                     } else {
                         mMap.setMyLocationEnabled(true);
                     }
                 }
            }
        });

		getActionBar().setTitle(R.string.loading_stops);
        getActionBar().setSubtitle(null);

        mStopsOverlay = new StopsOverlay();

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
    public void onRequestPermissionsResult(int requestCode, String[] permissions, int[] grantResults) {
        //ActivityCompat.requestPermissions(mContext, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, 42);
        if (requestCode == 42 && permissions[0].equals(Manifest.permission.ACCESS_FINE_LOCATION) &&
                grantResults[0] == PERMISSION_GRANTED) {
            //if (mContext.checkCallingPermission(Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                mMap.setMyLocationEnabled(true);
            // TODO silliness - above check is supposedly redundant (but it fails?), but ide complains otherwise
        }
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

// TODO mapv2
//        mMap.getUiSettings().setCompassEnabled(true);
//        if (mContext.checkCallingPermission("android.permission.ACCESS_FINE_LOCATION") == PERMISSION_GRANTED) {
//            mMap.setMyLocationEnabled(true);
//        }
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
// TODO mapv2
//        mMap.getUiSettings().setCompassEnabled(false);
//        if (mContext.checkCallingPermission("android.permission.ACCESS_FINE_LOCATION") == PERMISSION_GRANTED) {
//            mMap.setMyLocationEnabled(false);
//        }
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
// TODO ... use selects "my location". Should be handled automatically
// https://developers.google.com/android/reference/com/google/android/gms/maps/GoogleMap.OnMyLocationButtonClickListener
//			// Center the map over the current location
//			GeoPoint locn = mMylocation.getMyLocation();
//			if (locn == null) {
//				final Location l = mMylocation.getLastFix();
//				if (l != null) {
//					Toast.makeText(mContext, R.string.last_location_fix, Toast.LENGTH_LONG).show();
//					locn = new GeoPoint((int) (l.getLatitude() * 1000000), (int) (l.getLongitude() * 1000000));
//				}
//			}
//			if (locn != null) {
//				final MapController mcp = mMapFragment.getController();
//				mcp.animateTo(locn);
//				while (mMapFragment.getZoomLevel() < 17) {
//					if (!mcp.zoomIn()) {
//						break;
//					}
//				}
//			} else {
//				Toast.makeText(mContext, R.string.no_location_fix, Toast.LENGTH_LONG).show();
//			}
			return true;
		}
		default: {
			return NavOptions.onNavOptionSelected(mContext, item.getItemId());
		}
		}
	}

//    // This is required for the virtual base class MapActivity
//	@Override
//	protected boolean isRouteDisplayed() {
//		return false;
//	}

}
