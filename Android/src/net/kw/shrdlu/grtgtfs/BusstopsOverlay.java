package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.database.sqlite.SQLiteDatabase;
import android.graphics.drawable.Drawable;
import android.util.Log;

import com.google.android.maps.GeoPoint;
import com.google.android.maps.ItemizedOverlay;
import com.google.android.maps.OverlayItem;

public class BusstopsOverlay extends ItemizedOverlay {
	private static final String TAG = "GrtItemizedOverlay";

	private ArrayList<OverlayItem> mOverlayItems = new ArrayList<OverlayItem>();
	private Context mContext;
	private Cursor mCsr;
	private String mStopid;
	private DatabaseHelper dbHelper;
	public static SQLiteDatabase DB = null;
	
	// Use if no limit on stops
	public BusstopsOverlay(Drawable defaultMarker, Context context) {
		this(defaultMarker, context, null);
	}

	// Used to limit which stops are displayed
	public BusstopsOverlay(Drawable defaultMarker, Context context, String whereclause) {
		super(boundCenterBottom(defaultMarker));
		mContext = context;

		// Only open the DB once. Everything else uses this copy.
		if (DB == null) {
			dbHelper = new DatabaseHelper(context);
			DB = dbHelper.getReadableDatabase();
		}

        // Waterloo
//      GeoPoint point = new GeoPoint(43462580, -80518990);

    	String q = "select stop_lat, stop_lon, stop_id, stop_name from stops";
    	if (whereclause != null)
    		q += " where " + whereclause;
    	// TODO
//    	q += " limit 100";
    	
        Cursor csr;
    	try {
    		csr = DB.rawQuery(q, null);
    	} catch (SQLException e) {
    		Log.e(TAG, "DB query failed: \"" + q + "\", " + e.getMessage());
    		return;
    	}

		boolean more = csr.moveToPosition(0);

   		while (more) {
   			int stop_lat = (int)(csr.getFloat(0) * 1000000); // microdegrees
   			int stop_lon = (int)(csr.getFloat(1) * 1000000);
   			
   			GeoPoint point = new GeoPoint(stop_lat, stop_lon);
   			OverlayItem overlayitem = new OverlayItem(point, csr.getString(2), csr.getString(3));
   		    mOverlayItems.add(overlayitem);
   	        more = csr.moveToNext();
   		}
		populate();
	}

	// This is used when a route number is clicked on in the dialog, after a stop is clicked.
	private DialogInterface.OnClickListener mClick = new DialogInterface.OnClickListener() {
		  public void onClick(DialogInterface dialog, int which) {
			  if (mCsr.moveToPosition(which)) {
				  String route = mCsr.getString(0);
				  Log.i(TAG, "clicked position " + which + ": route " + route);

				  int split = route.indexOf(" - ");
				  String route_id = route.substring(0,split);
				  String headsign = route.substring(split+3);

				  Intent bustimes = new Intent(mContext, BustimesActivity.class);
				  bustimes.putExtra("route_id", route_id);
				  bustimes.putExtra("headsign", headsign);
				  bustimes.putExtra("stop_id", mStopid);
				  mContext.startActivity(bustimes);
			  }
		  }
	  };

	  // This is called when a bus stop is clicked on in the map.
	@Override
	protected boolean onTap(int index) {
	  OverlayItem item = mOverlayItems.get(index);
	  mStopid = item.getTitle();
	  
	  AlertDialog.Builder dialog = new AlertDialog.Builder(mContext);
	  dialog.setTitle("Routes using stop " + mStopid + ", " + item.getSnippet()); 

	  // Find which routes use the given stop.
	  String q = String.format(
			  "select distinct route_id || \" - \" || trip_headsign as _id from trips where trip_id in (select trip_id from stop_times where stop_id = \"%s\")",
			  item.getTitle());
	  mCsr = DB.rawQuery(q, null);

	  dialog.setCursor(mCsr, mClick, "_id");
	  dialog.show();
	  return true;
	}

	  
	@Override
	protected OverlayItem createItem(int i) {
	  return mOverlayItems.get(i);
	}
	
	@Override
	public int size() {
		return mOverlayItems.size();
	}
}
