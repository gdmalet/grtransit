package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.drawable.Drawable;
import android.util.Log;
import com.google.android.maps.ItemizedOverlay;
import com.google.android.maps.MapView;
import com.google.android.maps.OverlayItem;

public class GrtItemizedOverlay extends ItemizedOverlay {
	private static final String TAG = "GrtItemizedOverlay";

	private ArrayList<OverlayItem> mOverlayItems = new ArrayList<OverlayItem>();
	private Context mContext;
	private Cursor mCsr;
	private String mStopid;
	private float[] mRoute = null;
	
	public GrtItemizedOverlay(Drawable defaultMarker, Context context) {
		super(boundCenterBottom(defaultMarker));
		mContext = context;
	}

	public void stashRoute(float[] route) {
		mRoute = route;
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
	  mCsr = BusstopsActivity.DB.rawQuery(q, null);

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
	
	@Override
	public void draw(Canvas canvas, MapView view, boolean shadow) {
		if (shadow)
			return;
	
		if (mRoute != null) {
			Paint paint = new Paint(Paint.ANTI_ALIAS_FLAG);
			paint.setColor(0xffaf0000);
			canvas.drawLines(mRoute, paint);
		}
	}
	public void addOverlay(OverlayItem overlay) {
	    mOverlayItems.add(overlay);
//	    populate();
	}

	public void populateOverlay() {
		populate();
	}

}
