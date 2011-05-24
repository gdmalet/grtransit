package net.kw.shrdlu.grtgtfs;

import java.util.ArrayList;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.database.Cursor;
import android.graphics.drawable.Drawable;
import android.util.Log;

import com.google.android.maps.ItemizedOverlay;
import com.google.android.maps.OverlayItem;

public class GrtItemizedOverlay extends ItemizedOverlay {
	private static final String TAG = "GrtItemizedOverlay";

	private ArrayList<OverlayItem> mOverlays = new ArrayList<OverlayItem>();
	private Context mContext;
	private Cursor mCsr;
	
	public GrtItemizedOverlay(Drawable defaultMarker, Context context) {
		super(boundCenterBottom(defaultMarker));
		mContext = context;
	}

	private DialogInterface.OnClickListener mClick = new DialogInterface.OnClickListener() {
		  public void onClick(DialogInterface dialog, int which) {
			  if (mCsr.moveToPosition(which)) {
				  String route = mCsr.getString(0);
				  Log.i(TAG, "clicked position " + which + ": route " + route);
			  }
		  }
	  };

	@Override
	protected boolean onTap(int index) {
	  OverlayItem item = mOverlays.get(index);
	  AlertDialog.Builder dialog = new AlertDialog.Builder(mContext);
	  dialog.setTitle("Routes using stop " + item.getTitle() + ": " + item.getSnippet()); 

	  // Find which routes use the given stop.
	  String q = String.format(
			  "select distinct route_id from trips where trip_id in (select trip_id from stop_times where stop_id = \"%s\")",
			  item.getTitle());
	  mCsr = grtgtfs.DB.rawQuery(q, null);

	  dialog.setCursor(mCsr, mClick, "route_id");
	  dialog.show();
	  return true;
	}

	  
	@Override
	protected OverlayItem createItem(int i) {
	  return mOverlays.get(i);
	}
	
	@Override
	public int size() {
		return mOverlays.size();
	}
	
	public void addOverlay(OverlayItem overlay) {
	    mOverlays.add(overlay);
//	    populate();
	}

	public void populateOverlay() {
		populate();
	}

}
