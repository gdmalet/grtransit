package net.kw.shrdlu.grtgtfs.LayoutAdapters;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import net.kw.shrdlu.grtgtfs.R;

import java.util.ArrayList;

/**
 * Created by gdmalet on 30/12/14.
 */
public class NavDrawerListAdapter extends BaseAdapter {

    private ArrayList<NavDrawerItem> mNavDrawerItems;
    private final LayoutInflater mInflater;
    private final int mLayout;

    public NavDrawerListAdapter(Context context, int layout, ArrayList<NavDrawerItem> navDrawerItems){
        mInflater = LayoutInflater.from(context);
        mNavDrawerItems = navDrawerItems;
        mLayout = layout;
    }

    @Override
    public int getCount() {
        return mNavDrawerItems.size();
    }

    @Override
    public Object getItem(int position) {
        return mNavDrawerItems.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        if (convertView == null) {
            convertView = mInflater.inflate(R.layout.drawer_list_item, null);
        }

        ImageView imgIcon = (ImageView) convertView.findViewById(R.id.icon);
        TextView txtTitle = (TextView) convertView.findViewById(R.id.title);

        imgIcon.setImageDrawable(mNavDrawerItems.get(position).getIcon());
        txtTitle.setText(mNavDrawerItems.get(position).getTitle());

        return convertView;
    }

}
