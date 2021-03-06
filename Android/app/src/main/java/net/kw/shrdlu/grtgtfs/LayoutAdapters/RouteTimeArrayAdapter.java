/*
 * Copyright 2011-2015 Giles Malet.
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

package net.kw.shrdlu.grtgtfs.LayoutAdapters;

import android.app.Activity;
import android.support.annotation.NonNull;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import net.kw.shrdlu.grtgtfs.GRTApplication;
import net.kw.shrdlu.grtgtfs.R;
import net.kw.shrdlu.grtgtfs.ServiceCalendar;

import java.util.ArrayList;

public class RouteTimeArrayAdapter extends ArrayAdapter {

    private final ArrayList<String[]> mDetails;
    private final LayoutInflater mInflater;
    private final int mLayout;
    private final Activity mContext;

    public RouteTimeArrayAdapter(Activity context, int layout, ArrayList<String[]> details) {
        super(context, layout, details);

        mDetails = details;
        mInflater = LayoutInflater.from(context);
        mLayout = layout;
        mContext = context;
    }

    static class ViewHolder {
        TextView stoptime;
        TextView stopminutes;
        TextView stoprealtime;
        TextView routelabel;
        TextView routedesc;
    }

    @NonNull
    @Override
    public View getView(int position, View view, @NonNull ViewGroup parent) {

        ViewHolder holder;

        // Reuse the convertView if we already have one.... Android will create
        // only enough to fill the screen.
        if (view == null) {
            view = mInflater.inflate(mLayout, parent, false);

            // Save the view when we look them up.
            holder = new ViewHolder();
            holder.stoptime = (TextView) view.findViewById(R.id.stoptime);
            holder.stopminutes = (TextView) view.findViewById(R.id.stopminutes);
            holder.stoprealtime = (TextView) view.findViewById(R.id.stoprealtime);
            holder.routelabel = (TextView) view.findViewById(R.id.label);
            holder.routedesc = (TextView) view.findViewById(R.id.desc);
            view.setTag(holder);
        } else {
            holder = (ViewHolder) view.getTag();
        }

        final String bustime = mDetails.get(position)[0];
        holder.stoptime.setText(ServiceCalendar.formattedTime(bustime));
        int diffmins = ServiceCalendar.TimediffNow(bustime);
        holder.stopminutes.setText(ServiceCalendar.formattedMins(diffmins));

        // Show realtime data if we have it
        holder.stoprealtime.setText("");
        if (GRTApplication.mPreferences.fetchRealtime()) {
            String realtimediff = mDetails.get(position)[4];
            if (!realtimediff.equals("")) {
                diffmins = Integer.parseInt(realtimediff);
                holder.stoprealtime.setText(ServiceCalendar.formattedMins(diffmins, true));
                // highlight if there's a big difference
                if ( diffmins < -1 || diffmins > 3)
                    holder.stoprealtime.setTextColor(mContext.getResources().getColor(android.R.color.holo_red_light));
            }
        }

        // Look for things like route 7A, where the A is part of the description
        String route = mDetails.get(position)[2];
        String headsign = mDetails.get(position)[3];
        if (headsign.length() > 4
                && headsign.charAt(1) == ' '
                && headsign.charAt(2) == '-'
                && headsign.charAt(3) == ' '
                && Character.isUpperCase(headsign.charAt(0))) {
            route += headsign.charAt(0); // route number
            headsign = headsign.substring(4); // route headsign
        }

        holder.routelabel.setText(route);
        holder.routedesc.setText(headsign);

        return view;
    }
}
