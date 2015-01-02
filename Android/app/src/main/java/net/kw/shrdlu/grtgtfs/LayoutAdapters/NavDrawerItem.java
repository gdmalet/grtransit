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

import android.graphics.drawable.Drawable;

public class NavDrawerItem {

    private Drawable mIcon;
    private CharSequence mTitle;
    private int mId;

    public NavDrawerItem(){}

    public NavDrawerItem(Drawable icon, CharSequence title, int id){
        mTitle = title;
        mIcon = icon;
        mId = id;
    }

    public CharSequence getTitle(){
        return mTitle;
    }

    public Drawable getIcon(){
        return mIcon;
    }

    public int getId(){
        return mId;
    }
}
