package net.kw.shrdlu.grtgtfs.LayoutAdapters;

import android.graphics.drawable.Drawable;

/**
 * Created by gdmalet on 30/12/14.
 */
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
