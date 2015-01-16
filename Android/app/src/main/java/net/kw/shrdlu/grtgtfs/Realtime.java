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

package net.kw.shrdlu.grtgtfs;

import android.text.format.Time;
import android.util.JsonReader;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class Realtime {

    private static final String RealtimeURL = "http://realtimemap.grt.ca/Stop/GetStopInfo?stopId=%s&routeId=%s";

    private String mStopid, mRouteid;
    private RealtimeStopMap mMap = null;

    class TripDetails extends HashMap<String, String> {};

    public class RealtimeStop {
        TripDetails details = new TripDetails();

        public String get(String var)
        {
            return details.get(var);
        }

    }
    // Trip id mapped to the realtime data.
    public class RealtimeStopMap extends HashMap<String, RealtimeStop>
    {
        long snarfedat = new Date().getTime();
        String stopid = mStopid;
        String routeid = mRouteid;
    }

    public RealtimeStopMap getMap()
    {
        // Will only return non-empty maps, or null.
        return mMap;
    }

    // TODO -- should cache the maps & return anything under a minute old, else re-query.
    public Realtime(String stopid, String routeid)
    {
        mStopid = stopid;
        mRouteid = routeid;
        final String url = String.format(RealtimeURL, stopid, routeid);
        RealtimeStopMap map = null;

        final HttpClient client = new DefaultHttpClient();
        final HttpGet httpGet = new HttpGet(url);

        try {
            final HttpResponse response = client.execute(httpGet);
            final StatusLine statusLine = response.getStatusLine();
            final int statusCode = statusLine.getStatusCode();

            if (statusCode == 200) {
                final HttpEntity responseEntity = response.getEntity();
                InputStream is = responseEntity.getContent();

                map = readJsonStream(is);
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }

        // Only store non-empty maps
        if (map != null && !map.isEmpty())
            mMap = map;
    }

    private RealtimeStopMap readJsonStream(InputStream in) throws IOException
    {
        JsonReader reader = new JsonReader(new InputStreamReader(in, "UTF-8"));

        try {
            return readStopTimesArray(reader);
        } finally {
            reader.close();
        }
    }

    private RealtimeStopMap readStopTimesArray(JsonReader reader) throws IOException
    {
        RealtimeStopMap rsm = null;

        reader.beginObject();
        String name = reader.nextName();
        if (name.equals("status")) {
            String status = reader.nextString();
            if (status.equals("success") && reader.nextName().equals("stopTimes")) {
                rsm = new RealtimeStopMap();
                reader.beginArray();
                while (reader.hasNext()) {
                    RealtimeStop td = readTripDetails(reader);
                    String TripId = td.details.get("TripId");
                    if (TripId != null)
                        rsm.put(TripId, td);
                }
                reader.endArray();
            }
        }
        reader.endObject();

        return rsm;
    }

//  {
//      status: "success",
//      stopTimes: [
//          {
//              TripId: "101943501",
//              VehicleId: "",
//              HeadSign: "7D UW via University",
//              Name: "7 - Mainline to 7D UW via University",
//              Minutes: 4,
//              ArrivalDateTime: "/Date(1420951500000)/"
//          }
//      ]
//  }
    // Return a hash of all the values we're interested in.
    private RealtimeStop readTripDetails(JsonReader reader) throws IOException
    {
        RealtimeStop stop = new RealtimeStop();

        reader.beginObject();
        while (reader.hasNext()) {
            String name = reader.nextName();
            if (name.equals("TripId")) {
                stop.details.put("TripId", reader.nextString());
            } else if (name.equals("HeadSign")) {
                stop.details.put("HeadSign", reader.nextString());
            } else if (name.equals("Name")) {
                stop.details.put("Name", reader.nextString());
            } else if (name.equals("Minutes")) {
                Integer Minutes = reader.nextInt();
                stop.details.put("Minutes", Minutes.toString());
            } else if (name.equals("ArrivalDateTime")) {
                stop.details.put("ArrivalDateTime", reader.nextString());
            } else {
                reader.skipValue();
            }
        }
        reader.endObject();

        return stop;
    }
}
