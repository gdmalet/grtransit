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

import android.util.JsonReader;

import com.google.android.gms.analytics.HitBuilders;

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

public class Realtime {

    private static final String RealtimeURL = "http://realtimemap.grt.ca/Stop/GetStopInfo?stopId=%s&routeId=%s";

    private String mStopid, mRouteid;

    class TripDetails extends HashMap<String, String> {}

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
        final long expirytime = new Date().getTime() + 60*1000;   // 1 minute
        //final String stopid = mStopid;
        //final String routeid = mRouteid;
    }

    public Realtime(String stopid, String routeid)
    {
        mStopid = stopid;
        mRouteid = routeid;

        GRTApplication.tracker.send(new HitBuilders.EventBuilder()
                .setCategory("Realtime")
                .setAction(stopid)
                .setLabel(routeid)
                .build());
    }

    public String getTripDetail(String trip, String var)
    {
        Realtime.RealtimeStopMap rsm = getMap();

        if (rsm != null) {
            RealtimeStop rts = rsm.get(trip);
            if (rts != null)
                return rts.get(var);
        }

        return null;
    }

    private RealtimeStopMap getMap()
    {
        if (GRTApplication.mRealtimeStops == null)
            GRTApplication.mRealtimeStops = new HashMap<>();

        // Check the cache first
        final String key = mStopid + "::" + mRouteid;
        RealtimeStopMap rts = GRTApplication.mRealtimeStops.get(key);
        if (rts != null) {
            long exp = rts.expirytime;
            if (exp < new Date().getTime()) {
                GRTApplication.mRealtimeStops.remove(key);
                rts = null;
            }
        }

        // Nothing in cache; must query.
        if (rts == null) {
            final String url = String.format(RealtimeURL, mStopid, mRouteid);

            final HttpClient client = new DefaultHttpClient();
            final HttpGet httpGet = new HttpGet(url);

            try {
                final HttpResponse response = client.execute(httpGet);
                final StatusLine statusLine = response.getStatusLine();
                final int statusCode = statusLine.getStatusCode();

                if (statusCode == 200) {
                    final HttpEntity responseEntity = response.getEntity();
                    InputStream is = responseEntity.getContent();

                    rts = readJsonStream(is);
                }
            } catch (final Exception e) {
                e.printStackTrace();
            }

            // Put this in the cache
            if (rts != null) {
                GRTApplication.mRealtimeStops.put(key, rts);
            }
        }

        // Will only return non-empty maps, or null.
        if (rts != null && !rts.isEmpty())
            return rts;
        else
            return null;
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
            switch (name) {
                case "TripId":
                    stop.details.put("TripId", reader.nextString());
                    break;
                case "HeadSign":
                    stop.details.put("HeadSign", reader.nextString());
                    break;
                case "Name":
                    stop.details.put("Name", reader.nextString());
                    break;
                case "Minutes":
                    Integer Minutes = reader.nextInt();
                    stop.details.put("Minutes", Minutes.toString());
                    break;
                case "ArrivalDateTime":
                    stop.details.put("ArrivalDateTime", reader.nextString());
                    break;
                default:
                    reader.skipValue();
                    break;
            }
        }
        reader.endObject();

        return stop;
    }
}
