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

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

public class Realtime {

    private static final String RealtimeURL = "http://realtimemap.grt.ca/Stop/GetStopInfo?stopId=%s&routeId=%s";

    public static Map<String, Map<String, String>> GetRealtime(String stopid, String routeid) {

        final String url = String.format(RealtimeURL, stopid, routeid);
        Map<String, Map<String, String>> map = null;

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

        return map;
    }

    private static Map<String, Map<String, String>> readJsonStream(InputStream in) throws IOException {
        JsonReader reader = new JsonReader(new InputStreamReader(in, "UTF-8"));
        try {
            return readMessagesArray(reader);
        } finally {
            reader.close();
        }
    }

    private static Map<String, Map<String, String>> readMessagesArray(JsonReader reader) throws IOException {
        Map<String, Map<String, String>> messages = null;

        reader.beginObject();
        String name = reader.nextName();
        if (name.equals("status")) {
            String status = reader.nextString();
            if (status.equals("success") && reader.nextName().equals("stopTimes")) {
                messages = new HashMap<String, Map<String, String>>();
                reader.beginArray();
                while (reader.hasNext()) {
                    Map<String, String> m = readMessage(reader);
                    String TripId = m.get("TripId");
                    if (TripId != null)
                        messages.put(TripId, m);
                }
                reader.endArray();
            }
        }
        reader.endObject();

        return messages;
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
    private static Map<String, String> readMessage(JsonReader reader) throws IOException {

        Map<String, String> m = new HashMap<String, String>();

        reader.beginObject();
        while (reader.hasNext()) {
            String name = reader.nextName();
            if (name.equals("TripId")) {
                m.put("TripId", reader.nextString());
            } else if (name.equals("HeadSign")) {
                m.put("HeadSign", reader.nextString());
            } else if (name.equals("Name")) {
                m.put("Name", reader.nextString());
            } else if (name.equals("Minutes")) {
                Integer Minutes = reader.nextInt();
                m.put("Minutes", Minutes.toString());
            } else if (name.equals("ArrivalDateTime")) {
                m.put("ArrivalDateTime", reader.nextString());
            } else {
                reader.skipValue();
            }
        }
        reader.endObject();

        return m;
    }
}
