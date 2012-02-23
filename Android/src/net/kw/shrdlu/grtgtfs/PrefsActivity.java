package net.kw.shrdlu.grtgtfs;

import android.os.Bundle;
import android.preference.PreferenceActivity;

public class PrefsActivity extends PreferenceActivity {

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		// Override the defaul file name, since we already use a name.
		getPreferenceManager().setSharedPreferencesName(getApplicationInfo().packageName);

		addPreferencesFromResource(R.xml.preferences);
	}

}
