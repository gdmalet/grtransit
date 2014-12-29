/*
 * Copyright 2011 Giles Malet.
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

/**
 * Wrap calls to functions that may not be in the version of the OS that we're running. This class is only instantiated if we
 * refer to it, at which point Dalvik would discover the error. So don't refer to it if we know it will fail....
 */

public class APIReflectionWrapper {

	public static class API9 {
        /*
		public static String getDisplayName(Calendar cal, int field, int style, Locale locale) {
			return cal.getDisplayName(field, style, locale);
		}
		*/
	}

	public static class API11 {

	}
}
