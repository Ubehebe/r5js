/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* This is a hack to prevent the Google Closure Compiler from renaming
 the GayLisp object while doing advanced optimizations. I am unsure if
 it will work server-side, since window may not be available in those
 environments. See developers.google.com/closure/compiler/docs/api-tutorial3 */

window['GayLisp'] = GayLisp;