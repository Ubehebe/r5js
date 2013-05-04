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


goog.provide('r5js.tmp.globals');

var nullEnv; // this is (null-environment 5)
var r5RSEnv; // this is (scheme-report-environment 5)
var debug = Function('return "console" in this;')() && false; // setting this to true shows every bounce of the trampoline
var uniqueNodeCounter = 0; // mostly for getting fresh temp variable names
