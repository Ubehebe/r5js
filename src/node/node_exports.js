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

// Mostly re-export the stuff exported by api.js...
for (var name in GayLisp)
    exports[name] = GayLisp[name];

/* ...but testing is different. In a server environment, there's no point
 in keeping the tests in a separate file and pulling them in asynchronously.
 A separate file is one more thing to keep track of, and besides,
 GayLisp.evalUrl would fail because Node lacks an XMLHttpRequest object.
 So "make node" just embeds the Scheme tests in a JavaScript string literal
 named "tests" and runs directly on that. */
exports.test = function() {
    GayLisp.test(null, function(sideEffect) { console.log(sideEffect); });
    GayLisp.Eval(tests, function(sideEffect) { console.log(sideEffect); });
};