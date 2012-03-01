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

/* Just a pointer to an environment. It's separate from the
 TrampolineResultStruct to make it clear that old environments are only
 reused in a few situations. */
function EnvBuffer() {
    this.env = null;
}

EnvBuffer.prototype.setEnv = function(env) {
    this.env = env;
};

EnvBuffer.prototype.get = function(name) {
    return this.env.get(name);
};