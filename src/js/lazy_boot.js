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

function LazyBoot(delegate, onBoot) {
    this.delegate = delegate;
    this.booted = false;
    this.onBoot = onBoot;
    for (var name in delegate)
        this[name] = this.wrap(delegate[name]);
}

LazyBoot.prototype.wrap = function(f) {
    var self = this;
    return function() {
        /* The actual booting blocks until done. We could change this
         to call a callback when done, but there seems no great need
         for this yet. */
        if (!self.booted) {
            self.onBoot();
            self.booted = true;
        }
        return f.apply(self.delegate, arguments);
    };
};