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

function NodeBackedPort(filename, mode) {

    /* We set this inside the constructor instead of the usual way
     so that a ReferenceError isn't thrown during parsing. */
    if (!NodeBackedPort.prototype.fsModule) {
        try {
            /* Of course, require might be defined but do something other
             than what we expect, which is to import the filesystem module.
             We don't check for that. */
            NodeBackedPort.prototype.fsModule = require('fs');
        } catch (re) {
            if (re instanceof ReferenceError) {
                throw new IOError("the JavaScript environment lacks filesystem access required for this IO procedure. "
                + "(This probably means you are running in a browser.)");
            }
        }
    }

    this.fd = this.fsModule.openSync(filename, mode);
    this.size = this.fsModule.statSync(filename).size;
    this.offset = 0;
}

/* We name the functions with string literals, not properties, to
 prevent the Google Closure Compiler from renaming them. See comments
 at Port. */

NodeBackedPort.prototype['close'] = function() {
    this.fsModule.closeSync(this.fd);
};

NodeBackedPort.prototype['isCharReady'] = function() {
    return true;
};

NodeBackedPort.prototype['isEof'] = function() {
    return this.offset >= this.size;
};

NodeBackedPort.prototype['peekChar'] = function() {
    return this.isEof()
        ? this
        : this.fsModule.readSync(this.fd, 1, this.offset)[0];
};

NodeBackedPort.prototype['readChar'] = function() {
    return this.isEof()
        ? this
        : this.fsModule.readSync(this.fd, 1, this.offset++)[0];
};

NodeBackedPort.prototype['toString'] = function() {
    return String(this.fd);
};

NodeBackedPort.prototype['write'] = function(str) {
    this.fsModule.writeSync(this.fd, str, null);
};

NodeBackedPort.prototype['writeChar'] = function(c) {
    this.fsModule.writeSync(this.fd, c, null);
};


