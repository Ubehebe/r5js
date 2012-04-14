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

/* This class is never instantiated; it's just used to double-check that
 objects that purport to provide Scheme-port-like-services do. Of course,
 the check occurs at runtime, so it's of limited helpfulness. But it is
 slightly more helpful than a null pointer exception would be a few
 frames further on in the stack.

 bl important: Port implementations must name these functions with
 string literals, not properties (PortImpl.prototype['close'] = ..., not
 PortImpl.prototype.close = ...), to prevent the Google Closure Compiler
 from renaming them and thus making them useless on the trampoline. */

function Port() {
    this.close
        = this.isCharReady
        = this.isEof
        = this.peekChar
        = this.readChar
        = this.toString
        = this.writeChar
        = function () {
        throw new InternalInterpreterError(
            'this class should never be instantiated, '
                + 'it\'s just here for documentation')
    };
}

function portImplCheck(portImplObj) {
    if (!portImplObj)
        throw new IOError(portImplObj, 'is null!');

    var required = [
        'close',
        'isCharReady',
        'isEof',
        'peekChar',
        'readChar',
        'toString',
        'writeChar'
    ];

    var useConsole = Function('return "console" in this')();

    for (var i=0; i< required.length; ++i) {
        if (typeof portImplObj[required[i]] !== 'function') {
            if (useConsole)
                console.log(portImplObj);
            throw new IOError(portImplObj
                + "doesn't have required function "
                + required[i]);
        }
    }
    return portImplObj;
}