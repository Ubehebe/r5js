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


goog.provide('r5js.tmp.read');


goog.require('r5js.InternalInterpreterError');

/**
 * @constructor
 */
function Reader(scanner) {
    this.scanner = scanner;
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
    this.errorToken = null;
    this.errorMsg = '';
}

Reader.prototype.nextToken = function() {
    while (this.nextTokenToReturn >= this.readyTokens.length) {
        var token = this.scanner.nextToken();
        if (!token)
            return null;
        this.readyTokens.push(token);
    }
    return this.readyTokens[this.nextTokenToReturn++];
};

Reader.prototype.assertNextTokenType = function(type) {
    var token = this.nextToken();
    if (!token) {
        this.errorMsg = 'eof';
        return null;
    }
    if (token.type === type) {
        return token;
    } else {
        this.errorToken = token;
        this.errorMsg = 'expected ' + type;
        return null;
    }
};

Reader.prototype.rhs = function() {
    var ansDatum = new Datum();
    var parseFunction;
    var tokenStreamStart = this.nextTokenToReturn;

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        var cur = (parseFunction = this[element.type])
            ? this.onNonterminal(ansDatum, element, parseFunction)
            : this.onTerminal(ansDatum, element);
        if (!cur) {
            this.nextTokenToReturn = tokenStreamStart;
            return null;
        }
    }

    return ansDatum;
};

Reader.prototype.onNonterminal = function(ansDatum, element, parseFunction) {

    // Handle * and +
    if (element.atLeast !== undefined) { // explicit undefined since atLeast 0 should be valid
        var prev, cur, firstChild;
        var num = 0;
        while (cur = parseFunction.apply(this)) {
            ++num;
            if (!firstChild)
                firstChild = cur;
            if (prev)
                prev.nextSibling = cur;
            prev = cur;
        }

        if (num >= element.atLeast) {
            ansDatum.type = element.name || element.type;
            ansDatum.appendChild(firstChild);
            if (prev)
                prev.parent = ansDatum;
            return ansDatum;
        } else {
            this.nextTokenToReturn -= num;
            this.errorMsg = 'expected at least '
                + element.atLeast + ' ' + element.nodeName + ', got ' + num;
            return null;
        }
    }

    // The normal case is exactly one of element.
    else {
        var parsed = parseFunction.apply(this);
        if (!parsed)
            return parsed;
        else {
            ansDatum.type = element.name || element.type;
            ansDatum.appendChild(parsed);
            parsed.parent = ansDatum;
            return ansDatum;
        }
    }
};

Reader.prototype.onTerminal = function(ansDatum, element) {
    var token = this.assertNextTokenType(element.type);
    if (token) {
        if (token.payload !== undefined) { // watch out for 0 and false!
            ansDatum.payload = token.payload;
            ansDatum.type = token.type;
        }
        return ansDatum;
    } else return null;
};

Reader.prototype.alternation = function() {
    var possibleRhs;
    // The most informative error is probably the failed parse
    // that got furthest through the input.
    var mostInformativeErrorToken = null;
    var mostInformationErrorMsg = null;
    for (var i = 0; i < arguments.length; ++i) {
        possibleRhs = this.rhs.apply(this, arguments[i]);
        if (possibleRhs)
            return possibleRhs;
        else if (!mostInformativeErrorToken
            || (this.errorToken && this.errorToken.stop > mostInformativeErrorToken.stop)) {
            mostInformativeErrorToken = this.errorToken;
            mostInformationErrorMsg = this.errorMsg;
        }
    }

    this.errorToken = mostInformativeErrorToken;
    this.errorMsg = mostInformationErrorMsg;
    return null;
};

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
Reader.prototype['datum'] = function() {
    return this.alternation(
        [
            {type: 'identifier'}
        ],
        [
            {type: 'boolean'}
        ],
        [
            {type: 'number'}
        ],
        [
            {type: 'character'}
        ],
        [
            {type: 'string'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 0, name: '('},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 1, name: '.('},
            {type: '.'},
            {type: 'datum', name: '.('},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'datum', atLeast: 0, name: '#('},
            {type: ')'}
        ],
        [
            {type: "'"},
            {type: 'datum', name: "'"}
        ],
        [
            {type: '`'},
            {type: 'datum', name: '`'}
        ],
        [
            {type: ','},
            {type: 'datum', name: ','}
        ],
        [
            {type: ',@'},
            {type: 'datum', name: ',@'}
        ]);
};

Reader.prototype['datums'] = function() {
    return this.rhs({type: 'datum', name: 'datums', atLeast: 0});
};

Reader.prototype.read = function() {
    var datums = this['datums']();
    if (datums.firstChild)
        datums.firstChild.lastSibling().parent = null;
    return datums.firstChild;
};

// This is the inverse of Reader.prototype.read, which is why it's here.
Datum.prototype.toString = function(outputMode) {

    var ans, child;
    var endDelimiter = "";

    switch (this.type) {
        case 'ffi': // JavaScript object
            return this.payload.toString();
        case 'input-port':
            if (this.payload['isEof']())
                return 'EOF';
            // otherwise fallthrough
        case 'output-port':
                return this.type + ':' + this.payload.toString();
        case null:
            // Mainly for silly stuff like (cons (if #f #f) (display 'hi))
            return 'undefined';
        case 'ref':
            return this.payload.toString(outputMode);
        case 'environment-specifier': // R5RS 6.5
            return this.payload === 5
                ? 'scheme-report-environment-5'
                : 'null-environment-5';
        case 'lambda':
            return typeof this.payload === 'function'
                ? this.name
                : 'proc:' + this.payload.name;
        case 'macro':
            return '[macro]';
        case 'identifier':
            return this.payload;
        case 'boolean':
            return this.payload ? '#t' : '#f';
        case 'number':
            return this.payload + '';
        case 'character':
            switch (outputMode) {
                case OutputModes.WRITE:
                    if (this.payload === ' ')
                        return '#\\space';
                    else if (this.payload === '\n')
                        return '#\\newline';
                    else
                        return '#\\' + this.payload;
                case OutputModes.DISPLAY:
                default:
                    return this.payload;
            }
            break;
        case 'string':
            switch (outputMode) {
                case OutputModes.WRITE:
                    var ans = this.payload;
                    return '"' + ans.replace(/([\\"])/g, "\\$1") + '"';
                case OutputModes.DISPLAY:
                default:
                    return this.payload;
            }
            break;
        case '#(':
                    if (this.isArrayBacked()) {
                        ans = '#(';
                        if (this.payload.length > 0) {
                            for (var i = 0; i < this.payload.length - 1; ++i)
                                ans += this.payload[i] + ' ';
                            ans += this.payload[this.payload.length - 1];
                        }
                        return ans + ')';
                    }
                // fallthrough for non-array-backed vectors
                case '(':
                    endDelimiter = ')';
                // fallthrough
                case "'":
                case '`':
                case ',':
                case ',@':
                    /* Note: this will be an infinite loop for cyclical data
                     structures created by the programmer through set-cdr!, etc.
                     Some implementations do nice things, like print "holes" where
                     a cycle starts. But the R5RS standard does not seem to define
                     external representations for lists (vectors, etc.) that contain
                     cycles. In general, the spirit of the standard seems to be that
                     the programmer is responsible for mayhem caused by the creation
                     of such structures.

                     There is one exception: list? (a library procedure) must return
                     false for cyclical lists. Accordingly, I've written the
                     cycle-detecting logic wholly in Scheme, not bothering
                     to reimplement it here. */
                    ans = this.type;
                    /* Uncomment to show quasiquotation levels.
                     (These should not make it into any external representation.)
                     if (this.qqLevel !== undefined && ans !== "'")
                     ans += 'qq' + this.qqLevel; */
                    for (child = this.firstChild;
                         child && child.nextSibling;
                         child = child.nextSibling)
                        ans += child.toString(outputMode) + ' ';
                    return ans
                        + (child ? child.toString(outputMode) : '')
                        + endDelimiter;
                case '.(':
                    ans = '(';
                    for (child = this.firstChild;
                         child && child.nextSibling && child.nextSibling.nextSibling;
                         child = child.nextSibling)
                        ans += child.toString(outputMode) + ' ';
                    var nextToLastChildString = child
                        ? child.toString(outputMode)
                        : '';
                    var lastChildString = child.nextSibling ?
                        child.nextSibling.toString(outputMode)
                        : '';
                    return ans + nextToLastChildString + ' . ' + lastChildString + ')';
                default:
                    throw new r5js.InternalInterpreterError('unknown datum type ' + this.type);
            }
    };