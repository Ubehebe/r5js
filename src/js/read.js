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


goog.provide('r5js.Reader');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.OutputMode');
goog.require('r5js.parse.Nonterminals');

/**
 * @param {!r5js.Scanner} scanner The scanner.
 * @constructor
 */
r5js.Reader = function(scanner) {
    /** @const @private {!r5js.Scanner} */
    this.scanner_ = scanner;

    /** @const @private {!Array.<!r5js.Token>} */
    this.readyTokens_ = [];

    /** @private {number} */
    this.nextTokenToReturn_ = 0;

    /** @private {r5js.Token} */
    this.errorToken_ = null;

    /** @private {string} */
    this.errorMsg_ = '';
};

/** @return {r5js.Token} */
r5js.Reader.prototype.nextToken = function() {
    while (this.nextTokenToReturn_ >= this.readyTokens_.length) {
        var token = this.scanner_.nextToken();
        if (!token)
            return null;
        this.readyTokens_.push(token);
    }
    return this.readyTokens_[this.nextTokenToReturn_++];
};

r5js.Reader.prototype.assertNextTokenType = function(type) {
    var token = this.nextToken();
    if (!token) {
        this.errorMsg_ = 'eof';
        return null;
    }
    if (token.type === type) {
        return token;
    } else {
        this.errorToken_ = token;
        this.errorMsg_ = 'expected ' + type;
        return null;
    }
};

/**
 * @param {...*} var_args
 * @return {r5js.Datum} TODO bl
 */
r5js.Reader.prototype.rhs = function(var_args) {
    var ansDatum = new r5js.Datum();
    var tokenStreamStart = this.nextTokenToReturn_;
    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        var cur;
        if (element.type === r5js.parse.Nonterminals.DATUM) {
            cur = this.onDatumOrDatums_(ansDatum, element, this.parseDatum_);
        } else if (element.type === r5js.parse.Nonterminals.DATUMS) {
            cur = this.onDatumOrDatums_(ansDatum, element, this.parseDatums_);
        } else {
            cur = this.onNonDatum_(ansDatum, element);
        }
        if (!cur) {
            this.nextTokenToReturn_ = tokenStreamStart;
            return null;
        }
    }
    return ansDatum;
};


/**
 * @param {!r5js.Datum} ansDatum
 * @param {?} element TODO bl
 * @param {function(): !r5js.Datum} parseFunction
 * @return {r5js.Datum}
 * @private
 */
r5js.Reader.prototype.onDatumOrDatums_ = function(ansDatum, element, parseFunction) {

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
            // TODO bl is this cast needed, or does it indicate a bug?
            ansDatum.appendChild(/** @type {!r5js.Datum} */ (firstChild));
            if (prev)
                prev.parent = ansDatum;
            return ansDatum;
        } else {
            this.nextTokenToReturn_ -= num;
            this.errorMsg_ = 'expected at least '
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


/**
 * @param {!r5js.Datum} ansDatum
 * @param {?} element TODO bl
 * @return {r5js.Datum}
 * @private
 */
r5js.Reader.prototype.onNonDatum_ = function(ansDatum, element) {
    var token = this.assertNextTokenType(element.type);
    if (token) {
        if (token.payload !== undefined) { // watch out for 0 and false!
            ansDatum.payload = token.payload;
            ansDatum.type = token.type;
        }
        return ansDatum;
    } else return null;
};

/**
 * @param {...*} var_args
 * TODO bl: narrow the signature.
 */
r5js.Reader.prototype.alternation = function(var_args) {
    var possibleRhs;
    // The most informative error is probably the failed parse
    // that got furthest through the input.
    var mostInformativeErrorToken = null;
    var mostInformationErrorMsg = null;
    for (var i = 0; i < arguments.length; ++i) {
        possibleRhs = this.rhs.apply(this, arguments[i]);
        if (possibleRhs)
            return possibleRhs;
        else if (!mostInformativeErrorToken) {
            mostInformativeErrorToken = this.errorToken_;
            mostInformationErrorMsg = this.errorMsg_;
        }
    }

    this.errorToken_ = mostInformativeErrorToken;
    if (mostInformationErrorMsg) {
        this.errorMsg_ = mostInformationErrorMsg;
    }
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
r5js.Reader.prototype.parseDatum_ = function() {
    return this.alternation(
        [
            {type: r5js.DatumType.IDENTIFIER}
        ],
        [
            {type: r5js.DatumType.BOOLEAN}
        ],
        [
            {type: r5js.DatumType.NUMBER}
        ],
        [
            {type: r5js.DatumType.CHARACTER}
        ],
        [
            {type: r5js.DatumType.STRING}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 0, name: r5js.DatumType.LIST},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 1, name: r5js.DatumType.DOTTED_LIST},
            {type: '.'},
            {type: 'datum', name: r5js.DatumType.DOTTED_LIST},
            {type: ')'}
        ],
        [
            {type: r5js.DatumType.VECTOR},
            {type: 'datum', atLeast: 0, name: r5js.DatumType.VECTOR},
            {type: ')'}
        ],
        [
            {type: r5js.DatumType.QUOTE},
            {type: 'datum', name: r5js.DatumType.QUOTE}
        ],
        [
            {type: r5js.DatumType.QUASIQUOTE},
            {type: 'datum', name: r5js.DatumType.QUASIQUOTE}
        ],
        [
            {type: r5js.DatumType.UNQUOTE},
            {type: 'datum', name: r5js.DatumType.UNQUOTE}
        ],
        [
            {type: r5js.DatumType.UNQUOTE_SPLICING},
            {type: 'datum', name: r5js.DatumType.UNQUOTE_SPLICING}
        ]);
};

r5js.Reader.prototype.parseDatums_ = function() {
    return this.rhs({type: 'datum', name: 'datums', atLeast: 0});
};

r5js.Reader.prototype.read = function() {
    var datums = this.parseDatums_();
    if (datums.firstChild)
        datums.firstChild.lastSibling().parent = null;
    return datums.firstChild;
};

/**
 * This is the inverse of {@link r5js.Reader.read}, which is why it's here.
 * @param {!r5js.OutputMode} outputMode Desired output mode.
 * @return {string} String representation for desired output mode.
 */
r5js.Datum.prototype.stringForOutputMode = function(outputMode) {

    var ans, child;
    var endDelimiter = "";

    switch (this.type) {
        case r5js.DatumType.FFI: // JavaScript object
            return this.payload.toString();
        case r5js.DatumType.INPUT_PORT:
            if (this.payload['isEof']())
                return 'EOF';
            // otherwise fallthrough
        case r5js.DatumType.OUTPUT_PORT:
                return this.type + ':' + this.payload.toString();
        case null:
            // Mainly for silly stuff like (cons (if #f #f) (display 'hi))
            return 'undefined';
        case r5js.DatumType.REF:
            return this.payload.stringForOutputMode(outputMode);
        case r5js.DatumType.ENVIRONMENT_SPECIFIER: // R5RS 6.5
            return this.payload === 5
                ? 'scheme-report-environment-5'
                : 'null-environment-5';
        case r5js.DatumType.LAMBDA:
            return typeof this.payload === 'function'
                ? this.name
                : 'proc:' + this.payload.name;
        case r5js.DatumType.MACRO:
            return '[macro]';
        case r5js.DatumType.IDENTIFIER:
            return /** @type {string} */ (this.payload);
        case r5js.DatumType.BOOLEAN:
            return this.payload ? '#t' : '#f';
        case r5js.DatumType.NUMBER:
            return this.payload + '';
        case r5js.DatumType.CHARACTER:
            switch (outputMode) {
                case r5js.OutputMode.WRITE:
                    if (this.payload === ' ')
                        return '#\\space';
                    else if (this.payload === '\n')
                        return '#\\newline';
                    else
                        return '#\\' + this.payload;
                case r5js.OutputMode.DISPLAY:
                default:
                    return /** @type {string} */(this.payload);
            }
            break;
        case r5js.DatumType.STRING:
            switch (outputMode) {
                case r5js.OutputMode.WRITE:
                    ans = this.payload;
                    return '"' + ans.replace(/([\\"])/g, "\\$1") + '"';
                case r5js.OutputMode.DISPLAY:
                default:
                    return /** @type {string} */ (this.payload);
            }
            break;
        case r5js.DatumType.VECTOR:
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
                case r5js.DatumType.LIST:
                    endDelimiter = ')';
                // fallthrough
                case r5js.DatumType.QUOTE:
                case r5js.DatumType.QUASIQUOTE:
                case r5js.DatumType.UNQUOTE:
                case r5js.DatumType.UNQUOTE_SPLICING:
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
                        ans += child.stringForOutputMode(outputMode) + ' ';
                    return ans
                        + (child ? child.stringForOutputMode(outputMode) : '')
                        + endDelimiter;
                case r5js.DatumType.DOTTED_LIST:
                    ans = '(';
                    for (child = this.firstChild;
                         child && child.nextSibling && child.nextSibling.nextSibling;
                         child = child.nextSibling)
                        ans += child.stringForOutputMode(outputMode) + ' ';
                    var nextToLastChildString = child
                        ? child.stringForOutputMode(outputMode)
                        : '';
                    var lastChildString = child.nextSibling ?
                        child.nextSibling.stringForOutputMode(outputMode)
                        : '';
                    return ans + nextToLastChildString + ' . ' + lastChildString + ')';
                default:
                    throw new r5js.InternalInterpreterError('unknown datum type ' + this.type);
            }
    };