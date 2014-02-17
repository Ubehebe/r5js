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


goog.require('r5js.bnf');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.OutputMode');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.parse.isTerminal');

/**
 * @param {!r5js.scan.TokenStream} tokenStream
 * @implements {r5js.IReader}
 * @constructor
 */
r5js.Reader = function(tokenStream) {
    /** @const @private {!r5js.scan.TokenStream} */
    this.tokenStream_ = tokenStream;

    /** @private {r5js.Token} */
    this.errorToken_ = null;

    /** @private {string} */
    this.errorMsg_ = '';

    /** @const @private {function():r5js.Datum} */
    this.parseDatumBound_ = goog.bind(this.parseDatum_, this);

    /** @const @private {function():r5js.Datum} */
    this.parseDatumsBound_ = goog.bind(this.parseDatums_, this);
};


/**
 * @param {!r5js.bnf.Rule} rule
 * @return {r5js.Datum} TODO bl
 * @private
 */
r5js.Reader.prototype.rhs_ = function(rule) {
    var ansDatum = new r5js.Datum();
    var checkpoint = this.tokenStream_.checkpoint();
        var ok = rule.match(
                ansDatum,
                this.tokenStream_,
                this.parseDatumBound_,
                this.parseDatumsBound_);
        if (!ok) {
            this.tokenStream_.restore(checkpoint);
            return null;
        }
    return ansDatum;
};


/**
 * @param {...(!r5js.bnf.Rule|!Array.<!r5js.bnf.Rule>)} var_args
 * @private
 */
r5js.Reader.prototype.alternation_ = function(var_args) {
    var possibleRhs;
    // The most informative error is probably the failed parse
    // that got furthest through the input.
    var mostInformativeErrorToken = null;
    var mostInformationErrorMsg = null;
    for (var i = 0; i < arguments.length; ++i) {
        var rule = arguments[i];
        possibleRhs = this.rhs_(rule);
        if (possibleRhs) {
            return possibleRhs;
        } else if (!mostInformativeErrorToken) {
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
    return this.alternation_(
        r5js.bnf.onePrimitive(r5js.DatumType.IDENTIFIER),
        r5js.bnf.onePrimitive(r5js.DatumType.BOOLEAN),
        r5js.bnf.onePrimitive(r5js.DatumType.NUMBER),
        r5js.bnf.onePrimitive(r5js.DatumType.CHARACTER),
        r5js.bnf.onePrimitive(r5js.DatumType.STRING),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.bnf.zeroOrMore(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.LIST),
            r5js.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.bnf.oneOrMore(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.DOTTED_LIST),
            r5js.bnf.oneTerminal(r5js.parse.Terminals.DOT),
            r5js.bnf.one(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.DOTTED_LIST),
            r5js.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.LPAREN_VECTOR),
            r5js.bnf.zeroOrMore(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.VECTOR),
            r5js.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.TICK),
            r5js.bnf.one(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.QUOTE)),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.BACKTICK),
            r5js.bnf.one(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.QUASIQUOTE)),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.COMMA),
            r5js.bnf.one(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.UNQUOTE)),
        r5js.bnf.seq(
            r5js.bnf.oneTerminal(r5js.parse.Terminals.COMMA_AT),
            r5js.bnf.one(r5js.parse.Nonterminals.DATUM).named(r5js.DatumType.UNQUOTE_SPLICING)));
};

r5js.Reader.prototype.parseDatums_ = function() {
    return this.rhs_(r5js.bnf.zeroOrMore(r5js.parse.Nonterminals.DATUM).named(r5js.parse.Nonterminals.DATUMS));
};

/** @override */
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