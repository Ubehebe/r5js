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


goog.provide('r5js.Scanner');


goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ScanError');

/**
 * @param {string} type
 * @implements {r5js.Token}
 * @struct
 * @constructor
 * @private
 */
function Token_(type) {
    /** @const {string} */
    this.type = type;

    /** @private {?} */
    this.payload_ = undefined;
}

/** @override */
Token_.prototype.getPayload = function() {
    return this.payload_;
};

Token_.prototype.numberFunnyBusiness = /[esfdli#\/]/i;
Token_.prototype.numberForbidden = /[i@]/i;

Token_.prototype.convertNumber = function (payload) {

    /* Get rid of all exactness annotations. Because we're
     using JavaScript math, all numbers are inexact, so the
     exactness annotations have no semantic significance. */
    payload = payload.replace(/#i|#e/i, '');

    var originalLength = payload.length;

    if (this.numberForbidden.test(payload))
        throw new r5js.ScanError('unsupported number literal: ' + payload);

    var base = 10;
    if ((payload = payload.replace(/#x/i, '')).length < originalLength) {
        base = 16;
    } else if ((payload = payload.replace(/#d/i, '')).length < originalLength) {
        ;
    } else if ((payload = payload.replace(/#o/i, '')).length < originalLength) {
        base = 8;
    } else if ((payload = payload.replace(/#b/i, '')).length < originalLength) {
        base = 2;
    }

    /* Get rid of all lone hashes. The lone hashes appear in the <decimal 10>
     rule, but don't appear to have any semantic significance. */
    payload = payload.replace('#', '');

    var maybeRational = payload.split('/');
    if (maybeRational.length === 2)
        return parseInt(maybeRational[0], base) / parseInt(maybeRational[1], base);

    /* If the base is 10, it could have additional features like an exponent
     or a decimal point. ([sfdl] are precision annotations for exponents, which
     we ignore.) If the base is not 10, it can't have any features
     other than a base annotation (like "#x") and a division sign, both of
     which have already been taken care of. */
    else return base === 10
        ? parseFloat(payload.replace(/[sfdl]/i, 'e'))
        : parseInt(payload, base);
};

Token_.prototype.setPayload = function(payload) {
    switch (this.type) {
        case 'identifier':
            /* Converting Scheme identifiers to a canonical case makes
             interoperability with JavaScript awkward. For example:

             (((window 'document) 'querySelector) "body")

             If querySelector is lowercased to queryselector, we might
             have to search the receiver case-insensitively, which would
             compromise correctness. Alternatively (but more syntactically rude)
             we could require JS method names to be string literals.

             I see little downside to making Scheme case-sensitive
             (and R6RS might require it, I haven't looked), so I went ahead
             and did it, commenting out the few test cases that thereby failed. */
            this.payload_ = payload/*.toLowerCase()*/;
            break;
        case 'boolean':
            this.payload_ = payload === '#t' || payload === '#T';
            break;
        case 'number':
            this.payload_ = this.numberFunnyBusiness.test(payload)
                ? this.convertNumber(payload)
                : parseFloat(payload);
            break;
        case 'character':
            var afterSlash = payload.substr(2);
            if (afterSlash.length === 1)
                this.payload_ = afterSlash;
            /* R5RS 6.3.4: "Case is significant in #\<character>, but not in
             #\<character name>.*/
            else if (afterSlash.toLowerCase() === 'space')
                this.payload_ = ' ';
            else if (afterSlash.toLowerCase() === 'newline')
                this.payload_ = '\n';
            else throw new r5js.InternalInterpreterError('invalid character payload ' + payload);
            break;
        case 'string':
            this.payload_ = payload.substr(1, payload.length - 2);
            break;
        default:
            throw new r5js.InternalInterpreterError('invalid token type ' + this.type);
    }
    return this;
};

/**
 * @param {string} text Program text to scan.
 * @implements {r5js.IScanner}
 * @constructor
 */
r5js.Scanner = function(text) {

    /**
     * @type {string}
     */
    this.text = text;

    /**
     * @type {number}
     */
    this.start = 0;

    /**
     * Since all scanners use the same RegExp objects, we have to reset
     * the RegExp's state. If concurrent scanners are ever needed,
     * each will need its own RegExps.
     * @type {number}
     */
    this.token.lastIndex = 0;

    /**
     * R5RS 7.1.1: "Tokens which require implicit termination
     * (identifiers, numbers, characters, and dot) may be terminated
     * by any delimiter, but not necessarily by anything else."
     * @type {boolean}
     */
    this.needDelimiter = false;
};

/**
 * Just for debugging.
 * @return {!Array.<!r5js.Token>}
 */
r5js.Scanner.prototype.tokenize = function() {

    var ans = [];

    var token;
    while (token = this.nextToken())
        ans.push(token);

    return ans;

};


/**
 * @param matchArray
 * @return {boolean} True iff
 */
r5js.Scanner.prototype.shouldMatchAgain = function(matchArray) {
    if (!matchArray) {
        return false; // eof
    } else if (this.token.lastIndex > this.start + matchArray[0].length) {
        throw new r5js.ScanError(this.text.substr(this.start, this.token.lastIndex - this.start));
    } else {
        var indexOfWhitespace = 7;
        this.start = this.token.lastIndex;
        var ans = !!matchArray[indexOfWhitespace];
        /* Whitespace counts as a delimiter, so if the previous token needed
         a delimiter, we just found one. */
        if (ans)
            this.needDelimiter = false;
        return ans;
    }
};


/** @override */
r5js.Scanner.prototype.nextToken = function() {

    var match;

    do {
        match = this.token.exec(this.text);
    } while (this.shouldMatchAgain(match));

    if (!match) {
        /* token.exec silently sets token.lastIndex to 0 on failure.
         The only way exec can currently fail is at the end of input.
         Since we want the scanner to stay at the end of input, we
         manually set token.lastIndex. */
        if (this.start === this.text.length)
            this.token.lastIndex = this.text.length;
        else
            throw new r5js.ScanError(this.text.substr(this.start));
        return match;
    } else {
        return this.matchToToken_(match);
    }
};


/**
 * @param {!Array.<string>} matchArray
 * @return {r5js.Token}
 * @private
 */
r5js.Scanner.prototype.matchToToken_ = function(matchArray) {
    /* See the return value of Scanner.prototype.token for the significance
     of the magic numbers here. */
    var payload = matchArray[0];

    if (this.needDelimiter && !matchArray[6]) {
        /* If the previous token required a delimiter but we didn't get
         one, that's a scan error. Example: 1+2 scans as two numbers
         (1 and +2), but there has to be a delimiter between them. */
        throw new r5js.ScanError(this.text.substr(this.token.lastIndex));
    } else if (matchArray[6]) {
        this.needDelimiter = false;
        return new Token_(payload);
    } else if (matchArray[5]) {
        this.needDelimiter = true;
        return new Token_('identifier').setPayload(payload);
    } else if (matchArray[2]) {
        this.needDelimiter = false;
        return new Token_('boolean').setPayload(payload);
    } else if (matchArray[3]) {
        this.needDelimiter = true;
        return new Token_('character').setPayload(payload);
    } else if (matchArray[4]) {
        this.needDelimiter = false;
        return new Token_('string').setPayload(payload);
    } else if (matchArray[1]) {
        this.needDelimiter = true;
        return new Token_('number').setPayload(payload);
    } else throw new r5js.InternalInterpreterError('invariant incorrect');
};


/**
 * This is basically the lexical grammar given in R5RS 7.1.1.
 * It's hard to read because we have to do double backslash-escaping,
 * one for string literals and one for RegExps. Example: the RegExp
 * literal for matching a backslash is
 *
 * /\\/
 *
 * which is equivalent to
 *
 * new RegExp("/\\\\/").
 *
 * The order of the subgroups is quite important, because some tokens
 * are prefixes of others (for example, "." and "...", "-2" vs. "-" "2".)
 */
 r5js.Scanner.prototype.token = (function() {



    var letter = "[a-z]";
    var specialInitial = "[\\!\\$%&\\*\/\\:<\\=\\>\\?\\^_~]";
    var initial = "(?:" + letter + "|" + specialInitial + ")";
    var specialSubsequent = "[\\+\\-\\.@]";
    var subsequent = "(?:" + initial + "|\\d|" + specialSubsequent + ")";
    var peculiarIdentifier = "(?:\\+|\\-|\\.\\.\\.)";

    var identifier = "((?:" + initial + subsequent + "*" + ")|" + peculiarIdentifier + ")";
    var bool = "(#t|#f)";
    var character = "(#\\\\space|#\\\\newline|#\\\\.)";
    var string = "(\"(?:[^\"\\\\]|\\\\\\\"|\\\\\\\\)*\")";

    /* Tabs and carriage returns are not part of the R5RS whitespace syntax,
    but I've included them here for sanity's sake. */
    var intertokenSpace = "((?:[ \n\r\t]|;.*$|;.*[\n\r])+)";
    var specialTokens = "([\\(\\)'`\\.]|#\\(|,@|,)";

    var radix2 = "#b";
    var radix8 = "#o";
    var radix10 = "#d";
    var radix16 = "#x";

    var digit2 = "[01]";
    var digit8 = "[0-7]";
    var digit10 = "\\d";
    var digit16 = "[\\da-f]";

    var uinteger2 = "(?:" + digit2 + "+" + "#*)";
    var uinteger8 = "(?:" + digit8 + "+" + "#*)";
    var uinteger10 = "(?:" + digit10 + "+" + "#*)";
    var uinteger16 = "(?:" + digit16 + "+" + "#*)";

    var exponentMarker = "[esfdl]";
    var sign = "[\\-\\+]";
    var suffix = "(?:" + exponentMarker + sign + "?" + digit10 + "+)";

    var decimal10 = "(?:"
        + "\\." + digit10 + "+" + "#*" + suffix + "?"
        + "|" + digit10 + "+\\." + digit10 + "*#*" + suffix + "?"
        + "|" + digit10 + "+" + "#+\\.#*" + suffix + "?"
        + "|" + uinteger10 + suffix + "?"
        + ")";

    var exactness = "(?:#i|#e)";

    var prefix2 = "(?:" + radix2 + exactness + "?|" + exactness + "?" + radix2 + ")";
    var prefix8 = "(?:" + radix8 + exactness + "?|" + exactness + "?" + radix8 + ")";
    var prefix10 = "(?:" + radix10 + exactness + "?|" + exactness + "?" + radix10 + "|" + exactness + ")";
    var prefix16 = "(?:" + radix16 + exactness + "?|" + exactness + "?" + radix16 + ")";

    var ureal2 = "(?:" + uinteger2 + "\\/" + uinteger2 + "|" + uinteger2 + ")";
    var ureal8 = "(?:" + uinteger8 + "\\/" + uinteger8 + "|" + uinteger8 + ")";
    var ureal10 = "(?:" + uinteger10 + "\\/" + uinteger10 + "|" + decimal10 + "|" + uinteger10 + ")";
    var ureal16 = "(?:" + uinteger16 + "\\/" + uinteger16 + "|" + uinteger16 + ")";

    var real2 = "(?:" + sign + "?" + ureal2 + ")";
    var real8 = "(?:" + sign + "?" + ureal8 + ")";
    var real10 = "(?:" + sign + "?" + ureal10 + ")";
    var real16 = "(?:" + sign + "?" + ureal16 + ")";

    var complex2 = "(?:" + real2 + "@" + real2 + "|" + real2 + "[\\+\\-]" + ureal2 + "i|" + real2 + "[\\+\\-]i|[\\\\-]+" + ureal2 + "i|[\\+\\-]i|" + real2 + ")";
    var complex8 = "(?:" + real8 + "@" + real8 + "|" + real8 + "[\\+\\-]" + ureal8 + "i|" + real8 + "[\\+\\-]i|[\\\\-]+" + ureal8 + "i|[\\+\\-]i|" + real8 + ")";
    var complex10 = "(?:" + real10 + "@" + real10 + "|" + real10 + "[\\+\\-]" + ureal10 + "i|" + real10 + "[\\+\\-]i|[\\\\-]+" + ureal10 + "i|[\\+\\-]i|" + real10 + ")";
    var complex16 = "(?:" + real16 + "@" + real16 + "|" + real16 + "[\\+\\-]" + ureal16 + "i|" + real16 + "[\\+\\-]i|[\\\\-]+" + ureal16 + "i|[\\+\\-]i|" + real16 + ")";

    var num2 = "(?:" + prefix2 + complex2 + ")";
    var num8 = "(?:" + prefix8 + complex8 + ")";
    var num10 = "(?:" + prefix10 + "?" + complex10 + ")";
    var num16 = "(?:" + prefix16 + complex16 + ")";

    var number = "(" + num10 + "|" + num16 + "|" + num8 + "|" + num2 + ")";

    return new RegExp(
        number // index 1 in exec array
            + "|" + bool // index 2 in exec array
            + "|" + character // index 3 in exec array
            + "|" + string // index 4 in exec array
            + "|" + identifier // index 5 in exec array
            + "|" + specialTokens // index 6 in exec array
            + "|" + intertokenSpace, // index 7 in exec array
        "gi");
})();