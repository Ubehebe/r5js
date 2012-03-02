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

function Token(type) {
    this.type = type;
}

Token.prototype.numberFunnyBusiness = /[esfdli#\/]/i;
Token.prototype.numberForbidden = /[\/i@]/i;

Token.prototype.convertNumber = function (payload) {
    /* Get rid of all exactness annotations and lone hashes.
     The lone hashes appear in the <decimal 10> rule, but don't
     appear to have any semantic significance. And because we're
     using JavaScript math, all numbers are inexact, so the
     exactness annotations have no semantic significance either. */
    payload = payload.replace(/#i|#e|#/i, '');

    var originalLength = payload.length;

    if (this.numberForbidden.test(payload))
        throw new ScanError('unsupported number literal: ' + payload);

    var base = 10;
    if ((payload = payload.replace(/b/i, '')).length < originalLength) {
        base = 2;
    } else if ((payload = payload.replace(/o/i, '')).length < originalLength) {
        base = 8;
    } else if ((payload = payload.replace(/d/i, '')).length < originalLength) {
        ;
    } else if ((payload = payload.replace(/x/i, '')).length < originalLength) {
        base = 16;
    }

    // if the base is other than 10, it must be an integer
    return base === 10
        ? parseFloat(payload)
        : parseInt(payload, base);
};

Token.prototype.setPayload = function(payload) {
    /* As a small optimization, we 'evaluate' these payloads here, rather than in
     semantic actions attached in the parser. This should be a little more efficient
     when creating self-evaluating datums on the fly. For example:

     (let-syntax ((foo (syntax-rules () ((foo x) (+ x x x x x x))))) (foo 24))

     As the macro facility currently works, this will create a datum corresponding
     to 24, clone it six times, and insert the datums as siblings into the datum tree,
     bypassing the scanner. It is less work to parse the string "24" into the number
     24 before the cloning than after. */
    switch (this.type) {
        case 'identifier':
            this.payload = payload.toLowerCase();
            break;
        case 'boolean':
            this.payload = payload === '#t' || payload === '#T';
            break;
        case 'number':
            this.payload = this.numberFunnyBusiness.test(payload)
                ? this.convertNumber(payload)
                : parseFloat(payload);
            break;
        case 'character':
            var afterSlash = payload.substr(2);
            if (afterSlash.length === 1)
                this.payload = afterSlash;
            /* R5RS 6.3.4: "Case is significant in #\<character>, but not in
             #\<character name>.*/
            else if (afterSlash.toLowerCase() === 'space')
                this.payload = ' ';
            else if (afterSlash.toLowerCase() === 'newline')
                this.payload = '\n';
            else throw new InternalInterpreterError('invalid character payload ' + payload);
            break;
        case 'string':
            this.payload = payload.substr(1, payload.length - 2);
            break;
        default:
            throw new InternalInterpreterError('invalid token type ' + this.type);
    }
    return this;
};

function Scanner(text) {

    this.text = text;

    this.start = 0;

    /* Since all scanners use the same RegExp objects, we have to
     reset the RegExp's state. If concurrent scanners are ever needed,
     each will need its own RegExps. */
    this.token.lastIndex = 0;
}

// Just for debugging.
Scanner.prototype.tokenize = function() {

    var ans = [];

    var token;
    while (token = this.nextToken())
        ans.push(token);

    return ans;

};

Scanner.prototype.shouldMatchAgain = function(matchArray) {
    if (!matchArray) {
        return false; // eof
    } else {
        var indexOfWhitespace = 7;
        if (this.token.lastIndex > this.start + matchArray[0].length)
            throw new ScanError(this.text.substr(this.start, this.token.lastIndex - this.start));
        else
            this.start = this.token.lastIndex;
        return !!matchArray[indexOfWhitespace];
    }
};

Scanner.prototype.nextToken = function() {

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
            throw new ScanError(this.text.substr(this.start));
        return match;
    } else {
        return this.matchToToken(match);
    }
};

Scanner.prototype.matchToToken = function(matchArray) {
    /* See the return value of Scanner.prototype.token for the significance
     of the magic numbers here. */
    var payload = matchArray[0];
    if (matchArray[6]) {
        return new Token(payload);
    } else if (matchArray[5]) {
        return new Token('identifier').setPayload(payload);
    } else if (matchArray[2]) {
        return new Token('boolean').setPayload(payload);
    } else if (matchArray[3]) {
        return new Token('character').setPayload(payload);
    } else if (matchArray[4]) {
        return new Token('string').setPayload(payload);
    } else if (matchArray[1]) {
        return new Token('number').setPayload(payload);
    } else throw new InternalInterpreterError('invariant incorrect');
};

Scanner.prototype.token = (function() {

    /* This is basically the lexical grammar given in R5RS 7.1.1.
     It's hard to read because we have to do double backslash-escaping,
     one for string literals and one for RegExps. Example: the RegExp
     literal for matching a backslash is

     /\\/

     which is equivalent to

     new RegExp("/\\\\/").

     The order of the subgroups is quite important, because some tokens
     are prefixes of others (for example, "." and "...", "-2" vs. "-" "2".) */

    var letter = "[A-Za-z]";
    var specialInitial = "[\\!\\$%&\\*\/\\:<\\=\\>\\?\\^_~]";
    var initial = "(?:" + letter + "|" + specialInitial + ")";
    var specialSubsequent = "[\\+\\-\\.@]";
    var subsequent = "(?:" + initial + "|\\d|" + specialSubsequent + ")";
    var peculiarIdentifier = "(?:\\+|\\-|\\.\\.\\.)";

    var identifier = "((?:" + initial + subsequent + "*" + ")|" + peculiarIdentifier + ")";
    var bool = "(#t|#f)";
    var character = "(#\\\\space|#\\\\newline|#\\\\.)";
    var string = "(\"(?:[^\"\\\\]|\\\\\\\"|\\\\\\\\)*\")";

    /* Tabs are not part of the R5RS whitespace syntax, but I've included them
     here for sanity's sake. */
    var intertokenSpace = "((?:[ \n\t]|;.*$|;.*\n)+)";
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
    var prefix10 = "(?:" + radix10 + exactness + "?|" + exactness + "?" + radix10 + ")";
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