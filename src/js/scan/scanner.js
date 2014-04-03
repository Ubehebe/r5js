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
goog.require('r5js.Datum');
goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Character');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Number');
goog.require('r5js.ast.String');



/**
 * @param {string} text Program text to scan.
 * @implements {r5js.TokenStream}
 * @struct
 * @constructor
 */
r5js.Scanner = function(text) {

  /** @const @private {string} */
  this.text_ = text;

  /** @private {number} */
  this.start_ = 0;

  /**
   * This cannot be static because JavaScript regular expressions
   * are stateful, storing the indices of the last successful match.
   * @const
   * @private {!RegExp}
   */
  this.tokenRegex_ = r5js.Scanner.newTokenRegex_();

  /**
   * R5RS 7.1.1: "Tokens which require implicit termination
   * (identifiers, numbers, characters, and dot) may be terminated
   * by any delimiter, but not necessarily by anything else."
   * @private {boolean}
   */
  this.needDelimiter_ = false;

  /** @const @private {!Array.<!r5js.Token>} */
  this.readyTokens_ = [];

  /** @private {number} */
  this.nextTokenIndex_ = 0;
};


/**
 * @param {Array.<string>} matchArray
 * @return {boolean} TODO bl.
 * @private
 */
r5js.Scanner.prototype.shouldMatchAgain_ = function(matchArray) {
  if (!matchArray) {
    return false; // eof
  } else if (this.tokenRegex_.lastIndex > this.start_ + matchArray[0].length) {
    throw new r5js.ScanError(this.text_.substr(
        this.start_, this.tokenRegex_.lastIndex - this.start_));
  } else {
    var indexOfWhitespace = 7;
    this.start_ = this.tokenRegex_.lastIndex;
    var ans = !!matchArray[indexOfWhitespace];
    /* Whitespace counts as a delimiter, so if the previous token needed
         a delimiter, we just found one. */
    if (ans) {
      this.needDelimiter_ = false;
    }
    return ans;
  }
};


/** @override */
r5js.Scanner.prototype.checkpoint = function() {
  return this.nextTokenIndex_;
};


/** @override */
r5js.Scanner.prototype.nextToken = function() {
  while (this.nextTokenIndex_ >= this.readyTokens_.length) {
    var token = this.readNextToken_();
    if (!token) {
      return null;
    }
    this.readyTokens_.push(token);
  }
  return this.readyTokens_[this.nextTokenIndex_++];
};


/**
 * Previously, the scanner created immutable Token objects that had
 * toDatum methods. When the reader failed to read a particular form,
 * all this method had to do was reset {@link nextTokenIndex_}; toDatum
 * would be called anew during the next round of reading.
 *
 * But now the Token abstraction is gone; the scanner creates (mutable) Datums
 * directly. This saves some token instantiation, but the price is that
 * this method has to remember to restore the state of the rejected datums.
 * TODO bl: investigate creating a {@link r5js.SiblingBuffer} subclass
 * that would do this automatically.
 * @override
 * @suppress {checkTypes} for setNextSibling(null) TODO bl remove
 */
r5js.Scanner.prototype.restore = function(checkpoint) {
    for (var i = checkpoint+1; i < this.readyTokens_.length; ++i) {
        var token = this.readyTokens_[i];
        if (token instanceof r5js.Datum) {
            token.setNextSibling(null);
        }
    }
  this.nextTokenIndex_ = checkpoint;

};


/**
 * @return {!r5js.Token|null}
 * @private
 */
r5js.Scanner.prototype.readNextToken_ = function() {

  var match;

  do {
    match = this.tokenRegex_.exec(this.text_);
  } while (this.shouldMatchAgain_(match));

  if (!match) {
    /* token.exec silently sets token.lastIndex to 0 on failure.
         The only way exec can currently fail is at the end of input.
         Since we want the scanner to stay at the end of input, we
         manually set token.lastIndex. */
    if (this.start_ === this.text_.length) {
      this.tokenRegex_.lastIndex = this.text_.length;
    } else {
      throw new r5js.ScanError(this.text_.substr(this.start_));
    }
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

  if (this.needDelimiter_ && !matchArray[6]) {
    /* If the previous token required a delimiter but we didn't get
         one, that's a scan error. Example: 1+2 scans as two numbers
         (1 and +2), but there has to be a delimiter between them. */
    throw new r5js.ScanError(this.text_.substr(this.tokenRegex_.lastIndex));
  } else if (matchArray[6]) {
    this.needDelimiter_ = false;
    return payload;
  } else if (matchArray[5]) {
    this.needDelimiter_ = true;
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
      return new r5js.ast.Identifier(payload/*.toLowerCase()*/);
  } else if (matchArray[2]) {
    this.needDelimiter_ = false;
    return new r5js.ast.Boolean(payload === '#t' || payload === '#T');
  } else if (matchArray[3]) {
    this.needDelimiter_ = true;
    return new r5js.ast.Character(
        r5js.Scanner.normalizeCharacterPayload_(payload));
  } else if (matchArray[4]) {
    this.needDelimiter_ = false;
      // String literals could have escaped backslashes and double quotes,
      // but we want to store them unescaped.
      // TODO bl: there seem to be no tests exercising string unescaping. Add some.
      //      replace(/\\(["\\])/g, "$1");
      var actualPayload = payload.substr(1, payload.length - 2);
      return new r5js.ast.String(actualPayload);
  } else if (matchArray[1]) {
    this.needDelimiter_ = true;
      var numericPayload = r5js.Scanner.NUMBER_FUNNY_BUSINESS_.test(payload) ?
          r5js.Scanner.parseNumericPayload_(payload) :
          parseFloat(payload);
    return new r5js.ast.Number(numericPayload);
  } else throw new r5js.InternalInterpreterError('invariant incorrect');
};

/**
 * @param {string} payload
 * @return {string}
 * @private
 */
r5js.Scanner.normalizeCharacterPayload_ = function(payload) {
    var afterSlash = payload.substr(2);
    if (afterSlash.length === 1) {
        return afterSlash;
        /* R5RS 6.3.4: "Case is significant in #\<character>, but not in
         #\<character name>.*/
    } else if (afterSlash.toLowerCase() === 'space') {
        return ' ';
    } else if (afterSlash.toLowerCase() === 'newline') {
        return '\n';
    } else {
        throw new r5js.InternalInterpreterError(
            'invalid character payload ' + payload);
    }
};

/** @const @private */ r5js.Scanner.NUMBER_FUNNY_BUSINESS_ = /[esfdli#\/]/i;

/** @const @private */ r5js.Scanner.NUMBER_FORBIDDEN_ = /[i@]/i;


/**
 * @param {string} payload
 * @return {number}
 * @private
 */
r5js.Scanner.parseNumericPayload_ = function(payload) {
    /* Get rid of all exactness annotations. Because we're
     using JavaScript math, all numbers are inexact, so the
     exactness annotations have no semantic significance. */
    payload = payload.replace(/#i|#e/i, '');

    var originalLength = payload.length;

    if (r5js.Scanner.NUMBER_FORBIDDEN_.test(payload))
        throw new r5js.ScanError('unsupported number literal: ' + payload);

    var base = 10;
    if ((payload = payload.replace(/#x/i, '')).length < originalLength) {
        base = 16;
    } else if ((payload = payload.replace(/#d/i, '')).length < originalLength) {
        // nothing to do
    } else if ((payload = payload.replace(/#o/i, '')).length < originalLength) {
        base = 8;
    } else if ((payload = payload.replace(/#b/i, '')).length < originalLength) {
        base = 2;
    }

    /* Get rid of all lone hashes. The lone hashes appear in the <decimal 10>
     rule, but don't appear to have any semantic significance. */
    payload = payload.replace('#', '');

    var maybeRational = payload.split('/');
    if (maybeRational.length === 2) {
        return parseInt(maybeRational[0], base) / parseInt(maybeRational[1], base);
    } else {
        /* If the base is 10, it could have additional features like an exponent
         or a decimal point. ([sfdl] are precision annotations for exponents,
         which we ignore.) If the base is not 10, it can't have any features
         other than a base annotation (like "#x") and a division sign, both of
         which have already been taken care of. */
        return base === 10 ?
            parseFloat(payload.replace(/[sfdl]/i, 'e')) :
            parseInt(payload, base);
    }
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
 * @return {!RegExp}
 * @private
 */
r5js.Scanner.newTokenRegex_ = function() {

  var letter = '[a-z]';
  var specialInitial = '[\\!\\$%&\\*\/\\:<\\=\\>\\?\\^_~]';
  var initial = '(?:' + letter + '|' + specialInitial + ')';
  var specialSubsequent = '[\\+\\-\\.@]';
  var subsequent = '(?:' + initial + '|\\d|' + specialSubsequent + ')';
  var peculiarIdentifier = '(?:\\+|\\-|\\.\\.\\.)';

  var identifier = '((?:' + initial + subsequent + '*' + ')|' +
      peculiarIdentifier + ')';
  var bool = '(#t|#f)';
  var character = '(#\\\\space|#\\\\newline|#\\\\.)';
  var string = '(\"(?:[^\"\\\\]|\\\\\\\"|\\\\\\\\)*\")';

  /* Tabs and carriage returns are not part of the R5RS whitespace syntax,
    but I've included them here for sanity's sake. */
  var intertokenSpace = '((?:[ \n\r\t]|;.*$|;.*[\n\r])+)';
  var specialTokens = "([\\(\\)'`\\.]|#\\(|,@|,)";

  var radix2 = '#b';
  var radix8 = '#o';
  var radix10 = '#d';
  var radix16 = '#x';

  var digit2 = '[01]';
  var digit8 = '[0-7]';
  var digit10 = '\\d';
  var digit16 = '[\\da-f]';

  var uinteger2 = '(?:' + digit2 + '+' + '#*)';
  var uinteger8 = '(?:' + digit8 + '+' + '#*)';
  var uinteger10 = '(?:' + digit10 + '+' + '#*)';
  var uinteger16 = '(?:' + digit16 + '+' + '#*)';

  var exponentMarker = '[esfdl]';
  var sign = '[\\-\\+]';
  var suffix = '(?:' + exponentMarker + sign + '?' + digit10 + '+)';

  var decimal10 = '(?:' +
      '\\.' + digit10 + '+' + '#*' + suffix + '?' + '|' +
      digit10 + '+\\.' + digit10 + '*#*' + suffix + '?|' +
      digit10 + '+' + '#+\\.#*' + suffix + '?|' +
      uinteger10 + suffix + '?)';

  var exactness = '(?:#i|#e)';

  var prefix2 = '(?:' +
      radix2 + exactness + '?|' +
      exactness + '?' + radix2 + ')';
  var prefix8 = '(?:' +
      radix8 + exactness + '?|' +
      exactness + '?' + radix8 + ')';
  var prefix10 = '(?:' +
      radix10 + exactness + '?|' +
      exactness + '?' + radix10 + '|' +
      exactness + ')';
  var prefix16 = '(?:' +
      radix16 + exactness + '?|' +
      exactness + '?' + radix16 + ')';

  var ureal2 = '(?:' +
      uinteger2 + '\\/' + uinteger2 + '|' +
      uinteger2 + ')';
  var ureal8 = '(?:' +
      uinteger8 + '\\/' + uinteger8 + '|' +
      uinteger8 + ')';
  var ureal10 = '(?:' +
      uinteger10 + '\\/' + uinteger10 + '|' +
      decimal10 + '|' + uinteger10 + ')';
  var ureal16 = '(?:' +
      uinteger16 + '\\/' + uinteger16 + '|' +
      uinteger16 + ')';

  var real2 = '(?:' + sign + '?' + ureal2 + ')';
  var real8 = '(?:' + sign + '?' + ureal8 + ')';
  var real10 = '(?:' + sign + '?' + ureal10 + ')';
  var real16 = '(?:' + sign + '?' + ureal16 + ')';

  var complex2 = '(?:' +
      real2 + '@' + real2 + '|' +
      real2 + '[\\+\\-]' + ureal2 + 'i|' +
      real2 + '[\\+\\-]i|[\\\\-]+' + ureal2 + 'i|[\\+\\-]i|' + real2 + ')';
  var complex8 = '(?:' +
      real8 + '@' + real8 + '|' +
      real8 + '[\\+\\-]' + ureal8 + 'i|' +
      real8 + '[\\+\\-]i|[\\\\-]+' + ureal8 + 'i|[\\+\\-]i|' + real8 + ')';
  var complex10 = '(?:' +
      real10 + '@' + real10 + '|' +
      real10 + '[\\+\\-]' + ureal10 + 'i|' +
      real10 + '[\\+\\-]i|[\\\\-]+' + ureal10 + 'i|[\\+\\-]i|' + real10 + ')';
  var complex16 = '(?:' +
      real16 + '@' + real16 + '|' +
      real16 + '[\\+\\-]' + ureal16 + 'i|' +
      real16 + '[\\+\\-]i|[\\\\-]+' + ureal16 + 'i|[\\+\\-]i|' + real16 + ')';

  var num2 = '(?:' + prefix2 + complex2 + ')';
  var num8 = '(?:' + prefix8 + complex8 + ')';
  var num10 = '(?:' + prefix10 + '?' + complex10 + ')';
  var num16 = '(?:' + prefix16 + complex16 + ')';

  var number = '(' + num10 + '|' + num16 + '|' + num8 + '|' + num2 + ')';

  return new RegExp(
      number /* index 1 in exec array */ + '|' +
          bool /* index 2 in exec array */ + '|' +
          character /* index 3 in exec array */ + '|' +
          string /* index 4 in exec array */ + '|' +
          identifier /* index 5 in exec array */ + '|' +
          specialTokens /* index 6 in exec array */ + '|' +
          intertokenSpace, /* index 7 in exec array */
      'gi');
};
