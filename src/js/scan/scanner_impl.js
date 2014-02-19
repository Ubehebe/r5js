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


goog.provide('r5js.ScannerImpl');


goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ScanError');
goog.require('r5js.token.Boolean');
goog.require('r5js.token.Character');
goog.require('r5js.token.Identifier');
goog.require('r5js.token.Number');
goog.require('r5js.token.String');
goog.require('r5js.token.forSpecialTerminal');



/**
 * @param {string} text Program text to scan.
 * @implements {r5js.Scanner}
 * @struct
 * @constructor
 */
r5js.ScannerImpl = function(text) {

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
  this.tokenRegex_ = r5js.ScannerImpl.newTokenRegex_();

  /**
   * R5RS 7.1.1: "Tokens which require implicit termination
   * (identifiers, numbers, characters, and dot) may be terminated
   * by any delimiter, but not necessarily by anything else."
   * @private {boolean}
   */
  this.needDelimiter_ = false;
};


/**
 * @param {Array.<string>} matchArray
 * @return {boolean} TODO bl.
 * @private
 */
r5js.ScannerImpl.prototype.shouldMatchAgain_ = function(matchArray) {
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
r5js.ScannerImpl.prototype.nextToken = function() {

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
r5js.ScannerImpl.prototype.matchToToken_ = function(matchArray) {
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
    return r5js.token.forSpecialTerminal(payload);
  } else if (matchArray[5]) {
    this.needDelimiter_ = true;
    return new r5js.token.Identifier(payload);
  } else if (matchArray[2]) {
    this.needDelimiter_ = false;
    return new r5js.token.Boolean(payload);
  } else if (matchArray[3]) {
    this.needDelimiter_ = true;
    return new r5js.token.Character(payload);
  } else if (matchArray[4]) {
    this.needDelimiter_ = false;
    return new r5js.token.String(payload);
  } else if (matchArray[1]) {
    this.needDelimiter_ = true;
    return new r5js.token.Number(payload);
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
 * @return {!RegExp}
 * @private
 */
r5js.ScannerImpl.newTokenRegex_ = function() {

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
