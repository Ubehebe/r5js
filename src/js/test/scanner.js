/* Copyright 2011-2014 Brendan Linn

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

goog.provide('r5js.test.Scanner');


goog.require('expect');
goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Character');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.Number');
goog.require('r5js.ast.String');
goog.require('scanAs');
goog.require('tdd.TestSuite');
goog.require('tdd.TestType');



/**
 * Scanning-related tests.
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Scanner = function() {};


/** @override */
r5js.test.Scanner.prototype.getType = function() {
  return tdd.TestType.UNIT;
};


/** @override */
r5js.test.Scanner.prototype.toString = function() {
  return 'r5js.test.Scanner';
};


r5js.test.Scanner.prototype['testBooleans'] = function() {
  ['#t', '#f', '#T', '#F'].forEach(function(text) {
    expect(text).to(scanAs(r5js.ast.Boolean));
  });
  ['##f', '#', '#'].forEach(function(text) {
    expect(text).not().to(scanAs(r5js.ast.Boolean));
  });
};


r5js.test.Scanner.prototype['testCharacters'] = function() {
  ['#\\c', '#\\space', '#\\newline', '#\\\\'].forEach(function(text) {
    expect(text).to(scanAs(r5js.ast.Character));
  });
};


r5js.test.Scanner.prototype['testIdentifiers'] = function() {
  ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>',
   '?', '~', '_', '^', '&+', 'h+...@@@-.'].forEach(function(text) {
    expect(text).to(scanAs(r5js.ast.Identifier));
  });
  ['|', '[', ']', '{', '}'].forEach(function(text) {
    expect(text).not().to(scanAs(r5js.ast.Identifier));
  });
};


r5js.test.Scanner.prototype['testNumbers'] = function() {
  r5js.test.Scanner.getValidNumberTokens_().forEach(function(text) {
    expect(text).to(scanAs(r5js.ast.Number));
  });
  ['1+2'].forEach(function(text) {
    expect(text).not().to(scanAs(r5js.ast.Number));
  });
};


r5js.test.Scanner.prototype['testStrings'] = function() {
  ['""', '"hello, world"', '" \\" "', '"\\\\"'].forEach(function(text) {
    expect(text).to(scanAs(r5js.ast.String));
  });
  ['"', '\\'].forEach(function(text) {
    expect(text).not().to(scanAs(r5js.ast.String));
  });
};


/**
 * @return {!Array<string>}
 * @private
 */
r5js.test.Scanner.getValidNumberTokens_ = function() {
  /* TODO bl: adding in these prefixes creates a huge number of test cases
     (more than 30,000) with about half of them failing.
    let prefixes = r5js.test.Scanner.getValidNumberPrefixes_(); */
  const suffixes = r5js.test.Scanner.getValidNumberSuffixes_();
  const validDecimals = ['8762', '-3', '4987566###', '.765', '.549867#', '0.',
    '37.###', '565.54', '3765.4499##', '4##.', '56#.', '587##.#'];
  const validNumberTokens = [];
  for (let i = 0; i < validDecimals.length; ++i) {
    for (let j = 0; j < suffixes.length; ++j) {
      validNumberTokens.push(validDecimals[i] + suffixes[j]);
    }
  }
  return validNumberTokens;
};


/**
 * @return {!Array<string>}
 * @private
 */
r5js.test.Scanner.getValidNumberPrefixes_ = function() {
  const bases = ['', '#b', '#B', '#o', '#O', '#d', '#D', '#x', '#X'];
  const exactnesses = ['', '#e', '#E', '#i', '#I'];
  const prefixes = [];
  let i, j;
  for (i = 0; i < bases.length; ++i) {
    for (j = 0; j < exactnesses.length; ++j) {
      prefixes.push(bases[i] + exactnesses[j]);
      prefixes.push(exactnesses[j] + bases[i]);
    }
  }
  return prefixes;
};


/**
 * @return {!Array<string>}
 * @private
 */
r5js.test.Scanner.getValidNumberSuffixes_ = function() {
  const exponentMarkers = ['e', 's', 'f', 'd', 'l', 'E', 'S', 'F', 'D', 'L'];
  const signs = ['', '+', '-'];
  const suffixes = [''];

  for (let i = 0; i < exponentMarkers.length; ++i) {
    for (let j = 0; j < signs.length; ++j) {
      suffixes.push(exponentMarkers[i] + signs[j] + '2387');
    }
  }
  return suffixes;
};
