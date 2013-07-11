/* Copyright 2011-2013 Brendan Linn

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



goog.provide('r5js.test.fixtures');


goog.require('r5js.scan.TokenType');


/**
 * @type {!Object.<!r5js.scan.TokenType, !Array.<string>>}
 * @private
 */
r5js.test.fixtures.validTokens_ = {};


r5js.test.fixtures.validTokens_[r5js.scan.TokenType.CHARACTER] = [
    '#\\c',
    '#\\space',
    '#\\newline',
    '#\\\\'
];


r5js.test.fixtures.validTokens_[r5js.scan.TokenType.IDENTIFIER] = [
    'h',
    '+',
    '-',
    '...',
    '!',
    '$',
    '%',
    '&',
    '*',
    '/',
    ':',
    '<',
    '=',
    '>',
    '?',
    '~',
    '_',
    '^',
    '&+',
    'h+...@@@-.'
];


r5js.test.fixtures.validTokens_[r5js.scan.TokenType.STRING] = [
    '""',
    '"hello, world"',
    '" \\" "',
    '"\\\\"'
];


r5js.test.fixtures.validTokens_[r5js.scan.TokenType.BOOLEAN] = [
    '#t',
    '#f',
    '#T',
    '#F'
];


/**
 * @type {!Array.<string>}
 * @private
 */
r5js.test.fixtures.VALID_DECIMALS_ = [
    "8762",
    "-3",
    "4987566###",
    ".765",
    ".549867#",
    "0.",
    "37.###",
    "565.54",
    "3765.4499##",
    "4##.",
    "56#.",
    "587##.#"
];


/**
 * @type {!Object.<!r5js.scan.TokenType, !Array.<string>>}
 * @private
 */
r5js.test.fixtures.invalidTokens_ = {};


r5js.test.fixtures.invalidTokens_[r5js.scan.TokenType.IDENTIFIER] = [
    '|',
    '[',
    ']',
    '{',
    '}'
];


r5js.test.fixtures.invalidTokens_[r5js.scan.TokenType.CHARACTER] = [
/* todo bl */
];


r5js.test.fixtures.invalidTokens_[r5js.scan.TokenType.STRING] = [
    '"',
    "\\"
];


r5js.test.fixtures.invalidTokens_[r5js.scan.TokenType.BOOLEAN] = [
    '##f',
    '#',
    '#'
];


r5js.test.fixtures.invalidTokens_[r5js.scan.TokenType.NUMBER] = [
    '1+2'
];


/**
 * @param {!r5js.scan.TokenType} type
 * @return {!Array.<string>}
 */
r5js.test.fixtures.validTokensForType = function(type) {
    return (type === r5js.scan.TokenType.NUMBER) ?
        r5js.test.fixtures.getValidNumberTokens_() :
        r5js.test.fixtures.validTokens_[type];
};


/**
 * @param {!r5js.scan.TokenType} tokenType
 * @return {!Array.<string>}
 */
r5js.test.fixtures.invalidTokensForType = function(tokenType) {
  return r5js.test.fixtures.invalidTokens_[tokenType];
};


/**
 * @return {!Array.<string>}
 * @private
 */
r5js.test.fixtures.getValidNumberTokens_ = function() {
    /* TODO bl: adding in these prefixes creates a huge number of test cases
    (more than 30,000) with about half of them failing. */
    var prefixes = r5js.test.fixtures.getValidNumberPrefixes_();
    var suffixes = r5js.test.fixtures.getValidNumberSuffixes_();
    var decimals = r5js.test.fixtures.VALID_DECIMALS_;

    var validNumberTokens = [];
    for (var i = 0; i < decimals.length; ++i) {
        for (var j = 0; j < suffixes.length; ++j) {
            validNumberTokens.push(decimals[i] + suffixes[j]);
        }
    }
    return validNumberTokens;
};


/**
 * @return {!Array.<string>}
 * @private
 */
r5js.test.fixtures.getValidNumberPrefixes_ = function() {
    var bases = ['', '#b', '#B', '#o', '#O', '#d', '#D', '#x', '#X'];
    var exactnesses = ['', '#e', '#E', '#i', '#I'];
    var prefixes = [];
    var i, j;
    for (i = 0; i < bases.length; ++i) {
        for (j = 0; j < exactnesses.length; ++j) {
            prefixes.push(bases[i] + exactnesses[j]);
            prefixes.push(exactnesses[j] + bases[i]);
        }
    }
    return prefixes;
};


/**
 * @return {!Array.<string>}
 * @private
 */
r5js.test.fixtures.getValidNumberSuffixes_ = function() {
    var exponentMarkers = ['e', 's', 'f', 'd', 'l', 'E', 'S', 'F', 'D', 'L'];
    var signs = ['', '+', '-'];
    var suffixes = [''];

    for (var i = 0; i < exponentMarkers.length; ++i) {
        for (var j = 0; j < signs.length; ++j) {
            suffixes.push(exponentMarkers[i] + signs[j] + "2387");
        }
    }
    return suffixes;
};