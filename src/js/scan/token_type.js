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


goog.provide('r5js.scan.TokenType');


goog.require('r5js.InternalInterpreterError');


/**
 * @enum {number}
 */
r5js.scan.TokenType = {
    BOOLEAN: 0,
    CHARACTER: 1,
    IDENTIFIER: 2,
    NUMBER: 3,
    STRING: 4
};


/**
 * @param {!r5js.scan.TokenType} tokenType
 * @return {string}
 */
r5js.scan.tokenTypeName = function(tokenType) {
  switch (tokenType) {
      case r5js.scan.TokenType.BOOLEAN:
          return 'boolean';
      case r5js.scan.TokenType.CHARACTER:
          return 'character';
      case r5js.scan.TokenType.IDENTIFIER:
          return 'identifier';
      case r5js.scan.TokenType.NUMBER:
          return 'number';
      case r5js.scan.TokenType.STRING:
          return 'string';
      default:
          throw new r5js.InternalInterpreterError('enum changed');
  }
};


/**
  * @type {!Array.<!r5js.scan.TokenType>}
 */
r5js.scan.TokenType.ALL_TOKEN_TYPES = [
    r5js.scan.TokenType.BOOLEAN,
    r5js.scan.TokenType.CHARACTER,
    r5js.scan.TokenType.IDENTIFIER,
    r5js.scan.TokenType.NUMBER,
    r5js.scan.TokenType.STRING
];