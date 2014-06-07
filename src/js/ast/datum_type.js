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

goog.provide('r5js.DatumType');


/** @typedef {string} */
r5js.Type;


/**
 * This is basically the enumeration of types in R5RS 3.2.
 * Ordinarily, type tags are a code smell in object-oriented code.
 * But this implementation is not completely object-oriented; in particular,
 * Scheme booleans, numbers, and symbols are represented by
 * corresponding JavaScript primitives, not objects.
 * It is convenient to have names for these types, for example
 * when printing errors during runtime type-checking. But for the most part
 * they have no critical role in this implementation.
 * @enum {!r5js.Type}
 */
r5js.DatumType = {
  BOOLEAN: 'boolean',
  CHARACTER: 'char',
  ENVIRONMENT_SPECIFIER: 'environment-specifier',
  INPUT_PORT: 'input-port',
  NUMBER: 'number',
  OUTPUT_PORT: 'output-port',
  PAIR: 'pair',
  PROCEDURE: 'procedure',
  STRING: 'string',
  SYMBOL: 'symbol',
  VECTOR: 'vector'
};

