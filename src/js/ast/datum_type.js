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
 * @enum {!r5js.Type}
 * TODO bl: remove. Type enums are inferior to subclasses as a way
 * of structuring code. There is one major place where direct type checks are
 * probably unavoidable -- runtime argument type-checking -- but that can be
 * done directly with instanceof constructor checks, or opaque isXXX()
 * functions.
 */
r5js.DatumType = {
  CHARACTER: 'char',
  ENVIRONMENT_SPECIFIER: 'environment-specifier',
  FFI: 'ffi',
  INPUT_PORT: 'input-port',
  NUMBER: 'number',
  OUTPUT_PORT: 'output-port',
  PAIR: 'pair', // TODO bl
  // TODO bl: add PROCEDURE
  STRING: 'string',
  SYMBOL: 'symbol',
  VECTOR: '#('
};
