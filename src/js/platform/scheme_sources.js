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

goog.provide('r5js.SchemeSources');

goog.require('goog.Promise');



/**
 * @param {string} syntax
 * @param {string} procedures
 * @struct
 * @constructor
 */
r5js.SchemeSources = function(syntax, procedures) {
  /** @const */ this.syntax = syntax;
  /** @const */ this.procedures = procedures;
};


/** @private {goog.Promise<!r5js.SchemeSources>} */
r5js.SchemeSources.sources_ = null;


/**
 * @param {function(string):!goog.Promise<string>} urlFetcher
 * @return {!goog.Promise<!r5js.SchemeSources>}
 */
r5js.SchemeSources.get = function(urlFetcher) {
  if (!r5js.SchemeSources.sources_) {
    r5js.SchemeSources.sources_ = goog.Promise.all([
      r5js.SchemeSources.urls_.SYNTAX,
      r5js.SchemeSources.urls_.PROCEDURES
    ].map(urlFetcher)).then(function(sources) {
      return new r5js.SchemeSources(sources[0], sources[1]);
    });
  }
  return r5js.SchemeSources.sources_;
};


/**
 * @enum {string}
 * @private
 */
r5js.SchemeSources.urls_ = {
  SYNTAX: '/src/scm/r5rs-syntax.scm',
  PROCEDURES: '/src/scm/r5rs-procedures.scm'
};
