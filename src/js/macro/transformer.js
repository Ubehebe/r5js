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


goog.provide('r5js.Transformer');



/**
 * @param {!r5js.ListLikeTransformer} pattern
 * @param {!r5js.ListLikeTransformer} template
 * @struct
 * @constructor
 */
r5js.Transformer = function(pattern, template) {
  /** @const @private {!r5js.ListLikeTransformer} */
  this.pattern_ = pattern;

  /** @const @private {!r5js.ListLikeTransformer} */
  this.template_ = template;

  /** @const @private {string} */
  this.name_ = pattern.getName();

  /** @const @private {!Object<string, number>} */
  this.patternIds_ = {};

  /** @const @private {!Object<string, boolean>} */
  this.templateRenameCandidates_ = {};

  this.pattern_.collectNestingLevels(0, this);
  this.template_.collectNestingLevels(0, this);
};


/**
 * @param {!r5js.Datum} inputDatum The input datum.
 * @param {!Object<string, boolean>} literalIds Dictionary of literal ids.
 * @param {!r5js.IEnvironment} definitionEnv Definition environment.
 * @param {!r5js.IEnvironment} useEnv Use environment.
 * @param {!r5js.TemplateBindings} bindings Template bindings.
 * @return {boolean} True iff the transformer is a match (?).
 */
r5js.Transformer.prototype.matchInput = function(
    inputDatum, literalIds, definitionEnv, useEnv, bindings) {
  return this.pattern_.matchInput(
      inputDatum, literalIds, definitionEnv, useEnv, bindings);
};


/** @return {string} The name of this transformer. */
r5js.Transformer.prototype.getName = function() {
  return this.name_;
};


/** @return {!r5js.ListLikeTransformer} */
r5js.Transformer.prototype.getTemplate = function() {
  return this.template_;
};


/** @return {!Object<string, number>} */
r5js.Transformer.prototype.getPatternIds = function() {
  return this.patternIds_;
};


/**
 * @param {string} patternId
 * @return {number}
 */
r5js.Transformer.prototype.getEllipsisLevel = function(patternId) {
  return patternId in this.patternIds_ ? this.patternIds_[patternId] : -1;
};


/**
 * @param {string} patternId
 * @param {number} level
 */
r5js.Transformer.prototype.setEllipsisLevel = function(patternId, level) {
  this.patternIds_[patternId] = level;
};


/** @return {!Object<string, boolean>} */
r5js.Transformer.prototype.getTemplateRenameCandidates = function() {
  return this.templateRenameCandidates_;
};


/** @param {string} name */
r5js.Transformer.prototype.setTemplateRenameCandidate = function(name) {
  this.templateRenameCandidates_[name] = true;
};
