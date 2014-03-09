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


goog.provide('r5js.Transformer');


goog.require('r5js.EllipsisTransformer');
goog.require('r5js.IdOrLiteralTransformer');
goog.require('r5js.MacroError');



/**
 * @param {!r5js.ListLikeTransformer} pattern
 * @param {!r5js.ListLikeTransformer} template
 * @struct
 * @constructor
 * @suppress {checkTypes} TODO bl
 */
r5js.Transformer = function(pattern, template) {
  /** @const @private {!r5js.ListLikeTransformer} */
  this.pattern_ = pattern;

  /** @const @private {!r5js.ListLikeTransformer} */
  this.template_ = template;

  /** @const @private {string} */
  this.name_ = pattern.getName();

  /** @const @private {!Object.<string, number>} */
  this.patternIds_ = {};

  /** @const @private {!Object.<string, boolean>} */
  this.templateRenameCandidates_ = {};

  this.pattern_.forEachSubtransformer(this.patternFn_, 0, this);
  this.template_.forEachSubtransformer(this.templateFn_, 0, this);
};


/**
 * @param {!r5js.Datum} inputDatum The input datum.
 * @param {!Object.<string, boolean>} literalIds Dictionary of literal ids.
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


/**
 * @param {!r5js.ITransformer} subtransformer
 * @param {number} ellipsisLevel
 * @private
 * @suppress {checkTypes} TODO bl
 */
r5js.Transformer.prototype.patternFn_ = function(
    subtransformer, ellipsisLevel) {
  if (subtransformer instanceof r5js.IdOrLiteralTransformer) {
    if (subtransformer.datum.isIdentifier() &&
        subtransformer.datum.getPayload() !== this.name_) {
      this.patternIds_[subtransformer.datum.getPayload()] = ellipsisLevel;
    }
  } else subtransformer.forEachSubtransformer(
      this.patternFn_,
      subtransformer instanceof r5js.EllipsisTransformer ?
      ellipsisLevel + 1 :
      ellipsisLevel,
      this);
};


/**
 * @param {!r5js.ITransformer} subtransformer
 * @param {number} ellipsisLevel
 * @private
 * @suppress {checkTypes} TODO bl
 */
r5js.Transformer.prototype.templateFn_ = function(
    subtransformer, ellipsisLevel) {
  if (subtransformer instanceof r5js.IdOrLiteralTransformer) {
    if (subtransformer.datum.isIdentifier()) {
      var name = subtransformer.datum.getPayload();
      var maybeInPattern = this.patternIds_[name];
      /* An identifier in a template is a candidate for being
         renamed during transcription if it doesn't occur in the pattern
         and is not the name of the macro. I've also thrown in a check
         that it's not a parser-sensititive identifier so we don't
         accidentally break the parser, but this may be buggy.
         The right thing to do is to remove the parser altogether.
         See comments at the top of Parser. */
      if (maybeInPattern === undefined &&
          name !== this.name_) {
        if (!isParserSensitiveId(name))
          this.templateRenameCandidates_[name] = true;
      } else if (maybeInPattern !== ellipsisLevel &&
          name !== this.name_) {
        throw new r5js.MacroError(
            this.name_,
            name +
            ' is at ellipsis level ' +
            maybeInPattern +
            ' in pattern ' +
            this.pattern_.toString() +
            ' but at ellipsis level ' +
            ellipsisLevel +
            ' in template ' +
            this.template_.toString());
      }
    }
  } else subtransformer.forEachSubtransformer(
      this.templateFn_,
      subtransformer instanceof r5js.EllipsisTransformer ?
      ellipsisLevel + 1 :
      ellipsisLevel,
      this);
};


/** @return {!Object.<string, number>} */
r5js.Transformer.prototype.getPatternIds = function() {
  return this.patternIds_;
};


/** @return {!Object.<string, boolean>} */
r5js.Transformer.prototype.getTemplateRenameCandidates = function() {
  return this.templateRenameCandidates_;
};
