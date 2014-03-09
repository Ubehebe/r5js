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
 * @constructor
 */
r5js.Transformer = function(pattern, template) {
  /** @const @private {!r5js.ListLikeTransformer} */
  this.pattern_ = pattern;

  /** @const @private {!r5js.ListLikeTransformer} */
  this.template_ = template;

  /** @const {string} */
  this.name = pattern.getName();
  this.setupIds_();
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
  return this.name;
};


/** @return {!r5js.ListLikeTransformer} */
r5js.Transformer.prototype.getTemplate = function() {
  return this.template_;
};


/**
 * @private
 * TODO bl this procedure is too long.
 * @suppress {checkTypes} for the calls to forEachSubtransformer.
 */
r5js.Transformer.prototype.setupIds_ = function() {
  var patternIds = {}; // names to nesting levels
  var templateRenameCandidates = {};
  var pattern = this.pattern_;
  var template = this.template_;
  var macroName = this.name;

  var patternFn = function(subtrans, ellipsisLevel) {
    if (subtrans instanceof r5js.IdOrLiteralTransformer) {
      if (subtrans.datum.isIdentifier() &&
          subtrans.datum.getPayload() !== macroName) {
        patternIds[subtrans.datum.getPayload()] = ellipsisLevel;
      }
    } else subtrans.forEachSubtransformer(
        patternFn,
        subtrans instanceof r5js.EllipsisTransformer ?
            ellipsisLevel + 1 :
            ellipsisLevel);
  };

  var templateFn = function(subtrans, ellipsisLevel) {
    if (subtrans instanceof r5js.IdOrLiteralTransformer) {
      if (subtrans.datum.isIdentifier()) {
        var name = subtrans.datum.getPayload();
        var maybeInPattern = patternIds[name];
        /* An identifier in a template is a candidate for being
                 renamed during transcription if it doesn't occur in the pattern
                 and is not the name of the macro. I've also thrown in a check
                 that it's not a parser-sensititive identifier so we don't
                 accidentally break the parser, but this may be buggy.
                 The right thing to do is to remove the parser altogether.
                 See comments at the top of Parser. */
        if (maybeInPattern === undefined &&
            name !== macroName) {
          if (!isParserSensitiveId(name))
            templateRenameCandidates[name] = true;
        } else if (maybeInPattern !== ellipsisLevel &&
            name !== macroName) {
          throw new r5js.MacroError(
              macroName,
              name +
                  ' is at ellipsis level ' +
                  maybeInPattern +
                  ' in pattern ' +
                  pattern.toString() +
                  ' but at ellipsis level ' +
                  ellipsisLevel +
                  ' in template ' +
                  template.toString());
        }
      }
    } else subtrans.forEachSubtransformer(
        templateFn,
        subtrans instanceof r5js.EllipsisTransformer ?
            ellipsisLevel + 1 :
            ellipsisLevel);
  };

  pattern.forEachSubtransformer(patternFn, 0);
  template.forEachSubtransformer(templateFn, 0);

  /** @const @private {!Object.<string, boolean>} */
  this.templateRenameCandidates_ = templateRenameCandidates;

  /** @const @private {!Object.<string, number>} */
  this.patternIds_ = patternIds;
};


/** @return {!Object.<string, number>} */
r5js.Transformer.prototype.getPatternIds = function() {
  return this.patternIds_;
};


/** @return {!Object.<string, boolean>} */
r5js.Transformer.prototype.getTemplateRenameCandidates = function() {
  return this.templateRenameCandidates_;
};
