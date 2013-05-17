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


goog.provide('r5js.tmp.transformer');


goog.require('r5js.EllipsisTransformer');
goog.require('r5js.IdOrLiteralTransformer');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ListLikeTransformer');
goog.require('r5js.MacroError');

/**
 * @constructor
 */
function Transformer(pattern, template) {
    /* This is an InternalInterpreterError (= sanity check) instead of a
     MacroError because the grammar should make it impossible for
     a programmer to get here. */
    if (!(pattern instanceof r5js.ListLikeTransformer))
        throw new r5js.InternalInterpreterError(
            'transformer begins with a pattern '
                + pattern.toString()
                + ' that is not a ListLikeTransformer');
    this.pattern = pattern;
    this.template = template;
    this.name = pattern.subtransformers[0].datum.payload;
    this.setupIds();
}

Transformer.prototype.matchInput = function(inputDatum, literalIdentifiers, definitionEnv, useEnv, bindings) {
    return this.pattern.matchInput(inputDatum, literalIdentifiers, definitionEnv, useEnv, bindings);
};

Transformer.prototype.getName = function() {
    return this.name;
};

Transformer.prototype.setupIds = function() {
    var patternIds = {}; // names to nesting levels
    var templateRenameCandidates = {};
    var pattern = this.pattern;
    var template = this.template;
    var macroName = this.name;

    var patternFn = function(subtrans, ellipsisLevel) {
        if (subtrans instanceof r5js.IdOrLiteralTransformer) {
            if (subtrans.datum.isIdentifier() && subtrans.datum.payload !== macroName) {
                patternIds[subtrans.datum.payload] = ellipsisLevel;
            }
        } else subtrans.forEachSubtransformer(
            patternFn,
            subtrans instanceof r5js.EllipsisTransformer ? ellipsisLevel + 1 : ellipsisLevel);
    };

    var templateFn = function(subtrans, ellipsisLevel) {
        if (subtrans instanceof r5js.IdOrLiteralTransformer) {
            if (subtrans.datum.isIdentifier()) {
                var name = subtrans.datum.payload;
                var maybeInPattern = patternIds[name];
                /* An identifier in a template is a candidate for being
                 renamed during transcription if it doesn't occur in the pattern
                 and is not the name of the macro. I've also thrown in a check
                 that it's not a parser-sensititive identifier so we don't
                 accidentally break the parser, but this may be buggy.
                 The right thing to do is to remove the parser altogether.
                 See comments at the top of Parser. */
                if (maybeInPattern === undefined
                    && name !== macroName) {
                    if (!isParserSensitiveId(name))
                        templateRenameCandidates[name] = true;
                } else if (maybeInPattern !== ellipsisLevel
                    && name !== macroName) {
                    throw new r5js.MacroError(
                        macroName,
                        name
                            + ' is at ellipsis level '
                            + maybeInPattern
                            + ' in pattern '
                            + pattern.toString()
                            + ' but at ellipsis level '
                            + ellipsisLevel
                            + ' in template '
                            + template.toString());
                }
            }
        } else subtrans.forEachSubtransformer(
            templateFn,
            subtrans instanceof r5js.EllipsisTransformer ? ellipsisLevel + 1 : ellipsisLevel);
    };

    pattern.forEachSubtransformer(patternFn, 0);
    template.forEachSubtransformer(templateFn, 0);

    this.templateRenameCandidates = templateRenameCandidates;
    this.patternIds = patternIds;

    return this;
};

Transformer.prototype.getPatternIds = function() {
    return this.patternIds;
};

Transformer.prototype.getTemplateRenameCandidates = function() {
    return this.templateRenameCandidates;
};