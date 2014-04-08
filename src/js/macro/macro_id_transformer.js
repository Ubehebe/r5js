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


goog.provide('r5js.PatternIdTransformer');
goog.provide('r5js.TemplateIdTransformer');


goog.require('r5js.ast.Identifier');



/**
 * @param {!r5js.ast.SimpleDatum} datum
 * @struct
 * @constructor
 * @private
 */
r5js.MacroIdTransformer_ = function(datum) {
  /** @const @protected */ this.datum = datum;
};


/**
 * @param {!r5js.Datum} inputDatum
 * The input datum. TODO bl: narrow type.
 * @param {!Object.<string, boolean>} literalIds Dictionary of literal ids.
 * @param {!r5js.IEnvironment} definitionEnv Definition environment.
 * @param {!r5js.IEnvironment} useEnv Use environment.
 * @param {!r5js.TemplateBindings} bindings Template bindings.
 * @return {boolean} True iff the transformer is a match (?)
 * TODO bl: what is the use of the value type in the literalIds dictionary?
 */
r5js.MacroIdTransformer_.prototype.matchInput = function(
    inputDatum, literalIds, definitionEnv, useEnv, bindings) {

  inputDatum = /** @type {!r5js.ast.SimpleDatum|!r5js.ast.CompoundDatum} */ (
      inputDatum);

  /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
     [...] P is a datum and F is equal to P in the sense of the equal?
     procedure." */
  if (!(this.datum instanceof r5js.ast.Identifier)) {
    return inputDatum.isEqual(this.datum);
  }

  /* R5RS 4.3.2: "A subform in the input matches a literal identifier
     if and only if it is an identifier and either both its occurrence
     in the macro expression and its occurrence in the macro definition
     have the same lexical binding, or the two identifiers are equal
     and both have no lexical binding." */
  if (this.datum.getPayload() in literalIds) {
    return inputDatum instanceof r5js.ast.Identifier &&
        (this.datumsAreEqualAndUnbound_(
        inputDatum, definitionEnv, useEnv) ||
        this.datumsHaveSameLexicalBinding_(
            inputDatum, definitionEnv, useEnv));
  } else {
    /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a non-literal identifier [...]".
         That is, non-literal identifiers match anything. */
    bindings.addTemplateBinding(
        /** @type {string} */ (this.datum.getPayload()), inputDatum);
    return true;
  }
};


/**
 * @param {!r5js.ast.Identifier} inputDatum
 * @param {!r5js.IEnvironment} definitionEnv
 * @param {!r5js.IEnvironment} useEnv
 * @return {boolean}
 * @private
 */
r5js.MacroIdTransformer_.prototype.datumsAreEqualAndUnbound_ = function(
    inputDatum, definitionEnv, useEnv) {
  var name = /** @type {string} */ (inputDatum.getPayload());
  return name === this.datum.getPayload() &&
      !definitionEnv.hasBindingRecursive(name) &&
      !useEnv.hasBindingRecursive(name);
};


/**
 * @param {!r5js.ast.Identifier} inputDatum
 * @param {!r5js.IEnvironment} definitionEnv
 * @param {!r5js.IEnvironment} useEnv
 * @return {boolean}
 * @private
 */
r5js.MacroIdTransformer_.prototype.datumsHaveSameLexicalBinding_ = function(
    inputDatum, definitionEnv, useEnv) {
  var name = /** @type {string} */ (inputDatum.getPayload());
  return definitionEnv.get(name) === useEnv.get(name);
};


/**
 * @param {!r5js.TemplateBindings} bindings Template bindings.
 * @return {!r5js.Datum}
 * @suppress {checkTypes} TODO bl
 */
r5js.MacroIdTransformer_.prototype.toDatum = function(bindings) {
  return bindings.resolveDatum(this.datum);
};


/** @return {!r5js.Datum} */
r5js.MacroIdTransformer_.prototype.getDatum = function() {
  return this.datum;
};



/**
 * @param {!r5js.ast.SimpleDatum} datum
 * @implements {r5js.ITransformer}
 * @extends {r5js.MacroIdTransformer_}
 * @struct
 * @constructor
 */
r5js.PatternIdTransformer = function(datum) {
  goog.base(this, datum);
};
goog.inherits(r5js.PatternIdTransformer, r5js.MacroIdTransformer_);


/** @override */
r5js.PatternIdTransformer.prototype.collectNestingLevels = function(
    ellipsisLevel, transformer) {
  if (!(this.datum instanceof r5js.ast.Identifier)) {
    return;
  }
  var name = /** @type {string} */ (this.datum.getPayload());
  if (name !== transformer.getName()) {
    transformer.setEllipsisLevel(name, ellipsisLevel);
  }
};



/**
 * @param {!r5js.ast.SimpleDatum} datum
 * @implements {r5js.ITransformer}
 * @extends {r5js.MacroIdTransformer_}
 * @struct
 * @constructor
 */
r5js.TemplateIdTransformer = function(datum) {
  goog.base(this, datum);
};
goog.inherits(r5js.TemplateIdTransformer, r5js.MacroIdTransformer_);


/** @override */
r5js.TemplateIdTransformer.prototype.collectNestingLevels = function(
    ellipsisLevel, transformer) {
  if (!(this.datum instanceof r5js.ast.Identifier)) {
    return;
  }
  var name = /** @type {string} */ (this.datum.getPayload());
  var maybeInPattern = transformer.getEllipsisLevel(name);
  /* An identifier in a template is a candidate for being
     renamed during transcription if it doesn't occur in the pattern
     and is not the name of the macro. I've also thrown in a check
     that it's not a parser-sensititive identifier so we don't
     accidentally break the parser, but this may be buggy.
     The right thing to do is to remove the parser altogether.
     See comments at the top of Parser. */
  if (maybeInPattern === -1 &&
      name !== transformer.getName()) {
    if (!isParserSensitiveId(name)) {
      transformer.setTemplateRenameCandidate(name);
    }
  } else if (maybeInPattern !== ellipsisLevel &&
      name !== transformer.getName()) {
    throw new r5js.MacroError(
        transformer.getName(),
        name +
        ' is at ellipsis level ' +
                maybeInPattern +
                ' in pattern ' +
                ' but at ellipsis level ' +
                ellipsisLevel +
                ' in template ');
  }
};
