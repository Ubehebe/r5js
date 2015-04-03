goog.provide('r5js.PatternIdTransformer');
goog.provide('r5js.TemplateIdTransformer');

goog.require('r5js.ITransformer');
goog.require('r5js.ast.Identifier');
goog.require('r5js.error');

r5js.MacroIdTransformer_ = /** @private */ class {
    /** @param {!r5js.ast.SimpleDatum} datum */
    constructor(datum) {
        /** @const @protected */ this.datum = datum;
    }

    /**
     * @param {!r5js.Datum} inputDatum
     * The input datum. TODO bl: narrow type.
     * @param {!Object<string, boolean>} literalIds Dictionary of literal ids.
     * @param {!r5js.IEnvironment} definitionEnv Definition environment.
     * @param {!r5js.IEnvironment} useEnv Use environment.
     * @param {!r5js.TemplateBindings} bindings Template bindings.
     * @return {boolean} True iff the transformer is a match (?)
     * TODO bl: what is the use of the value type in the literalIds dictionary?
     */
    matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {

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
    }

    /**
     * @param {!r5js.ast.Identifier} inputDatum
     * @param {!r5js.IEnvironment} definitionEnv
     * @param {!r5js.IEnvironment} useEnv
     * @return {boolean}
     * @private
     */
    datumsAreEqualAndUnbound_(inputDatum, definitionEnv, useEnv) {
        var name = /** @type {string} */ (inputDatum.getPayload());
        return name === this.datum.getPayload()
            && !definitionEnv.hasBindingRecursive(name)
            && !useEnv.hasBindingRecursive(name);
    }

    /**
     * @param {!r5js.ast.Identifier} inputDatum
     * @param {!r5js.IEnvironment} definitionEnv
     * @param {!r5js.IEnvironment} useEnv
     * @return {boolean}
     * @private
     */
    datumsHaveSameLexicalBinding_(inputDatum, definitionEnv, useEnv) {
        var name = /** @type {string} */ (inputDatum.getPayload());
        return definitionEnv.get(name) === useEnv.get(name);
    }

    /**
     * @param {!r5js.TemplateBindings} bindings Template bindings.
     * @return {!r5js.Datum}
     * @suppress {checkTypes} TODO bl
     */
    toDatum(bindings) {
        return bindings.resolveDatum(this.datum);
    }

    /** @return {!r5js.Datum} */
    getDatum() {
        return this.datum;
    }
};

r5js.PatternIdTransformer = /** @implements {r5js.ITransformer} */ class extends r5js.MacroIdTransformer_ {
    /** @param {!r5js.ast.SimpleDatum} datum */
    constructor(datum) {
        super(datum);
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, transformer) {
        if (!(this.datum instanceof r5js.ast.Identifier)) {
            return;
        }
        var name = /** @type {string} */ (this.datum.getPayload());
        if (name !== transformer.getName()) {
            transformer.setEllipsisLevel(name, ellipsisLevel);
        }
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
  r5js.TemplateIdTransformer.base(this, 'constructor', datum);
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
    throw r5js.error.macro(
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
