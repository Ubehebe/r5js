goog.module('r5js.MacroIdTransformer');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const IEnvironment = goog.require('r5js.IEnvironment');
const ITransformer = goog.require('r5js.ITransformer');
const Identifier = goog.require('r5js.ast.Identifier');
const RenameUtil = goog.require('r5js.RenameUtil');
const SimpleDatum = goog.require('r5js.ast.SimpleDatum');
const TemplateBindings = goog.require('r5js.TemplateBindings');

class MacroIdTransformer {
    /**
     * @param {!SimpleDatum} datum
     * @private No reason to construct outside of this file.
     */
    constructor(datum) {
        /** @const @protected */ this.datum = datum;
    }

    /**
     * @param {!Datum} inputDatum
     * The input datum. TODO bl: narrow type.
     * @param {!Object<string, boolean>} literalIds Dictionary of literal ids.
     * @param {!IEnvironment} definitionEnv Definition environment.
     * @param {!IEnvironment} useEnv Use environment.
     * @param {!TemplateBindings} bindings Template bindings.
     * @return {boolean} True iff the transformer is a match (?)
     * TODO bl: what is the use of the value type in the literalIds dictionary?
     */
    matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {

        inputDatum = /** @type {!SimpleDatum|!CompoundDatum} */ (inputDatum);

        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a datum and F is equal to P in the sense of the equal?
         procedure." */
        if (!(this.datum instanceof Identifier)) {
            return inputDatum.isEqual(this.datum);
        }

        /* R5RS 4.3.2: "A subform in the input matches a literal identifier
         if and only if it is an identifier and either both its occurrence
         in the macro expression and its occurrence in the macro definition
         have the same lexical binding, or the two identifiers are equal
         and both have no lexical binding." */
        if (this.datum.getPayload() in literalIds) {
            return inputDatum instanceof Identifier
                && (this.datumsAreEqualAndUnbound_(inputDatum, definitionEnv, useEnv)
                || this.datumsHaveSameLexicalBinding_(inputDatum, definitionEnv, useEnv));
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
     * @param {!Identifier} inputDatum
     * @param {!IEnvironment} definitionEnv
     * @param {!IEnvironment} useEnv
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
     * @param {!Identifier} inputDatum
     * @param {!IEnvironment} definitionEnv
     * @param {!IEnvironment} useEnv
     * @return {boolean}
     * @private
     */
    datumsHaveSameLexicalBinding_(inputDatum, definitionEnv, useEnv) {
        var name = /** @type {string} */ (inputDatum.getPayload());
        return definitionEnv.get(name) === useEnv.get(name);
    }

    /**
     * @param {!TemplateBindings} bindings Template bindings.
     * @return {!Datum}
     * @suppress {checkTypes} TODO bl
     */
    toDatum(bindings) {
        return bindings.resolveDatum(this.datum);
    }

    /** @return {!Datum} */
    getDatum() {
        return this.datum;
    }

    /**
     * @param {!SimpleDatum} datum
     * @return {!ITransformer}
     */
    static pattern(datum) {
        return new PatternIdTransformer(datum);
    }

    /**
     * @param {!SimpleDatum} datum
     * @return {!ITransformer}
     */
    static template(datum) {
        return new TemplateIdTransformer(datum);
    }
}

/** @implements {ITransformer} */
class PatternIdTransformer extends MacroIdTransformer {
    /** @param {!SimpleDatum} datum */
    constructor(datum) {
        super(datum);
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, transformer) {
        if (!(this.datum instanceof Identifier)) {
            return;
        }
        var name = /** @type {string} */ (this.datum.getPayload());
        if (name !== transformer.getName()) {
            transformer.setEllipsisLevel(name, ellipsisLevel);
        }
    }
}

/** @implements {ITransformer} */
class TemplateIdTransformer extends MacroIdTransformer {
    /** @param {!SimpleDatum} datum */
    constructor(datum) {
        super(datum);
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, transformer) {
        if (!(this.datum instanceof Identifier)) {
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
        if (maybeInPattern === -1
            && name !== transformer.getName()) {
            if (!RenameUtil.isParserSensitiveId(name)) {
                transformer.setTemplateRenameCandidate(name);
            }
        } else if (maybeInPattern !== ellipsisLevel
            && name !== transformer.getName()) {
            throw Error.macro(
                transformer.getName(),
                name +
                ' is at ellipsis level ' +
                maybeInPattern +
                ' in pattern ' +
                ' but at ellipsis level ' +
                ellipsisLevel +
                ' in template ');
        }
    }
}

exports = MacroIdTransformer;
