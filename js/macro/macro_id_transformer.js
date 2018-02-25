goog.module('r5js.MacroIdTransformer');

const {CompoundDatum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/compound_datum');
const {Datum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {SimpleDatum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');
const Subtransformer = goog.require('r5js.Subtransformer');
const TemplateBindings = goog.require('r5js.TemplateBindings');

class MacroIdTransformer {
    /** @param {!SimpleDatum} datum */
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
     * @return {!Subtransformer}
     */
    static pattern(datum) {
        return new PatternIdTransformer(datum);
    }

    /**
     * @param {!SimpleDatum} datum
     * @return {!Subtransformer}
     */
    static template(datum) {
        return new TemplateIdTransformer(datum);
    }
}

/** @implements {Subtransformer} */
class PatternIdTransformer extends MacroIdTransformer {
    /** @param {!SimpleDatum} datum */
    constructor(datum) {
        super(datum);
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, renameHelper) {
        if (this.datum instanceof Identifier) {
            renameHelper.recordPatternId(this.datum.getPayload(), ellipsisLevel);
        }
    }
}

/** @implements {Subtransformer} */
class TemplateIdTransformer extends MacroIdTransformer {
    /** @param {!SimpleDatum} datum */
    constructor(datum) {
        super(datum);
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, renameHelper) {
        if (this.datum instanceof Identifier) {
            renameHelper.recordTemplateId(this.datum.getPayload(), ellipsisLevel);
        }
    }
}

exports = MacroIdTransformer;
