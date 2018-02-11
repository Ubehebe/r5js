goog.module('r5js.Transformer');

const Datum = goog.require('r5js.Datum');
const ListLikeTransformer = goog.require('r5js.ListLikeTransformer');
const RenameHelper = goog.require('r5js.macro.RenameHelper');
const TemplateBindings = goog.require('r5js.TemplateBindings');
const {IEnvironment} = goog.require('r5js.IEnvironment');

class Transformer {
    /**
     * @param {!ListLikeTransformer} pattern
     * @param {!ListLikeTransformer} template
     */
    constructor(pattern, template) {
        /** @const @private */ this.pattern_ = pattern;
        /** @const @private */ this.template_ = template;
        /** @const @private {string} */ this.name_ = pattern.getName();

        const renameHelper = new RenameHelper(this.name_);

        this.pattern_.collectNestingLevels(0, renameHelper);
        this.template_.collectNestingLevels(0, renameHelper);

        /** @const @private {!Object<string, number>} */
        this.patternIds_ = renameHelper.getPatternIds();

        /** @const @private {!Object<string, boolean>} */
        this.templateRenameCandidates_ = renameHelper.getRenameCandidates();
    }

    /**
     * @param {!Datum} inputDatum The input datum.
     * @param {!Object<string, boolean>} literalIds Dictionary of literal ids.
     * @param {!IEnvironment} definitionEnv Definition environment.
     * @param {!IEnvironment} useEnv Use environment.
     * @param {!TemplateBindings} bindings Template bindings.
     * @return {boolean} True iff the transformer is a match (?).
     */
    matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
        return this.pattern_.matchInput(
            inputDatum, literalIds, definitionEnv, useEnv, bindings);
    }

    /** @return {string} The name of this transformer. */
    getName() {
        return this.name_;
    }

    /** @return {!ListLikeTransformer} */
    getTemplate() {
        return this.template_;
    }


    /** @return {!Object<string, number>} */
    getPatternIds() {
        return this.patternIds_;
    }

    /** @return {!Object<string, boolean>} */
    getTemplateRenameCandidates() {
        return this.templateRenameCandidates_;
    }
}

exports = Transformer;