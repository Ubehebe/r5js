goog.module('r5js.Subtransformer');

const Datum = goog.require('r5js.Datum');
const RenameHelper = goog.require('r5js.macro.RenameHelper');
const TemplateBindings = goog.require('r5js.TemplateBindings');

/** @interface @extends {ObjectValue} TODO bl inappropriate */
class Subtransformer {
 /**
  * @param {number} ellipsisLevel Nesting level of ellipses, passed as the last
  * argument to the callback.
  * @param {!RenameHelper} renameHelper Rename helper.
  */
 collectNestingLevels(ellipsisLevel, renameHelper) {}

 /**
  * @param {!Datum} inputDatum The input datum.
  * @param {!Object<string, boolean>} literalIds Dictionary of literal ids.
  * @param {!IEnvironment} definitionEnv Definition environment.
  * @param {!IEnvironment} useEnv Use environment.
  * @param {!TemplateBindings} bindings Template bindings.
  * @return {boolean} True iff the transformer is a match (?)
  * TODO bl: what is the use of the value type in the literalIds dictionary?
  */
 matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {}

 /**
  * @param {!TemplateBindings} bindings Template bindings.
  * @return {?Datum} null if something failed. TODO: if what failed?
  */
 toDatum(bindings) {}
}

exports = Subtransformer;
