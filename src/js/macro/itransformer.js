goog.module('r5js.ITransformer');

const Datum = goog.require('r5js.Datum');
const IEnvironment = goog.require('r5js.IEnvironment');
const ObjectValue = goog.require('r5js.runtime.ObjectValue');
const TemplateBindings = goog.require('r5js.TemplateBindings');
const Transformer = goog.require('r5js.Transformer');

/** @interface @extends {ObjectValue} TODO bl inappropriate */
class ITransformer {
 /**
  * @param {number} ellipsisLevel Nesting level of ellipses, passed as the last
  * argument to the callback.
  * @param {!Transformer} transformer
  */
 collectNestingLevels(ellipsisLevel, transformer) {}

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
  * @return {!Datum}
  */
 toDatum(bindings) {}
}

exports = ITransformer;
