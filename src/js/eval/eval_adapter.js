goog.provide('r5js.EvalAdapter');



/**
 * An {@link r5js.Evaluator} maps input strings to Scheme values.
 * But to interact with the outside world, Scheme values need to be converted
 * to a suitable external representation. This convenience class does both
 * the evaluation and the conversion, saving clients from having to convert
 * results all the time.
 * @param {!r5js.Evaluator} evaluator Evaluator to use.
 * @param {function(!r5js.runtime.Value):T} adapter Function to use to convert
 * Scheme values to a form suitable for use in the target environment.
 * @struct
 * @constructor
 * @template T
 */
r5js.EvalAdapter = function(evaluator, adapter) {
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.adapter_ = adapter;
};


/**
 * @param {string} input
 * @return {T}
 */
r5js.EvalAdapter.prototype.evaluate = function(input) {
  return this.adapter_(this.evaluator_.evaluate(input));
};
