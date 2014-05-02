goog.provide('haveJsValue');
goog.provide('haveStringValue');
goog.provide('r5js.test.matchers.setSharedEvaluator');
goog.setTestOnly('haveJsValue');
goog.setTestOnly('haveStringValue');
goog.setTestOnly('r5js.test.matchers.setSharedEvaluator');


goog.require('r5js.ToJsEvaluator');
goog.require('r5js.ToStringEvaluator');


/**
 * @param {?} value
 * @return {!tdd.matchers.Matcher}
 */
haveJsValue = function(value) {
  return new r5js.test.matchers.HasJsValue_(value);
};


/**
 * @param {string} value
 * @return {!tdd.matchers.Matcher}
 */
haveStringValue = function(value) {
  return new r5js.test.matchers.HasStringValue_(value);
};



/**
 * @param {?} expectedValue
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasJsValue_ = function(expectedValue) {
  /** @const @private */ this.expectedValue_ = expectedValue;
};


/** @private {r5js.EvaluateToExternalRepresentation.<?>} */
r5js.test.matchers.HasJsValue_.toJsEvaluator;


/** @override */
r5js.test.matchers.HasJsValue_.prototype.matches = function(input) {
  return r5js.test.matchers.HasJsValue_.sharedEvaluator_.evaluate(input) ===
      this.expectedValue_;
};


/** @override */
r5js.test.matchers.HasJsValue_.prototype.getSuccessMessage = function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasJsValue_.prototype.getFailureMessage = function(input) {
  return 'want ' +
      this.expectedValue_ +
      ' got ' +
      r5js.test.matchers.HasJsValue_.sharedEvaluator_.evaluate(input);
};



/**
 * @param {string} expectedValue
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasStringValue_ = function(expectedValue) {
  /** @const @private */ this.expectedValue_ = expectedValue;
};


/** @private {r5js.EvaluateToExternalRepresentation.<string>} */
r5js.test.matchers.HasStringValue_.sharedEvaluator_;


/** @override */
r5js.test.matchers.HasStringValue_.prototype.matches = function(input) {
  return this.expectedValue_ ===
      r5js.test.matchers.HasStringValue_.sharedEvaluator_.evaluate(input);
};


/** @override */
r5js.test.matchers.HasStringValue_.prototype.getSuccessMessage =
    function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasStringValue_.prototype.getFailureMessage =
    function(input) {
  return 'want ' +
      this.expectedValue_ +
      ' got ' +
      r5js.test.matchers.HasStringValue_.sharedEvaluator_.evaluate(input);
};


/** @param {!r5js.Evaluator} evaluator */
r5js.test.matchers.setSharedEvaluator = function(evaluator) {
  r5js.test.matchers.HasJsValue_.sharedEvaluator_ =
      new r5js.ToJsEvaluator(evaluator);
  r5js.test.matchers.HasStringValue_.sharedEvaluator_ =
      new r5js.ToStringEvaluator(evaluator);
};
