goog.provide('haveJsOutput');
goog.provide('haveJsValue');
goog.provide('haveStringOutput');
goog.provide('haveStringValue');
goog.provide('r5js.test.matchers.setSharedEvaluator');
goog.setTestOnly('haveJsOutput');
goog.setTestOnly('haveJsValue');
goog.setTestOnly('haveStringValue');
goog.setTestOnly('r5js.test.matchers.setSharedEvaluator');


goog.require('goog.array');
goog.require('r5js.OutputSavingPort');
goog.require('r5js.ToJsEvaluator');
goog.require('r5js.ToStringEvaluator');
goog.require('r5js.datumutil');


/**
 * @param {?} value
 * @return {!tdd.matchers.Matcher}
 */
haveJsValue = function(value) {
  return new r5js.test.matchers.HasJsValue_(
      value, /** @type {!r5js.EvaluateToExternalRepresentation.<?>} */ (
          r5js.test.matchers.HasJsValue_.sharedEvaluator_));
};


/**
 * @param {string} value
 * @return {!tdd.matchers.Matcher}
 */
haveStringValue = function(value) {
  return new r5js.test.matchers.HasStringValue_(
      value, /** @type {!r5js.EvaluateToExternalRepresentation.<string>} */ (
          r5js.test.matchers.HasStringValue_.sharedEvaluator_));
};


/**
 * @param {?} output
 * @return {!tdd.matchers.Matcher}
 */
haveJsOutput = function(output) {
  return new r5js.test.matchers.HasJsOutput_(output,
      /** @type {!r5js.Evaluator} */(
      r5js.test.matchers.HasJsOutput_.sharedEvaluator_),
      r5js.test.matchers.HasJsOutput_.sharedOutputPort_);
};


/**
 * @param {string} output
 * @return {!tdd.matchers.Matcher}
 */
haveStringOutput = function(output) {
  return new r5js.test.matchers.HasStringOutput_(output,
      /** @type {!r5js.Evaluator} */ (
      r5js.test.matchers.HasStringOutput_.sharedEvaluator_),
      r5js.test.matchers.HasStringOutput_.sharedOutputPort_);
};



/**
 * @param {?} expectedValue
 * @param {!r5js.EvaluateToExternalRepresentation.<?>} evaluator
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasJsValue_ = function(expectedValue, evaluator) {
  /** @const @private */ this.expectedValue_ = expectedValue;
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @private {r5js.EvaluateToExternalRepresentation.<?>} */
r5js.test.matchers.HasJsValue_.sharedEvaluator_;


/** @override */
r5js.test.matchers.HasJsValue_.prototype.matches = function(input) {
  return r5js.test.matchers.HasJsValue_.equals(
      this.expectedValue_, this.evaluator_.evaluate(input));
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
      this.evaluator_.evaluate(input);
};


/**
 * @param {?} x
 * @param {?} y
 * @return {boolean}
 */
r5js.test.matchers.HasJsValue_.equals = function(x, y) {
  var xIsArray = x instanceof Array;
  var yIsArray = y instanceof Array;
  if (xIsArray && yIsArray) {
    return x.length === y.length &&
        goog.array.zip(x, y).every(function(pair) {
          return r5js.test.matchers.HasJsValue_.equals(pair[0], pair[1]);
        });
  } else if (!(xIsArray || yIsArray)) {
    return x === y;
  } else {
    return false;
  }
};



/**
 * @param {string} expectedValue
 * @param {!r5js.EvaluateToExternalRepresentation.<string>} evaluator
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasStringValue_ = function(expectedValue, evaluator) {
  /** @const @private */ this.expectedValue_ = expectedValue;
  /** @const @private */ this.evaluator_ = evaluator;
};


/** @private {r5js.EvaluateToExternalRepresentation.<string>} */
r5js.test.matchers.HasStringValue_.sharedEvaluator_;


/** @override */
r5js.test.matchers.HasStringValue_.prototype.matches = function(input) {
  return this.expectedValue_ === this.evaluator_.evaluate(input);
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
      this.evaluator_.evaluate(input);
};



/**
 * @param {?} expectedOutput
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasJsOutput_ = function(
    expectedOutput, evaluator, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @private {r5js.Evaluator} */
r5js.test.matchers.HasJsOutput_.sharedEvaluator_;


/**
 * @private {!r5js.OutputSavingPort.<boolean|number|string|!Array|undefined>}
 * @const
 */
r5js.test.matchers.HasJsOutput_.sharedOutputPort_ = new r5js.OutputSavingPort(
    r5js.ToJsEvaluator.schemeToJsValue);


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.matches = function(input) {
  this.evaluator_.evaluate(input);
  var actualOutput = this.outputPort_.getAndClearOutput()[0];
  return r5js.test.matchers.HasJsValue_.equals(
      actualOutput, this.expectedOutput_);
};


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.getSuccessMessage = function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasJsOutput_.prototype.getFailureMessage = function(input) {
  return 'want ' +
      this.expectedOutput_ +
      ' got ';
};



/**
 * @param {string} expectedOutput
 * @param {!r5js.Evaluator} evaluator
 * @param {!r5js.OutputSavingPort} outputPort
 * @implements {tdd.matchers.Matcher}
 * @struct
 * @constructor
 * @private
 */
r5js.test.matchers.HasStringOutput_ = function(
    expectedOutput, evaluator, outputPort) {
  /** @const @private */ this.expectedOutput_ = expectedOutput;
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.outputPort_ = outputPort;
};


/** @private {r5js.Evaluator} */
r5js.test.matchers.HasStringOutput_.sharedEvaluator_;


/** @const @private {!r5js.OutputSavingPort.<string>} */
r5js.test.matchers.HasStringOutput_.sharedOutputPort_ =
    new r5js.OutputSavingPort(
        r5js.ToStringEvaluator.schemeValueToWriteString,
        r5js.ToStringEvaluator.schemeValueToDisplayString);


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.matches = function(input) {
  this.evaluator_.evaluate(input);
  var actualOutput = this.outputPort_.getAndClearOutput()[0];
  return actualOutput === this.expectedOutput_;
};


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.getSuccessMessage =
    function(input) {
  return 'ok';
};


/** @override */
r5js.test.matchers.HasStringOutput_.prototype.getFailureMessage =
    function(input) {
  return 'want ' +
      this.expectedOutput_ +
      ' got ';
};


/** @param {!r5js.Evaluator} evaluator */
r5js.test.matchers.setSharedEvaluator = function(evaluator) {
  r5js.test.matchers.HasJsValue_.sharedEvaluator_ =
      new r5js.ToJsEvaluator(evaluator);
  r5js.test.matchers.HasStringValue_.sharedEvaluator_ =
      new r5js.ToStringEvaluator(evaluator);
  r5js.test.matchers.HasJsOutput_.sharedEvaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL, r5js.test.matchers.HasJsOutput_.sharedOutputPort_);
  r5js.test.matchers.HasStringOutput_.sharedEvaluator_ = evaluator.withPorts(
      r5js.InputPort.NULL,
      r5js.test.matchers.HasStringOutput_.sharedOutputPort_);
};
