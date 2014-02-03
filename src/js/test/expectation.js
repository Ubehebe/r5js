goog.provide('Expect');
goog.setTestOnly('Expect');


goog.require('expect');
goog.require('r5js.test.matchers.ParsesAs');
goog.require('tdd.Expectation');


/**
 * @param {*} actualValue
 * @return {!r5js.Expectation}
 */
var Expect = function(actualValue) {
    return new r5js.Expectation(actualValue);
};


/**
 * @param {*} actualValue
 * @extends {tdd.Expectation}
 * @struct
 * @constructor
 */
r5js.Expectation = function(actualValue) {
  goog.base(this, actualValue);
};
goog.inherits(r5js.Expectation, tdd.Expectation);


/**
 * @return {!r5js.Expectation}
 * @override
 */
r5js.Expectation.prototype.not = function() {
    return /** @type {!r5js.Expectation} */ (goog.base(this, 'not'));
};


/** @suppress {accessControls} */
r5js.Expectation.prototype.toParseAs = function(type) {
    this.setResult_(new r5js.test.matchers.ParsesAs(
        type, /** @type {string} */(this.actualValue_)));
};