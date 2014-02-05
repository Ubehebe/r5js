goog.provide('r5js.test.Interpreter');
goog.setTestOnly('r5js.test.Interpreter');


goog.require('expect');



/**
 * @param {!r5js.PublicApi} publicApi
 * @implements {tdd.TestSuite}
 * @struct
 * @constructor
 */
r5js.test.Interpreter = function(publicApi) {
    /** @const @private {!r5js.PublicApi} */
    this.publicApi_ = publicApi;

    /** @const @private {!r5js.util.Logger} */
    this.logger_ = r5js.util.Logger.getLogger('r5js.test.Interpreter');
};


/** @override */
r5js.test.Interpreter.prototype.getType = function() {
    return tdd.TestType.UNIT;
};


/** @override */
r5js.test.Interpreter.prototype.toString = function() {
    return 'r5js.test.Interpreter';
};


r5js.test.Interpreter.prototype['testTrivial'] = function() {
    expect(this.publicApi_.Eval(
        "(+ 1 1)",
        goog.nullFunction /* sideEffectHandler */,
        this.logger_)).toBe("2");
};


r5js.test.Interpreter.prototype['testTrivial2'] = function() {
    expect(this.publicApi_.Eval(
        "(+ 2 2)",
        goog.nullFunction /* sideEffectHandler */,
        this.logger_)).toBe("4");
};