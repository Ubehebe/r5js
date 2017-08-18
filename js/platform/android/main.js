goog.module('r5js.platform.android.main');

const CallbackBackedPort = goog.require('r5js.CallbackBackedPort');
const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const curPlatform = goog.require('r5js.curPlatform');


/** @private {OutputPort} */ let outputPort = null;
/** @private {Evaluator} */ let evaluator = null;

/**
 * The main method for the Android port.
 * @param {string} input
 */
function main(input) {
    if (!evaluator) {
        outputPort = new CallbackBackedPort(function (s) {
            AndroidSchemePlatform.print(s);
        });
        evaluator = curPlatform().newEvaluator(InputPort.NULL, outputPort);
    }
    return evaluator.evaluate(input).then(function (result) {
        AndroidSchemePlatform.returnValue(result);
    });
}

goog.exportSymbol('EVAL', main);
exports = main;