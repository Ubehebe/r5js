goog.module('r5js.platform.html5.console');

const curPlatform = goog.require('r5js.curPlatform');
const Promise = goog.require('goog.Promise');
let evaluator = null;

/**
 * Convenience function for evaluating Scheme programs from the browser console.
 * @param {string} input
 * @return {!Promise<string>}
 */
function EVAL(input) {
    if (!evaluator) {
        evaluator = curPlatform().newEvaluator();
    }
    return evaluator.evaluate(input);
}

goog.exportSymbol('EVAL', EVAL);