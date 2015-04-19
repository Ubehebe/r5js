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
    return evaluator
        ? evaluator.evaluate(input)
        : curPlatform().newEvaluator()
        .then(e => evaluator = e)
        .then(() => EVAL(input));
}

goog.exportSymbol('EVAL', EVAL);