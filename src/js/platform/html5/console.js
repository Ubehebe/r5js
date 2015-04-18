goog.module('r5js.platform.html5.console');

const curPlatform = goog.require('r5js.curPlatform');
const Promise = goog.require('goog.Promise');
let evaluator = null;

/**
 * Convenience function for evaluating Scheme programs from the browser console.
 * TODO bl: convert this to use a synchronous evaluator that directly returns the value.
 * With promises, this function spams the console with its useless return value (undefined).
 * @param {string} input
 */
function EVAL(input) {
    if (evaluator) {
        evaluator.evaluate(input).then(output => console.log(output));
    } else {
        curPlatform().newEvaluator()
            .then(e => evaluator = e)
            .then(() => EVAL(input));
    }
}

goog.exportSymbol('EVAL', EVAL);