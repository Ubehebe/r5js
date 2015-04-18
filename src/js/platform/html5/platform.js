goog.module('r5js.platform.html5.Platform');

const Client = goog.require('r5js.platform.html5.Client');
const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Platform = goog.require('r5js.Platform');
const Promise = goog.require('goog.Promise');
const replutil = goog.require('r5js.replutil');
const Terminal = goog.require('r5js.platform.html5.Terminal');

/**
 * @implements {Platform}
 * @package
 */
class Html5 {
    /** @override */
    exit(statusCode) {}

    /**
     * @param {?} jqConsole
     * @return {!Terminal}
     * @package
     */
    getTerminal(jqConsole) {
        return new Terminal(
            jqConsole, function(line) {
                return Promise.resolve(replutil.isLineComplete(line));
            });
    }

    /**
     * @param {!InputPort=} opt_inputPort
     * @param {!OutputPort=} opt_outputPort
     * @return {!Promise<!Evaluator>}
     * @override TODO bl compiler bug?
     */
    newEvaluator(opt_inputPort, opt_outputPort) {
        /** @type {!Evaluator} */ const evaluator = new Client(opt_outputPort || OutputPort.NULL);
        return Promise.resolve(evaluator);
    }
}

exports = Html5;

