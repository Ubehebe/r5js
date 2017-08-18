goog.module('r5js.platform.html5.Platform');

const Client = goog.require('r5js.platform.html5.Client');
const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Platform = goog.require('r5js.Platform');
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
        return new Terminal(jqConsole);
    }

    /**
     * @param {!InputPort=} opt_inputPort
     * @param {!OutputPort=} opt_outputPort
     * @return {!Evaluator}
     * @override TODO bl compiler bug?
     */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return new Client(opt_outputPort || OutputPort.NULL);
    }
}

exports = Html5;

