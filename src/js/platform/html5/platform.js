goog.module('r5js.platform.html5.Platform');

const Promise = goog.require('goog.Promise');
const OutputPort = goog.require('r5js.OutputPort');
const Platform = goog.require('r5js.Platform');
// TODO bl not needed by this file. Workaround for circular dep elsewhere.
const PrimitiveProcedures = goog.require('r5js.PrimitiveProcedures');
const Client = goog.require('r5js.platform.html5.Client');
const Terminal = goog.require('r5js.platform.html5.Terminal');
const replutil = goog.require('r5js.replutil');

/**
 * @implements {Platform}
 * @package
 */
class Html5 {
    /** @override */
    exit() {
    }

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

    /** @override */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return Promise.resolve(new Client(opt_outputPort || OutputPort.NULL));
    }
}

exports = Html5;

