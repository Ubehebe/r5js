goog.provide('r5js.curPlatform');

goog.require('r5js.Platform');
goog.require('r5js.platform.common.newEvaluator');
goog.require('r5js.platform.node.Terminal');

/**
 * NodeJS-specific environment facilities.
 *
 * TODO bl: The main benefit of running the interpreter in Node
 * over a browser is filesystem access: open-input-file and open-output-file
 * should be connected to the local filesystem.
 *
 * However, the current implementation merely uses in-memory ports.
 * The reason is that the R5RS I/O facilities are underspecified to such
 * an extent as to be of little use to the programmer. (For example,
 * it is unspecified whether calling open-output-file on an existing file
 * truncates or appends, and the effect of concurrent modifications
 * to a file isn't even discussed.)
 *
 * Proper filesystem access through Node will be added for R6RS.
 */
r5js.platform.Node_ = /** @private @implements {r5js.Platform} */ class {
    /** @override */
    exit(statusCode) {
        process.exit(statusCode);
    }

    /**
     * @return {!r5js.Terminal}
     * @package
     */
    getTerminal() {
        return new r5js.platform.node.Terminal();
    }

    /**
     * @param {!r5js.InputPort=} opt_inputPort
     * @param {!r5js.OutputPort=} opt_outputPort
     * @return {!goog.Promise<!r5js.Evaluator>}
     * @override TODO bl compiler bug?
     */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return r5js.platform.common.newEvaluator(opt_inputPort, opt_outputPort);
    }
};


/** @return {!r5js.Platform} */
r5js.curPlatform = function() {
  return new r5js.platform.Node_();
};



