goog.module('r5js.platform.Node');

const Evaluator = goog.require('r5js.Evaluator');
const InputPort = goog.require('r5js.InputPort');
const newCommonEvaluator = goog.require('r5js.platform.common.newEvaluator');
const OutputPort = goog.require('r5js.OutputPort');
const Platform = goog.require('r5js.Platform');
const Promise = goog.require('goog.Promise');
const Terminal = goog.require('r5js.platform.node.Terminal');

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
 * @implements {Platform}
 */
class Node {
    /** @override */
    exit(statusCode) {
        process.exit(statusCode);
    }

    /**
     * @return {!Terminal}
     * @package
     */
    getTerminal() {
        return new Terminal();
    }

    /**
     * @param {!InputPort=} opt_inputPort
     * @param {!OutputPort=} opt_outputPort
     * @return {!Promise<!Evaluator>}
     * @override TODO bl compiler bug?
     */
    newEvaluator(opt_inputPort, opt_outputPort) {
        return newCommonEvaluator(opt_inputPort, opt_outputPort);
    }
}

exports = Node;


