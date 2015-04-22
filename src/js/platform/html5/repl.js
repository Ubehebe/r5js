goog.module('r5js.platform.html5.repl');

const array = goog.require('goog.array');
const CallbackBackedPort = goog.require('r5js.CallbackBackedPort');
const curPlatform = goog.require('r5js.curPlatform');
const InputPort = goog.require('r5js.InputPort');
const Platform = goog.require('r5js.platform.html5.Platform');
const Promise = goog.require('goog.Promise');
const Repl = goog.require('r5js.Repl');
const replutil = goog.require('r5js.replutil');
const Terminal = goog.require('r5js.platform.html5.Terminal');

/**
 * The main REPL method.
 * @param {?} jqConsole
 */
function repl(jqConsole) {
    const platform = /** @type {!Platform} */ (curPlatform.apply(null, array.toArray(arguments)));
    /** @type {Terminal} */ let terminal = null;
    const stdin = InputPort.NULL;
    const stdout = new CallbackBackedPort(output => terminal.print(output));
    const evaluator = platform.newEvaluator(stdin, stdout);
    terminal = platform.getTerminal(jqConsole);
    new Repl(terminal, evaluator).start();
}

exports = repl;

goog.exportSymbol('r5js.repl.main', repl);
