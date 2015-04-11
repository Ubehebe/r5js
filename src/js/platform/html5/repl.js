goog.module('r5js.platform.html5.repl');

const Promise = goog.require('goog.Promise');
const array = goog.require('goog.array');
const CallbackBackedPort = goog.require('r5js.CallbackBackedPort');
const InputPort = goog.require('r5js.InputPort');
const Repl = goog.require('r5js.Repl');
const curPlatform = goog.require('r5js.curPlatform');
const Platform = goog.require('r5js.platform.html5.Platform');
const Terminal = goog.require('r5js.platform.html5.Terminal');
const replutil = goog.require('r5js.replutil');

/**
 * The main REPL method.
 * @param {?} jqConsole
 */
function repl(jqConsole) {
  const platform = /** @type {!Platform} */ (curPlatform.apply(null, array.toArray(arguments)));
  /** @type {Terminal} */ let terminal = null;
  const stdin = InputPort.NULL;
  const stdout = new CallbackBackedPort(output => terminal.print(output));
  platform.newEvaluator(stdin, stdout).then(evaluator => {
    const isLineComplete = line => Promise.resolve(replutil.isLineComplete(line));
    terminal = platform.getTerminal(jqConsole);
    new Repl(terminal, evaluator, isLineComplete).start();
  });
}

exports = repl;

goog.exportSymbol('r5js.repl.main', repl);
