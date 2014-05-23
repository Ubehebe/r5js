goog.provide('r5js.Repl');


goog.require('r5js.EvalAdapter');



/**
 * @param {!r5js.Terminal} terminal
 * @param {!r5js.Evaluator} evaluator
 * @struct
 * @constructor
 */
r5js.Repl = function(terminal, evaluator) {
  /** @const @private */ this.terminal_ = terminal;
  /** @const @private */ this.evaluator_ = evaluator;
};


/** Starts the read-eval-print loop. */
r5js.Repl.prototype.start = function() {
  this.terminal_.getNextLineOfInput().then(
      this.handleInputLine, undefined /* opt_onRejected */, this);
};


/** @param {string} inputLine */
r5js.Repl.prototype.handleInputLine = function(inputLine) {
  // TODO bl re-enable
  //    if (!this.lineComplete_(inputLine)) {
  //        this.terminal_.getNextLineOfInput().then(
  //
  //        )
  //        this.terminal_.prompt(true /* opt_continued */);
  //        return;
  //    }

  try {
    var value = r5js.EvalAdapter.toDisplayString(
        this.evaluator_.evaluate(inputLine));
    this.terminal_.print(value);
  } catch (e) {
    this.terminal_.error(e.toString());
  } finally {
    this.terminal_.getNextLineOfInput().then(
        this.handleInputLine, undefined /* opt_onRejected */, this);
  }
};
