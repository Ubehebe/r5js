goog.provide('r5js.Repl');


goog.require('r5js.EvalAdapter');
goog.require('r5js.EvaluatorImpl');



/**
 * @param {!r5js.Terminal} terminal
 * @param {!r5js.Pipeline} pipeline
 * @param {!r5js.InputPort} inputPort
 * @param {!r5js.OutputPort} outputPort
 * @struct
 * @constructor
 */
r5js.Repl = function(terminal, pipeline, inputPort, outputPort) {
  /** @const @private */ this.terminal_ = terminal;
  /** @const @private */ this.pipeline_ = pipeline;
  this.evaluator_ = new r5js.EvaluatorImpl(pipeline, inputPort, outputPort);
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


/**
 * @param {string} inputLine Input line
 * @return {boolean}
 * @private
 */
r5js.Repl.prototype.lineComplete_ = function(inputLine) {
  try {
    this.pipeline_.parse(/** @type {!r5js.Datum} */ (
        this.pipeline_.read(
        this.pipeline_.scan(inputLine))));
    return true;
  } catch (x) {
    /* If parsing failed, we usually want to wait for another line
         of input. There's one common exception: unquoted empty lists
         () and nested versions of the same. If a programmer types ()
         at the terminal and presses enter, she will be stuck forever:
         nothing she later types in will make the line buffer parse, and
         so the terminal will never send the line buffer off for
         evaluation. As a heuristic, if the parse has not succeeded,
         we return false unless the number of opening and closing parens
         is the same. This might not be the right heuristic,
         but I haven't found a counterexample yet. Note that it's
         fine to type unquoted empty lists as their own lines as long
         as they are not the first line: for example the following is
         fine:

         >> (define-syntax
         >> foo
         >> (syntax-rules
         >> ()
         >> ((foo f) 'hi)))
         >> (foo ())

         If we find more of these situations where parsing fails but
         we should not wait for more input, it might be a better idea
         to equip the programmer with a button or key to flush the
         line buffer. */
    var lparens = inputLine.match(/\(/g);
    var rparens = inputLine.match(/\)/g);
    return !!(lparens && rparens && lparens.length === rparens.length);
  }
};
