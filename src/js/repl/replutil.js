goog.module('r5js.replutil');

const Datum = goog.require('r5js.Datum');
const Pipeline = goog.require('r5js.Pipeline');

/** @type {Pipeline.Interface} */ let pipeline = null;

/**
 * @param {string} input
 * @return {boolean}
 * @suppress {checkTypes} for Pipeline.forEnvironment(null).
 */
function isLineComplete(input) {
  if (!pipeline) {
    pipeline = Pipeline.forEnvironment(null);
  }

  try {
    const tokenStream = pipeline.scan(input);
    const datum = pipeline.parse(/** @type {!Datum} */ (pipeline.read(tokenStream)));
    return !tokenStream.nextToken() && !!datum;
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
    const lparens = input.match(/\(/g);
    const rparens = input.match(/\)/g);
    return !!(lparens && rparens && lparens.length === rparens.length);
  }
}

exports.isLineComplete = isLineComplete;
