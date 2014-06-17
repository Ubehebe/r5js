/* Copyright 2011-2014 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.replutil');
goog.require('r5js.PipelineImpl');


/** @type {r5js.Pipeline} */ r5js.replutil.pipeline_;


/**
 * @param {string} input
 * @return {boolean}
 * @suppress {checkTypes} for new r5js.PipelineImpl(null).
 */
r5js.replutil.isLineComplete = function(input) {
  if (!r5js.replutil.pipeline_) {
    r5js.replutil.pipeline_ = new r5js.PipelineImpl(null);
  }

  try {
    var tokenStream = r5js.replutil.pipeline_.scan(input);
    var datum = r5js.replutil.pipeline_.parse(
        /** @type {!r5js.Datum} */ (r5js.replutil.pipeline_.read(tokenStream)));
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
    var lparens = input.match(/\(/g);
    var rparens = input.match(/\)/g);
    return !!(lparens && rparens && lparens.length === rparens.length);
  }
};