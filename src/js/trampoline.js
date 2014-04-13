/* Copyright 2011, 2012 Brendan Linn

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


goog.provide('r5js.trampoline');


goog.require('r5js.EnvBuffer');
goog.require('r5js.Parser');
goog.require('r5js.TrampolineHelper');

/* This is the main evaluation function.

    The subtlest part is probably the question "what is the current
    environment?" In general, a Continuable object should have an attached
    Environment object that tells it where to look up identifiers.
    The code that attaches Environments to Continuables is scattered about
    and may be buggy.

 Here is a worked example. Pen and paper is recommended!

 (define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))

 (fac 3 [_0 ...]) ; create new env A where n = 3

 [jump to procedure body, choose consequent]

 (*{env A} n (fac (- n 1)) [_0 ...]) ; this needs to be CPSified

 (-{env A} n 1 [_1
    (fac{env A} _1 [_2
        (*{env A} n _2 [_0 ...])])]) ; CPSified

 [bind _1 = 2 in env A]

 (fac{env A} _1 [_2
    (*{env A} n _2 [_0 ...])]) ; create new env B where n = 2

 [jump to procedure body, choose consequent]

 (*{env B} n (fac (- n 1)) [_2
    (*{env A} n _2 [_0 ...])]) ; this needs to be CPSified

 (-{env B} n 1 [_3
    (fac{env B} _3 [_4
        (*{env B} n _4 [_2
            (*{env A} n _2 [_0 ...])])]) ; CPSified

 [bind _3 = 1 in env B]

 (fac{env B} _3 [_4
    (*{env B} n _4 [_2
        (*{env A} n _2 [_0 ...])])]) ; create new env C where n = 1

 [jump to procedure body, choose consequent]

 (*{env C} n (fac (- n 1)) [_4
    (*{env B} n _4 [_2
        (*{env A} n _2 [_0 ...])])]) ; this needs to be CPSified

 (-{env C} n 1 [_5
    (fac{env C} _5 [_6
        (*{env C} n _6 [_4
            (*{env B} n _4 [_2
                (*{env A} n _2 [_0 ...])])])])]) ; CPSified

 [bind _5 = 0 in env C]

 (fac{env C} _5 [_6
    (*{env C} n _6 [_4
        (*{env B} n _4 [_2
            (*{env A} n _2 [_0 ...])])])]) ; create new env D where n = 0

 [jump to procedure body, choose alternate]

 (id{env D} 1 [_6
    (*{env C} n _6 [_4
        (*{env B} n _4 [_2
            (*{env A} n _2 [_0 ...])])])]) ; bind _6 = 1 in env C

 (*{env C} n _6 [_4
    (*{env B} n _4 [_2
        (*{env A} n _2 [_0 ...])])]) ; bind _4 = 1 in env B

 (*{env B} n _4 [_2
    (*{env A} n _2 [_0 ...])]) ; bind _2 = 2 in env A


 (*{env A} n _2 [_0 ...]) ; bind _0 = 6 in env whatever
 */
/**
 * @param {!r5js.Continuable} continuable The continuable object to evaluate.
 * @param {!r5js.InputPort} inputPort Input port.
 * @param {!r5js.OutputPort} outputPort Output port.
 * @param {goog.log.Logger} logger Logger, for debugging messages.
 * @return {?} TODO bl what does this return?
 * @suppress {uselessCode} for the currently-disabled debugging branch.
 */
r5js.trampoline = function(continuable, inputPort, outputPort, logger) {

  var cur = continuable;
  var resultStruct = new r5js.TrampolineHelper(inputPort, outputPort);
  var savedEnv = new r5js.EnvBuffer();
  var ans;

    while (cur) {
      resultStruct = cur.getSubtype().evalAndAdvance(
          cur.getContinuation(),
          resultStruct,
          savedEnv,
          parserProvider);
      ans = resultStruct.getValue();
      cur = resultStruct.getNextContinuable();
      resultStruct.clear();
    }

  return ans;
};


/**
 * @param {!r5js.Datum} datum Root of the parse tree.
 * @return {!r5js.Parser} New parser that will parse the given datum.
 */
function parserProvider(datum) {
  return new r5js.Parser(datum);
}
