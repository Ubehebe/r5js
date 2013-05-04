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


goog.provide('r5js.tmp.trampoline');

/* This is the main evaluation function.

    The subtlest part is probably the question "what is the current environment?"
    In general, a Continuable object should have an attached Environment
    object that tells it where to look up identifiers. The code that attaches
    Environments to Continuables is scattered about and may be buggy.

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
 * @suppress {undefinedVars} For console.
 * TODO bl: remove the @suppress.
 */
function trampoline(continuable, inputPort, outputPort, debug) {

    var cur = continuable;
    var resultStruct = new TrampolineHelper(inputPort, outputPort);
    var savedEnv = new EnvBuffer();
    var ans;

    /* The debug check is hoisted out of the while loop because this
     is expected to be hot code. */
    if (debug) {

        while (cur) {
            // a good first step for debugging:
            console.log('boing: ' + cur);
            resultStruct = cur.subtype.evalAndAdvance(cur.continuation, resultStruct, savedEnv);
            ans = resultStruct.ans;
            cur = resultStruct.nextContinuable;
            resultStruct.clear();
        }

    } else {
        while (cur) {
            resultStruct = cur.subtype.evalAndAdvance(cur.continuation, resultStruct, savedEnv);
            ans = resultStruct.ans;
            cur = resultStruct.nextContinuable;
            resultStruct.clear();
        }
    }

    return ans;
}
