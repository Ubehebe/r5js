import {ProcCallLike} from "../ast/proc_call_like";
import {Value} from "../base/value";
import {InputPort} from "../io/input_port";
import {OutputPort} from "../io/output_port";
import {ParserImpl} from "../parse/parser_impl";
import {Environment} from "./environment";
import {TrampolineHelper} from "./trampoline_helper";

/**
 * This is the main evaluation function.
 *
 * The subtlest part is probably the question "what is the current
 * environment?" In general, a Continuable object should have an attached
 * Environment object that tells it where to look up identifiers.
 * The code that attaches Environments to Continuables is scattered about
 * and may be buggy.
 *
 * Here is a worked example. Pen and paper is recommended!
 *
 * (define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))
 *
 * (fac 3 [_0 ...]) ; create new env A where n = 3
 *
 * [jump to procedure body, choose alternate]
 *
 * (*{env A} n (fac (- n 1)) [_0 ...]) ; this needs to be CPSified
 *
 * (-{env A} n 1 [_1
 *     (fac{env A} _1 [_2
 *         (*{env A} n _2 [_0 ...])])]) ; CPSified
 *
 * [bind _1 = 2 in env A]
 *
 * (fac{env A} _1 [_2
 *     (*{env A} n _2 [_0 ...])]) ; create new env B where n = 2
 *
 * [jump to procedure body, choose alternate]
 *
 * (*{env B} n (fac (- n 1)) [_2
 *     (*{env A} n _2 [_0 ...])]) ; this needs to be CPSified
 *
 * (-{env B} n 1 [_3
 *     (fac{env B} _3 [_4
 *         (*{env B} n _4 [_2
 *             (*{env A} n _2 [_0 ...])])]) ; CPSified
 *
 * [bind _3 = 1 in env B]
 *
 * (fac{env B} _3 [_4
 *     (*{env B} n _4 [_2
 *         (*{env A} n _2 [_0 ...])])]) ; create new env C where n = 1
 *
 * [jump to procedure body, choose alternate]
 *
 * (*{env C} n (fac (- n 1)) [_4
 *     (*{env B} n _4 [_2
 *         (*{env A} n _2 [_0 ...])])]) ; this needs to be CPSified
 *
 * (-{env C} n 1 [_5
 *     (fac{env C} _5 [_6
 *         (*{env C} n _6 [_4
 *             (*{env B} n _4 [_2
 *                 (*{env A} n _2 [_0 ...])])])])]) ; CPSified
 *
 * [bind _5 = 0 in env C]
 *
 * (fac{env C} _5 [_6
 *     (*{env C} n _6 [_4
 *         (*{env B} n _4 [_2
 *             (*{env A} n _2 [_0 ...])])])]) ; create new env D where n = 0
 *
 * [jump to procedure body, choose consequent]
 *
 * (id{env D} 1 [_6
 *     (*{env C} n _6 [_4
 *         (*{env B} n _4 [_2
 *             (*{env A} n _2 [_0 ...])])])]) ; bind _6 = 1 in env C
 *
 * (*{env C} n _6 [_4
 *     (*{env B} n _4 [_2
 *         (*{env A} n _2 [_0 ...])])]) ; bind _4 = 1 in env B
 *
 * (*{env B} n _4 [_2
 *     (*{env A} n _2 [_0 ...])]) ; bind _2 = 2 in env A
 *
 * (*{env A} n _2 [_0 ...]) ; bind _0 = 6 in env whatever
 *
 * @param procCallLike The continuable object to evaluate.
 */
export function trampoline(
  procCallLike: ProcCallLike,
  startingEnv: Environment,
  inputPort: InputPort,
  outputPort: OutputPort): Value {
  let cur: ProcCallLike | null = procCallLike;
  const resultStruct = new TrampolineHelper(inputPort, outputPort);
  let prevEnv: Environment | null = startingEnv;

  while (cur) {
    let curEnv = cur.getEnv();
    // If the procedure call has no attached environment, we use the environment left over
    // from the previous action on the trampoline.
    if (!curEnv && prevEnv) {
      cur.setStartingEnv(prevEnv);
    }
    cur.evalAndAdvance(
      resultStruct,
      prevEnv!,
      datum => new ParserImpl(datum));
    // Save the environment we used in case the next action on the trampoline needs it.
    curEnv = cur.getEnv();
    prevEnv = curEnv;
    // We shouldn't leave the environment pointer hanging around.
    cur.clearEnv();
    cur = resultStruct.getNextProcCallLike();
    resultStruct.clear();
  }

  return resultStruct.getValue();
}
