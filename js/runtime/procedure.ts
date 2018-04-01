import {ProcCallResult} from "../ast/datum";
import {ObjectValue, Value} from "../value";
import {Environment} from "./environment";
import {ProcCallLike} from "../ast/proc_call_like";

export class /* TODO interface */ Procedure implements ObjectValue {
  evaluate(args: Value[], procCall: ProcCallLike, resultStruct: ProcCallResult, env: Environment) {}
}