import {ProcCallLike, ProcCallResult} from "../ast/datum";
import {ObjectValue, Value} from "../value";
import {Environment} from "./environment";

export class /* TODO interface */ Procedure implements ObjectValue {
  evaluate(args: Value[], procCall: ProcCallLike, resultStruct: ProcCallResult, env: Environment) {}
}