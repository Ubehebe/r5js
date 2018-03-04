import {ProcCallLike, ProcCallResult} from "../ast/datum";

export class /* TODO interface */ Procedure implements ObjectValue {
  evaluate(args: Value[], procCall: ProcCallLike, resultStruct: ProcCallResult, env: IEnvironment) {}
}