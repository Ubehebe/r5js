import {ProcCallLike, ProcCallResult} from "../ast/datum";
import {ObjectValue, Value} from "../value";
import {IEnvironment} from "./ienvironment";

export class /* TODO interface */ Procedure implements ObjectValue {
  evaluate(args: Value[], procCall: ProcCallLike, resultStruct: ProcCallResult, env: IEnvironment) {}
}