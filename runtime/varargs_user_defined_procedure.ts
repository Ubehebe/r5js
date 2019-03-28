import {CompoundDatum} from "../ast/compound_datum";
import {wrapValue} from "../ast/datum_util";
import {List} from "../ast/list";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {Error} from "../base/error";
import {Value} from "../base/value";
import {Environment} from "./environment";
import {UserDefinedProcedure} from "./user_defined_procedure";

export class VarargsUserDefinedProcedure extends UserDefinedProcedure {
  constructor(formalsArray: string[], bodyStart: CompoundDatum | null, env: Environment, name: string) {
    super(formalsArray, bodyStart, env, name);
  }

  /** @override */
  checkNumArgs(numActuals: number) {
    const minNumArgs = this.formalsArray.length - 1;
    if (numActuals < minNumArgs) {
      throw Error.tooFewVarargs(this.toString(), minNumArgs, numActuals);
    }
  }

  /** @override */
  bindArgs(args: Value[], env: Environment) {
    let name, i;

    for (i = 0; i < this.formalsArray.length - 1; ++i) {
      name = this.formalsArray[i];
      env.addBinding(name, args[i]);
    }

    if (this.formalsArray.length > 0) {
      name = this.formalsArray[i];
      // Roll up the remaining arguments into a list
      const siblingBuffer = new SiblingBuffer();
      for (let j = i; j < args.length; ++j) {
        siblingBuffer.appendSibling(wrapValue(args[j]));
      }
      env.addBinding(name, siblingBuffer.toList(List));
    }
  }
}
