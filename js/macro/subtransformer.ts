import {RenameHelper} from "./rename_helper";
import {Datum} from "../ast/datum";
import {TemplateBindings} from "./template_bindings";

export class /* TODO should be interface */ Subtransformer implements ObjectValue /* TODO inappropriate */ {
  /** @param ellipsisLevel Nesting level of ellipses, passed as the last argument to the callback. */
  collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper) {
  }

  /**
   * @return True iff the transformer is a match (?)
   * TODO bl: what is the use of the value type in the literalIds dictionary?
   */
  matchInput(inputDatum: Datum,
             literalIds: { [key: string]: boolean },
             definitionEnv: IEnvironment,
             useEnv: IEnvironment,
             bindings: TemplateBindings): boolean {
    return false;
  }

  /** @return null if something failed. TODO: if what failed? */
  toDatum(bindings: TemplateBindings): Datum | null {
    return null;
  }
}

