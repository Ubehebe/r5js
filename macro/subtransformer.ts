import {Datum} from "../ast/datum";
import {ObjectValue} from "../base/value";
import {Environment} from "../runtime/environment";
import {RenameHelper} from "./rename_helper";
import {TemplateBindings} from "./template_bindings";

export interface Subtransformer extends ObjectValue /* TODO inappropriate */ {
  /** @param ellipsisLevel Nesting level of ellipses, passed as the last argument to the callback. */
  collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper): void;

  /**
   * @return True iff the transformer is a match (?)
   * TODO bl: what is the use of the value type in the literalIds dictionary?
   */
  matchInput(inputDatum: Datum,
             literalIds: Set<string>,
             definitionEnv: Environment,
             useEnv: Environment,
             bindings: TemplateBindings): boolean;

  /** @return null if something failed. TODO: if what failed? */
  toDatum(bindings: TemplateBindings): Datum | null;
}
