import {Datum} from "../ast/datum";
import {Environment} from "../runtime/environment";
import {ListLikeTransformer} from "./list_like_transformer";
import {RenameHelper} from "./rename_helper";
import {TemplateBindings} from "./template_bindings";

export class Transformer {

  private readonly name: string;
  private readonly patternIds: { [key: string]: number };
  private readonly templateRenameCandidates: Set<string>;

  constructor(
      private readonly pattern: ListLikeTransformer,
      private readonly template: ListLikeTransformer) {
    this.name = pattern.getName();
    const renameHelper = new RenameHelper(this.name);
    this.pattern.collectNestingLevels(0, renameHelper);
    this.template.collectNestingLevels(0, renameHelper);
    this.patternIds = renameHelper.getPatternIds();
    this.templateRenameCandidates = renameHelper.getRenameCandidates();
  }

  /** @return True iff the transformer is a match (?) */
  matchInput(inputDatum: Datum,
             literalIds: Set<string>,
             definitionEnv: Environment,
             useEnv: Environment,
             bindings: TemplateBindings): boolean {
    return this.pattern.matchInput(
        inputDatum, literalIds, definitionEnv, useEnv, bindings);
  }

  /** @return The name of this transformer. */
  getName(): string {
    return this.name;
  }

  getTemplate(): ListLikeTransformer {
    return this.template;
  }

  getPatternIds(): { [key: string]: number } {
    return this.patternIds;
  }

  getTemplateRenameCandidates(): Set<string> {
    return this.templateRenameCandidates;
  }
}