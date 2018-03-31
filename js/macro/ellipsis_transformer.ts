import {Subtransformer} from "./subtransformer";
import {Datum} from "../ast/datum";
import {TemplateBindings} from "./template_bindings";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {RenameHelper} from "./rename_helper";
import {IEnvironment} from "../runtime/ienvironment";

export class EllipsisTransformer implements Subtransformer {

  constructor(private readonly subtransformer: Subtransformer) {}

  /** @override */
  matchInput(inputDatum: Datum,
             literalIds: { [key: string]: boolean },
             definitionEnv: IEnvironment,
             useEnv: IEnvironment,
             bindings: TemplateBindings): boolean {

    // We have to leave some evidence in the TemplateBindings object of  an empty match. Example:
    //
    //  (define-syntax foo
    //  (syntax-rules ()
    //  ((foo (x ...) ...)
    //  (+ (* x ...) ...))))
    //
    //  on input `(foo () () ())` should create a TemplateBindings object like
    //
    //  child 0:
    //  child 0:
    //  child 1:
    //  child 0:
    //  child 2:
    //  child 0:
    //
    //  so that we get the correct transcription `(+ (*) (*) (*)) => 3`.
    if (!inputDatum) {
      bindings.addChildBindings(
          new TemplateBindings(
              useEnv,
              bindings.getPatternIds(),
              bindings.getTemplateRenameCandidates()));
    }

    for (let subinput: Datum|null = inputDatum; subinput; subinput = subinput.getNextSibling()) {
      const childBindings = new TemplateBindings(
          useEnv,
          bindings.getPatternIds(),
          bindings.getTemplateRenameCandidates());
      const maybeMatched = this.subtransformer.matchInput(
          subinput, literalIds, definitionEnv, useEnv, childBindings);
      if (maybeMatched) {
        bindings.addOrIncorporateChild(childBindings);
      } else {
        return false;
      }
    }
    return true;
  }

  /** @override */
  toDatum(bindings: TemplateBindings): Datum | null {
    const buf = new SiblingBuffer();
    let bindingsToUse;
    let success;
    while ((bindingsToUse = bindings.getNextChild()) &&
    (success = this.subtransformer.toDatum(bindingsToUse))) {
      buf.appendSibling(success);
    }
    bindings.resetCurChild();
    return buf.toSiblings();
  }

  /** @override */
  collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper) {
    this.subtransformer.collectNestingLevels(ellipsisLevel + 1, renameHelper);
  }
}
