import {Subtransformer} from "./subtransformer";
import {SimpleDatum} from "../ast/simple_datum";
import {Identifier} from "../ast/identifier";
import {Datum} from "../ast/datum";
import {TemplateBindings} from "./template_bindings";
import {RenameHelper} from "./rename_helper";
import {Environment} from "../runtime/environment";

export abstract class MacroIdTransformer implements Subtransformer {

  constructor(protected readonly datum: SimpleDatum<any>) {}

  /** @override */
  matchInput(inputDatum: any /* TODO tighten */,
             literalIds: Set<string>,
             definitionEnv: Environment,
             useEnv: Environment,
             bindings: TemplateBindings): boolean {


    // R5RS 4.3.2: "An input form F matches a pattern P if and only if [...] P is a datum and F
    // is equal to P in the sense of the equal? procedure."
    if (!(this.datum instanceof Identifier)) {
      return inputDatum.isEqual(this.datum);
    }

    // TODO: there used to be logic here for the following situation:
    // R5RS 4.3.2: "A subform in the input matches a literal identifier if and only if it is an
    // identifier and either both its occurrence in the macro expression and its occurrence in the
    // macro definition have the same lexical binding, or the two identifiers are equal and both
    // have no lexical binding."
    // I deleted this code when a refactoring I was doing (changing literalIds from a plain object
    // to a Set) proved that the code was dead. This probably means I need to add tests.
    bindings.addTemplateBinding(this.datum.getPayload(), inputDatum);
    return true;
  }

  abstract collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper);

  /** @override */
  toDatum(bindings: TemplateBindings): Datum | null {
    return bindings.resolveDatum(this.datum) as Datum;
  }

  getDatum(): SimpleDatum<any> {
    return this.datum;
  }

  static pattern(datum: SimpleDatum<any>): Subtransformer {
    return new PatternIdTransformer(datum);
  }

  static template(datum: SimpleDatum<any>): Subtransformer {
    return new TemplateIdTransformer(datum);
  }
}

class PatternIdTransformer extends MacroIdTransformer {
  constructor(datum: SimpleDatum<any>) {
    super(datum);
  }

  /** @override */
  collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper) {
    if (this.datum instanceof Identifier) {
      renameHelper.recordPatternId(this.datum.getPayload(), ellipsisLevel);
    }
  }
}

class TemplateIdTransformer extends MacroIdTransformer {
  constructor(datum: SimpleDatum<any>) {
    super(datum);
  }

  /** @override */
  collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper) {
    if (this.datum instanceof Identifier) {
      renameHelper.recordTemplateId(this.datum.getPayload(), ellipsisLevel);
    }
  }
}