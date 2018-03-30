import {Subtransformer} from "./subtransformer";
import {SimpleDatum} from "../ast/simple_datum";
import {Identifier} from "../ast/identifier";
import {Datum} from "../ast/datum";
import {TemplateBindings} from "./template_bindings";
import {RenameHelper} from "./rename_helper";

export abstract class MacroIdTransformer implements Subtransformer {

  constructor(protected readonly datum: SimpleDatum<any>) {}

  /** @override */
  matchInput(inputDatum: any /* TODO tighten */,
             literalIds: { [key: string]: boolean },
             definitionEnv: IEnvironment,
             useEnv: IEnvironment,
             bindings: TemplateBindings): boolean {


    // R5RS 4.3.2: "An input form F matches a pattern P if and only if [...] P is a datum and F
    // is equal to P in the sense of the equal? procedure."
    if (!(this.datum instanceof Identifier)) {
      return inputDatum.isEqual(this.datum);
    }

    // R5RS 4.3.2: "A subform in the input matches a literal identifier if and only if it is an
    // identifier and either both its occurrence in the macro expression and its occurrence in the
    // macro definition have the same lexical binding, or the two identifiers are equal and both
    // have no lexical binding."
    if (this.datum.getPayload() in literalIds) {
      return inputDatum instanceof Identifier
          && (this.datumsAreEqualAndUnbound(inputDatum, definitionEnv, useEnv)
              || datumsHaveSameLexicalBinding(inputDatum, definitionEnv, useEnv));
    } else {
      /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
       [...] P is a non-literal identifier [...]".
       That is, non-literal identifiers match anything. */
      bindings.addTemplateBinding(
          /** @type {string} */ (this.datum.getPayload()), inputDatum);
      return true;
    }
  }

  abstract collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper);

  private datumsAreEqualAndUnbound(
      inputDatum: Identifier,
      definitionEnv: IEnvironment,
      useEnv: IEnvironment): boolean {
    const name = inputDatum.getPayload();
    return name === this.datum.getPayload()
        && !definitionEnv.hasBindingRecursive(name)
        && !useEnv.hasBindingRecursive(name);
  }

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

function datumsHaveSameLexicalBinding(
    inputDatum: Identifier,
    definitionEnv: IEnvironment,
    useEnv: IEnvironment): boolean {
  const name = inputDatum.getPayload();
  return definitionEnv.get(name) === useEnv.get(name);
}