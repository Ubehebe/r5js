import {CompoundDatum} from "../ast/compound_datum";
import {Datum} from "../ast/datum";
import {DottedList, List} from "../ast/list";
import {Quote} from "../ast/quote";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {Vector} from "../ast/vector";
import {Environment} from "../runtime/environment";
import {EllipsisTransformer} from "./ellipsis_transformer";
import {MacroIdTransformer} from "./macro_id_transformer";
import {RenameHelper} from "./rename_helper";
import {Subtransformer} from "./subtransformer";
import {TemplateBindings} from "./template_bindings";

export interface ListLikeTransformer extends Subtransformer {
  addSubtransformer(subtransformer: Subtransformer): this;
  getName(): string;
}

export function list(): ListLikeTransformer {
  return new ListTransformer();
}

export function dottedList(): ListLikeTransformer {
  return new DottedListTransformer();
}

export function quote(): ListLikeTransformer {
  return new QuoteTransformer();
}

export function vector(): ListLikeTransformer {
  return new VectorTransformer();
}

class Base implements ListLikeTransformer {

  protected readonly subtransformers: Subtransformer[] = [];

  constructor(private readonly ctor: new (datum: Datum) => Datum) {}

  /** @override */
  addSubtransformer(subtransformer: Subtransformer): this {
    this.subtransformers.push(subtransformer);
    return this;
  }

  /** @override */
  getName(): string {
    return (this.subtransformers[0] as MacroIdTransformer)
        .getDatum()
        .getPayload();
  }

  /** @override */
  collectNestingLevels(ellipsisLevel: number, renameHelper: RenameHelper) {
    this.subtransformers.forEach(st => st.collectNestingLevels(ellipsisLevel, renameHelper));
  }

  couldMatch(inputDatum: Datum): boolean {
    return false;
  }

  /** @override */
  toDatum(bindings: TemplateBindings) {
    const siblingBuffer = this.toSiblingBuffer(bindings);
    return siblingBuffer && siblingBuffer.toList(this.ctor);
  }

  /** @override */
  matchInput(inputDatum: CompoundDatum,
             literalIds: Set<string>,
             definitionEnv: Environment,
             useEnv: Environment,
             bindings: TemplateBindings) {
    const len = this.subtransformers.length;
    const maybeEllipsis = this.subtransformers[len - 1] instanceof EllipsisTransformer
        && this.subtransformers[len - 1];

    if (!this.couldMatch(inputDatum)) {
      return false;
    }

    // R5RS 4.3.2: "an input form F matches a pattern P if and only if [...]
    // - P is a list (P1 ... Pn) and F is a list of n forms match P1 through Pn, respectively; or
    // - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list of n or more
    //   forms that match P1 through Pn, respectively, and whose nth "cdr" matches Pn+1; or
    // - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ... and F
    //   is a proper list of at least n forms, the first n of which match P1 through Pn,
    //   respectively, and each remaining element of F matches Pn+1; or
    // - P is a vector of the form #(P1 ...Pn) and F is a vector of n forms that match P1 through Pn; or
    // - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ... and F
    //   is a vector of n or more forms the first n of which match P1 through Pn, respectively,
    //   and each remaining element of F matches Pn+1".
    let subinput: Datum|null;
    let i = 0;
    for (subinput = inputDatum.getFirstChild(); subinput; subinput = subinput.getNextSibling(), ++i) {

      if (i === len - 1 && maybeEllipsis) {
        // If there's an ellipsis in the pattern, break out to deal with it.
        break;
      } else if (i >= len) {
        // If there's no ellipsis in the pattern and the input is longer than the pattern, this is
        // a failure.
        return false;
      } else if (!this.subtransformers[i].matchInput(
              subinput, literalIds, definitionEnv, useEnv, bindings)) {
        // If pattern matching on the subinput and subpattern fails, this is a failure.
        return false;
      }
    }

    if (maybeEllipsis) {
      // Corner case: an empty input like () cannot match a pattern like (x y ...)
      return (!inputDatum.getFirstChild() && len > 1) ?
          false :
          maybeEllipsis.matchInput(
              subinput!,
              literalIds, definitionEnv, useEnv, bindings);
    } else {
      // If we matched all of the input without getting through all of the pattern, this is
      // a failure.
      return i === len;
    }
  }

  private toSiblingBuffer(bindings: TemplateBindings): SiblingBuffer | null {
    const buf = new SiblingBuffer();

    for (const subtransformer of this.subtransformers) {
      const success = subtransformer.toDatum(bindings) as (Datum|boolean);
      if (success === false) {
        return null;
      } else if (success) {
        buf.appendSibling(success as Datum);
      }
    }
    return buf;
  }
}

class QuoteTransformer extends Base {
  constructor() {
    super(Quote);
  }

  /**
   * This is a no-op mainly so we don't accidentally rename identifiers inside
   * quotes in {@link r5js.Transformer#setupIds_}.
   * @override
   */
  collectNestingLevels() {
  }
}

class VectorTransformer extends Base {
  constructor() {
    super(Vector);
  }

  /** @override */
  couldMatch(inputDatum: Datum) {
    // Vector patterns match only vector inputs
    return inputDatum instanceof Vector;
  }
}

class ListTransformer extends Base {
  constructor() {
    super(List);
  }

  /** @override */
  couldMatch(inputDatum: Datum) {
    // Proper list patterns can match only proper list inputs
    return inputDatum instanceof List;
  }
}

class DottedListTransformer extends Base {
  constructor() {
    super(DottedList);
  }

  /** @override */
  couldMatch(inputDatum: Datum) {
    // Dotted list patterns can match proper or dotted list inputs
    return inputDatum instanceof List || inputDatum.isImproperList();
  }

  /** @override */
  matchInput(
      inputDatum: CompoundDatum,
      literalIds: Set<string>,
      definitionEnv: Environment,
      useEnv: Environment,
      bindings: TemplateBindings) {
    const len = this.subtransformers.length;
    const maybeEllipsis =
        this.subtransformers[len - 1] instanceof EllipsisTransformer
        && this.subtransformers[len - 1];

    if (!this.couldMatch(inputDatum)) {
      return false;
    }

    /* R5RS 4.3.2: "an input form F matches a pattern P if and only if [...]
     - P is a list (P1 ... Pn) and F is a list of n forms match P1 through Pn,
     respectively; or
     - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or
     improper list of n or more forms that match P1 through Pn, respectively,
     and whose nth "cdr" matches Pn+1; or
     - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
     the identifier ... and F is a proper list of at least n forms,
     the first n of which match P1 through Pn, respectively,
     and each remaining element of F matches Pn+1; or
     - P is a vector of the form #(P1 ...Pn) and F is a vector of n forms
     that match P1 through Pn; or
     - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
     the identifier ... and F is a vector of n or more forms the first n
     of which match P1 through Pn, respectively, and each remaining element
     of F matches Pn+1" */
    let subinput;
    let i = 0;
    for (subinput = inputDatum.getFirstChild();
         subinput;
         subinput = subinput.getNextSibling(), ++i) {

      if (i === len - 1) {
        // If there's an ellipsis in the pattern, break out to deal with it.
        break;
      } else if (i >= len) {
        /* If there's no ellipsis in the pattern and the input is longer
         than the pattern, this is a failure. */
        return false;
      } else if (!this.subtransformers[i].matchInput(
              subinput, literalIds, definitionEnv, useEnv, bindings)) {
        /* If pattern matching on the subinput and subpattern fails, this is
         a failure. */
        return false;
      }
    }

    if (maybeEllipsis) {
      /* Corner case:
       an empty input like () cannot match a pattern like (x y ...) */
      return (!inputDatum.getFirstChild() && len > 1) ?
          false :
          maybeEllipsis.matchInput(subinput!, literalIds, definitionEnv, useEnv, bindings);
    } else {
      // Dotted-list patterns cannot end in ellipses.
      let toMatchAgainst: Datum|null = null;

      if (inputDatum instanceof List) {
        toMatchAgainst = new SiblingBuffer().appendSibling(subinput!).toList(List);
      } else if (inputDatum.isImproperList()) {
        toMatchAgainst = subinput!.getNextSibling()
            ? new SiblingBuffer().appendSibling(subinput!).toList(DottedList)
            : subinput;
      }

      return this.subtransformers[i].matchInput(
          toMatchAgainst!, literalIds, definitionEnv, useEnv, bindings);
    }
  }
}
