import {CompoundDatum} from "../ast/compound_datum";
import {Datum} from "../ast/datum";
import {Identifier} from "../ast/identifier";
import {Macro} from "../ast/macro";
import {Error} from "../base/error";
import {newCpsName} from "../parse/rename_util";
import {Environment} from "../runtime/environment";

/**
 * My approach for supporting nested ellipses in macro transcriptions
 * is to take a single pass through the input and build up a TemplateBindings
 * object whose tree structure mirrors the ellipsis nesting in the pattern.
 * For example:
 *
 * (define-syntax foo
 *  (syntax-rules ()
 *      ((foo ((x y) ...) ...)
 *      (quote (((x ...) (y ...)) ...)))))
 *
 * with input
 *
 * (foo ((a b)) ((c d) (e f)))
 *
 * produces this TemplateBindings object:
 *
 * child 0:
 *  child 0:
 *      x = a
 *      y = b
 * child 1:
 *  child 0:
 *      x = c
 *      y = d
 *  child 1:
 *      x = e
 *      y = f
 *
 * Then transcription involves a single pass through the template with
 * this TemplateBindings object, using the ellipses in the template
 * to descend through the TemplateBindings tree. Here's the flow of control
 * during transcription:
 *
 * 1. Transcribe ((x ...) (y ...)) with child0
 * 2. Transcribe x with child0.child0 => a
 * 3. Transcribe x with [no more children] => false. Reset cur child.
 * 4. Transcribe y with child0.child0 => b
 * 5. Transcribe y with [no more children] => false. Reset cur child.
 * [1 completes as ((a) (b))]
 * 6. Transcribe ((x ...) (y ...)) with child1
 * 7. Transcribe x with child1.child0 => c
 * 8. Transcribe x with child1.child1 => e
 * 9. Transcribe x with [no more children] => false. Reset cur child.
 * 10. Transcribe y with child1.child0 => d
 * 11. Transcribe y with child1.child1 => f
 * 12. Transcribe y with [no more children] => false. Reset cur child.
 * [6 completes as ((c e) (d f))]
 * 13. Transcribe ((x ...) (y ...)) with [no more children] => false.
 * Reset cur child.
 * [13 completes as (((a) (b)) ((c e) (d f)))]
 *
 * TODO bl: explain -- or ideally remove -- all the crazy logic dealing
 * with "incorporation". Do we even need it?
 */
export class TemplateBindings {

  private readonly bindings: { [key: string]: Datum } = {};
  private readonly children: TemplateBindings[] = [];
  private curChild = 0;
  private readonly renameInTemplate: Set<string> = new Set();

  constructor(
      private readonly letSyntaxEnv: Environment,
      private readonly patternIds: { [key: string]: number },
      private readonly templateRenameCandidates: Set<string>) {}

  resetCurChild(): this {
    this.curChild = 0;
    return this;
  }

  addTemplateBinding(name: string, val: Datum) {
    if (name in this.bindings) {
      throw Error.internalInterpreterError('invariant incorrect');
    } else if (val instanceof Macro) {
      // See comments at SchemeMacro.prototype.setIsLetOrLetrecSyntax
      const fakeName = newCpsName();
      this.letSyntaxEnv.addBinding(fakeName, val.getMacro());
      this.bindings[name] = new Identifier(fakeName);
    } else {
      this.bindings[name] = val;
    }

    if (val instanceof Identifier) {
      this.maybeRenameId(val);
    } else if (val instanceof CompoundDatum) {
      val.forEachChild(child => this.maybeRenameId(child));
    }
  }

  /**
   * We have to check the datum to be bound for conflicts with identifiers
   * in the template. Example:
   *
   * (define-syntax or
   * (syntax-rules ()
   * (or test1 test2 ...)
   * (let ((x test1)) (if x x (or test2 ...)))))
   *
   * (let ((x 4) (y 3)) (or x y))
   *
   * The identifier x occurs in the template but not the pattern, so it will
   * appear in templateRenameCandidates (see Transformer.prototype.setupIds).
   * Then, during pattern matching, addTemplateBinding(test1, x) will be
   * called. This should signal that, during transcription, any occurrence of
   * the _template's_ x should be safely renamed.
   * @see {r5js.Macro#transcribe}.
   */
  private maybeRenameId(datum: Datum) {
    if (datum instanceof Identifier) {
      const id = datum.getPayload();
      if (this.templateRenameCandidates.has(id)) {
        this.renameInTemplate.add(id);
      }
    } else if (datum instanceof CompoundDatum) {
      datum.forEachChild(child => this.maybeRenameId(child));
    }
  }

  addChildBindings(child: TemplateBindings): this {
    this.children.push(child);
    return this;
  }

  private hasNoneOf(other: TemplateBindings): boolean {
    for (const name in other.bindings) {
      if (name in this.bindings) {
        return false;
      }
    }
    return true;
  }

  /**
   * Try to incorporate the child's bindings in an existing child
   * if there's room, otherwise just tack the child on to the parent.
   */
  addOrIncorporateChild(child: TemplateBindings): this {
    return this.incorporateChild(child) || this.addChildBindings(child);
  }

  incorporateChild(child: TemplateBindings): this | null {
    // We only incorporate flat TemplateBindings objects.
    if (child.children.length > 0) {
      return null;
    }

    /* Dump all the child's bindings in the first child that doesn't
     have any of the bindings.

     todo bl: i have no idea why this heuristic seems to work. */
    for (let i = 0; i < this.children.length; ++i) {
      const candidate = this.children[i];
      if (candidate.hasNoneOf(child)) {
        for (const name in child.bindings) {
          candidate.addTemplateBinding(name, child.bindings[name]);
        }
        return this;
      }
    }

    return null;
  }

  getNextChild(): TemplateBindings | null {
    if (this.curChild < this.children.length) {
      return this.children[this.curChild++];
    } else {
      this.curChild = 0;   // reset for next time
      return null;
    }
  }

  /** TODO bl document what this does. */
  resolveDatum(datum: Datum): Datum | boolean {
    if (datum instanceof Identifier) {
      const name = datum.getPayload();
      const maybe = this.bindings[name];
      if (maybe) {
        return maybe.clone();
      } else if (this.patternIds[name] !== undefined) {
        // It's important to return false here, instead of some other "falsey" value like null.
        // This value is immediately returned by IdOrLiteralTransformer.prototype.matchInput.
        // Meanwhile, EllipsisTransformer.prototype.matchInput returns
        // new r5js.SiblingBuffer().toList() when it has successfully matched the ellipsis zero
        // times, which is not a failure. And if you look at the implementation, you will see there
        // is a good reason that  new r5js.SiblingBuffer().toList() === null. So we have to return
        // something different. Static types would be useful here.
        return false;
      } else {
        return datum.clone();
      }

    } else {
      return datum.clone();
    }
  }

  getPatternIds(): {[key: string]: number} {
    return this.patternIds;
  }

  getTemplateRenameCandidates(): Set<string> {
    return this.templateRenameCandidates;
  }

  wasRenamed(id: string): boolean {
    return this.renameInTemplate.has(id);
  }
}
