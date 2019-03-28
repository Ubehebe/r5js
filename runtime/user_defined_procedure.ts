import {CompoundDatum} from "../ast/compound_datum";
import {Datum} from "../ast/datum";
import {Identifier} from "../ast/identifier";
import {List} from "../ast/list";
import {ProcCallLike} from "../ast/proc_call_like";
import {Quote} from "../ast/quote";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {extractDefinition} from "../ast/util";
import {Error} from "../error";
import {DEFINITION} from "../parse/nonterminals";
import {DEFINE} from "../parse/terminals";
import {Value} from "../value";
import {Environment} from "./environment";
import {ProcCall} from "./proc_call";
import {Procedure} from "./procedure";
import {TrampolineHelper} from "./trampoline_helper";

export class UserDefinedProcedure extends Procedure {

  private readonly env: Environment;
  private body: ProcCallLike | null;
  private last: ProcCallLike | null;

  /** @param formalsArray The procedure's formal parameters, in order. */
  constructor(
      protected readonly formalsArray: string[],
      bodyStart: CompoundDatum | null,
      env: Environment,
      private readonly name: string) {
    super();
    this.env = env.child();
    this.body = bodyStart && this.setupBody(bodyStart) ;
    this.last = this.body && this.body.getLast();
  }

  getName(): string {
    return this.name;
  }

  private setupBody(bodyStart: CompoundDatum): ProcCallLike {
    const helper = new LetrecBindingsHelper();
    const letrecBindings = helper.collectLetrecBindings(bodyStart);
    if (letrecBindings.isEmpty()) {
      return helper.getLast()!.sequence(this.env)!;
    } else {
      const letrec = new List(letrecBindings.toSiblings());
      letrec.setNextSibling(helper.getLast());
      return new ProcCall(new Identifier('letrec'), letrec);
    }
  }

  cloneWithEnv(env: Environment): UserDefinedProcedure {
    const ans = new (<typeof UserDefinedProcedure> this.constructor)
    (this.formalsArray, null /* bodyStart */, env, `${cloneWithEnvNameCounter++}`);
    ans.env.setClosuresFrom(this.env); // non-cloning ok?
    ans.body = this.body;
    ans.last = this.last;
    return ans;
  }

  private setContinuation(procCallLike: ProcCallLike) {
    /* This will be a vacuous write for a tail call. But that is
     probably still faster than checking if we are in tail position and,
     if so, explicitly doing nothing. */
    if (this.last) {
      this.last.setNext(procCallLike.getNext()!);
      this.last.setResultName(procCallLike.getResultName());
    }
  }

  /** TODO bl are we sure this covers all forms of tail recursion in R5RS? */
  private isTailCall(procCallLike: ProcCallLike): boolean {
    if (this.last === procCallLike) {
      // a good place to see if tail recursion is actually working :)
      // console.log('TAIL RECURSION!!!');
      return true;
    } else {
      return false;
    }
  }

  /** @override */
  toString(): string {
    return `proc:${this.name}`;
  }

  private setEnv(env: Environment) {
    if (this.body) {
      this.body.setStartingEnv(env);
    }
  }

  /** @param numActuals The number of arguments passed to the procedure during evaluation. */
  protected checkNumArgs(numActuals: number) {
    if (numActuals !== this.formalsArray.length) {
      throw Error.incorrectNumArgs(this.toString(), this.formalsArray.length, numActuals);
    }
  }

  protected bindArgs(args: Value[], env: Environment) {
    for (let i = 0; i < this.formalsArray.length; ++i) {
      env.addBinding(this.formalsArray[i], args[i]);
    }
  }

  /**
   * Example: suppose we have
   *
   * (define (foo x y) (+ x (* 2 y)))
   *
   * The body of this procedure is desugared as
   *
   * (* 2 y [_0 (+ x _0 [_1 ...])])
   *
   * Then we have the (nested) procedure call
   *
   * (+ 1 (foo 3 4))
   *
   * which is desugared as
   *
   * (foo 3 4 [foo' (+ 1 foo' [_2 ...])])
   *
   * We bind the arguments ("1" and "2") to the formal parameters ("x" and "y"),
   * append the ProcCall's continuation to the end of the Procedure's
   * continuation, and advance to the beginning of the Procedure's body.
   * Thus, on the next iteration of the trampoline loop, we will have
   * the following:
   *
   * (* 2 y [_0 (+ x _0 [foo' (+ 1 foo' [_2 ...])])])
   * @override
   */
  evaluate(args: Value[], procCallLike: ProcCallLike, trampolineHelper: TrampolineHelper, env: Environment) {
    const procCallEnv = procCallLike.getEnv()!;

    //If we're at a tail call we can reuse the existing environment.
    // Otherwise create a new environment pointing back to the current one.
    const newEnv = this.isTailCall(procCallLike)
        ? procCallEnv.allowRedefs()
        : this.env.child().addClosuresFrom(this.env);

    const next = procCallLike.getNext();
    /* Remember to discard the new environment
     at the end of the procedure call. */
    if (procCallEnv && next && !next.getEnv()) {
      next.setStartingEnv(procCallEnv);
    }

    // Do some bookkeeping to prepare for jumping into the procedure
    this.setContinuation(procCallLike);
    this.checkNumArgs(args.length);
    this.bindArgs(args, newEnv);
    this.setEnv(newEnv);

    // And away we go
    trampolineHelper.setNext(this.body!);
  }
}

let cloneWithEnvNameCounter = 0;

class LetrecBindingsHelper {

  private readonly bindings: SiblingBuffer = new SiblingBuffer();
  private last: Datum | null = null;

  /**
   * R5RS 5.2.2: "A <body> containing internal definitions can always be
   * converted into a completely equivalent letrec expression."
   */
  collectLetrecBindings(bodyStart: CompoundDatum): SiblingBuffer {
    let cur: CompoundDatum| null;
    for (cur = bodyStart;
         cur && cur.peekParse() === DEFINITION;
         cur = cur.getNextSibling() as CompoundDatum) {
      const firstChild = cur.getFirstChild();
      if (firstChild instanceof Identifier &&
          firstChild.getPayload() === DEFINE) {
        this.bindings.appendSibling(extractDefinition(cur));
      } else {
        cur.forEachChild(child => this.collectLetrecBindingsForChild(child));
      }
    }
    this.last = cur;
    return this.bindings;
  }

  private collectLetrecBindingsForChild(node: Datum) {
    if (!(node instanceof CompoundDatum)
        || node instanceof Quote) {
      return;
    }

    const firstChild = node.getFirstChild();

    if (firstChild instanceof Identifier
        && firstChild.getPayload() === DEFINE) {
      this.bindings.appendSibling(extractDefinition(node));
    } else if (node instanceof CompoundDatum) {
      node.forEachChild(child => this.collectLetrecBindingsForChild(child));
    }
  }

  getLast(): Datum | null {
    return this.last;
  }
}
