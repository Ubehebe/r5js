import {EXPRESSION} from '../parse/nonterminals';
import {COMMA, COMMA_AT} from '../parse/terminals';
import {Environment} from "../runtime/environment";
import {CompoundDatum} from './compound_datum';
import {ContinuableHelper} from './continuable_helper';
import {Datum} from './datum';
import {Identifier} from './identifier';
import {ProcCallLike, ProcCallResult} from "./proc_call_like";
import {Quote} from './quote';
import {Unquote} from './unquote';
import {UnquoteSplicing} from './unquote_splicing';

export class Quasiquote extends CompoundDatum {
  constructor(firstChild: Datum) {
    super();
    this.setFirstChild(firstChild);
  }

  /**
   * TODO bl: not sure this is the right thing to do for quasiquotes.
   * We can't just "unescape" the quasiquotations. Example:
   *
   * (equal? '(a `(b ,(+ 1 2))) '(a `(b ,(+ 1 2))))
   *
   * This will eventually call
   *
   * (eqv? `(b ,(+ 1 2)) `(b ,(+ 1 2)))
   *
   * From this procedure call, it looks as if we should unescape the quasiquotes,
   * but that's incorrect; we've lost the surrounding quotation level.
   * It may be possible to figure out what to do based on the qqLevels,
   * but it's been a while since I've looked at that subsystem.
   *
   * @override
   */
  eqv(other: Datum): boolean {
    return this.isEqual(other as CompoundDatum);
  }

  /** Example: `(1 ,(+ 2 3)) should desugar as (+ 2 3 [_0 (id (1 _0) [_2 ...])]) */
  processQuasiquote(env: Environment, parserProvider: (datum: Datum) => any /* TODO should be Parser */): ProcCallLike {
    const newCalls = new ContinuableHelper();
    const qqLevel = this.qqLevel;

    this.replaceChildren(
        node => (node instanceof Unquote || node instanceof UnquoteSplicing)
              && node.getQQLevel() === qqLevel,
        node => {
          const asContinuable = parserProvider((node as CompoundDatum).getFirstChild()!)
              .parse(EXPRESSION)
              .desugar(env, true);
          /* Throw out the last result name and replace it with another
           identifier (also illegal in Scheme) that will let us know if it's
           unquotation or unquotation with splicing. */
          const name = (node instanceof Unquote
              ? COMMA
              : COMMA_AT) + '' + counter++;
          const last = asContinuable.getLast();
          last.setResultName(name);
          newCalls.appendProcCallLike(asContinuable);
          return new Identifier(name);
        });

    const newDatum = new Quote(this.getFirstChild());

    newCalls.appendProcCallLike(newDatum.toProcCallLike());
    return newCalls.toContinuable() as ProcCallLike;
  }

  /** @override */
  setQuasiquotationLevel(qqLevel: number): this {
    this.qqLevel = qqLevel + 1;
    return super.setQuasiquotationLevel(this.qqLevel);
  }

  /** @override */
  toProcCallLike(): ProcCallLike {
    return new QuasiquoteShim(this);
  }
}

/** TODO bl the purpose of this class is unclear. */
class QuasiquoteShim extends ProcCallLike {

  private readonly firstOperand: Quasiquote;

  constructor(payload: Quasiquote, continuationName: string | undefined = undefined) {
    super(continuationName);
    this.firstOperand = payload;
  }

  /** @override */
  evalAndAdvance(resultStruct: ProcCallResult,
                 env: Environment,
                 parserProvider: (datum: Datum) => any /* TODO should be Parser */) {
    const next = this.tryQuasiquote(this.firstOperand, parserProvider);
    if (next) {
      resultStruct.setNext(next);
    }
  }

  private tryQuasiquote(quasiquote: Quasiquote, parserProvider: (datum: Datum) => any /* TODO Parser */): ProcCallLike {
    const continuable = quasiquote.processQuasiquote(this.getEnv()!, parserProvider);
    const next = this.getNext();
    if (next) {
      continuable.append(next);
    }
    return continuable;
  }
}

let counter = 0;