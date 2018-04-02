import {CompoundDatum} from './compound_datum';
import {Datum, UNSPECIFIED_VALUE} from './datum';
import {Identifier} from './identifier';
import {Ref} from './ref';
import {wrapValue} from './datum_util';
import {Error} from '../error';
import {Pair} from './pair';
import {List} from './list';
import {QUOTE} from '../parse/terminals';
import {Value} from "../value";
import {Environment} from "../runtime/environment";
import {ProcCallLike, ProcCallResult} from "./proc_call_like";

export class Quote extends Pair {
  constructor(firstChild: Datum | null) {
    super();
    firstChild && this.setFirstChild(firstChild.setImmutable());
  }

  car(): Value {
    return CAR;
  }

  cdr(): Value {
    return new List(this.getFirstChild());
  }

  /** @override */
  fixParserSensitiveIds() {
  }

  /** @override */
  toProcCallLike(): ProcCallLike {
    return new QuoteShim(this);
  }
}

const CAR = new Identifier(QUOTE);

/** TODO bl the purpose of this class is unclear. */
class QuoteShim extends ProcCallLike {

  private readonly firstOperand: Quote;

  constructor(payload: Quote, continuationName: string | undefined = undefined) {
    super(continuationName);
    this.firstOperand = payload;
  }

  /** @override */
  evalAndAdvance(resultStruct: ProcCallResult, env: Environment, parserProvider: (x: any) => any /* TODO */) {
    const ans = this.tryQuote(this.firstOperand);
    if (ans !== null) {
      this.bindResult(ans);
      resultStruct.setValue(ans);
    }

    const nextContinuable = this.getNext();

    if (nextContinuable) {
      resultStruct.setNext(nextContinuable);
    }
  }

  private tryQuote(quote: Quote): Value | null {
    const env = this.getEnv();
    // Do the appropriate substitutions.
    const ans = quote.replaceChildren(
        (node) => node instanceof Identifier && node.shouldUnquote(),
        (node) => {
          const result: Value | null = env!.get(node.getPayload());
          let ans: Datum | null = (result === null)
              ? UNSPECIFIED_VALUE
              : wrapValue(result);
          // TODO bl document why we're doing this
          if (ans instanceof Ref) {
            ans = ans.deref();
          }
          if (node instanceof Identifier && node.shouldUnquoteSplice()) {
            if (ans instanceof List) {
              if (ans.getFirstChild()) { // `(1 ,@(list 2 3) 4) => (1 2 3 4)
                ans = ans.getFirstChild();
              } else { // `(1 ,@(list) 2) => (1 2)
                ans = null;
              }
            } else throw Error.quasiquote(ans + ' is not a list');
          }
          return ans;
        });
    // Now strip away the quote mark.
    // the newIdOrLiteral part is for (quote quote)
    return (ans instanceof CompoundDatum && ans.getFirstChild())
        ? ans.getFirstChild()
        : CAR;
  }
}
