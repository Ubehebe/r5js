/**
 * Example: (g (f x y) z) desugared is
 *
 * (f x y [f' (g f' z [g' ...])])
 *
 * The continuation c is [f' (g f' z [g' ...])]
 *
 * c.lastResultName is f'
 * c.nextContinuable is (g f' z ...)
 */
import {ProcCallLike, ProcCallResult} from "../ast/datum";
import {Value} from "../value";

export class Continuation {

  protected readonly lastResultName: string;
  protected readonly nextContinuable: ProcCallLike | null;

  /**
   * @param resultName Optional name to use for the last result.
   *     If not given, a unique name will be created.
   */
  constructor(resultName: string, next: ProcCallLike | null) {
    this.lastResultName = resultName;
    this.nextContinuable = next;
  }

  evaluate(arg: Value, procCallLike: ProcCallLike, resultStruct: ProcCallResult) {
    procCallLike.getEnv()!.addBinding(this.lastResultName, arg);
    resultStruct.setValue(arg);
    if (this.nextContinuable) {
      resultStruct.setNext(this.nextContinuable);
    }
    Continuation.repairInfiniteLoop(procCallLike, resultStruct);
  }

  /**
   * Cut out the current proc call from the continuation chain to avoid an
   * infinite loop. Example:
   *
   * (define cont #f)
   * (display
   * (call-with-current-continuation
   * (lambda (c)
   * (set! cont c)
   * "inside continuation")))
   * (cont "outside continuation")
   * 42
   *
   * This should display "inside continuation", then "outside continuation",
   * then return 42. When the trampoline is at
   *
   * (cont "outside continuation")
   *
   * proc.nextContinuable will be something like
   *
   * (cont "outside continuation" _0 [_0 (id 42 [_1 ...])])
   *
   * We clearly have to cut out the first part of this chain to avoid an
   * infinite loop.
   */
  static repairInfiniteLoop(procCall: ProcCallLike, resultStruct: ProcCallResult) {
    for (let
             tmp: ProcCallLike|null = resultStruct.getNextProcCallLike(),
             prev: ProcCallLike|undefined;
         tmp;
         prev = tmp, tmp = tmp.getNext()) {
      if (tmp === procCall) {
        if (prev) {
          prev.setNext(
              // TODO bl remove cast. At least one test relies on setNext(null) here.
              tmp.getNext()!);
        }
        return;
      }
    }
  }
}