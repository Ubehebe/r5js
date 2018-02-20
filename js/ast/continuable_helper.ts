import {ProcCallLike, getLastProcCallLike} from './datum';

/**
 * A buffer to accumulate a Continuable-Continuation chain
 * without the caller having to do the pointer arithmetic.
 */
export class ContinuableHelper {

  private firstProcCallLike_: ProcCallLike | null = null;
  private lastProcCallLike_: ProcCallLike | null = null;

  appendProcCallLike(procCallLike: ProcCallLike) {
    if (!this.firstProcCallLike_) {
      this.firstProcCallLike_ = procCallLike;
    } else {
      this.lastProcCallLike_!.setNext(procCallLike);
    }
    this.lastProcCallLike_ = getLastProcCallLike(procCallLike);
  }

  toContinuable(): ProcCallLike | null {
    return this.firstProcCallLike_;
  }
}