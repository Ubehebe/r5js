export class ProcCallLike {

    private resultName_: string;
    private next_: ProcCallLike|null;
    private env_: IEnvironment|null;

    constructor(lastResultName:string=`@${counter++}`) {
        this.resultName_ = lastResultName;
    }

    /**
     * parserProvider Function that will return a new Parser for the given Datum when called.
     */
    evalAndAdvance(resultStruct: ProcCallResult /* TODO */, env: IEnvironment, parserProvider: any /* TODO */) {}

    getResultName(): string {
        return this.resultName_;
    }

    /** TODO bl remove. */
    setResultName(resultName: string) {
        this.resultName_ = resultName;
    }

    setStartingEnv(env: IEnvironment) {
        this.env_ = env;
    }

    getEnv(): IEnvironment|null {
        return this.env_;
    }

    /** Clears the current environment. TODO bl not well understood. */
    clearEnv() {
        this.env_ = null;
    }

    getNext(): ProcCallLike|null {
        return this.next_;
    }

    setNext(next: ProcCallLike) {
        this.next_ = next;
    }

    bindResult(val: Value) {
        /* If the next procedure call already has an environment,
         bind the result there. Otherwise, bind it in the current
         environment; it will be carried forward by the EnvBuffer. */
        const envToUse = (this.next_ && this.next_.getEnv()) || this.env_;
        envToUse && envToUse.addBinding(this.resultName_, val);
    }
}

let counter: number = 0;

export function appendProcCallLike(procCallLike: ProcCallLike, next: ProcCallLike) {
  getLastProcCallLike(procCallLike).setNext(next);
}

export function getLastProcCallLike(procCallLike: ProcCallLike): ProcCallLike {
  const maybeNext = procCallLike.getNext();
  return maybeNext ? getLastProcCallLike(maybeNext) : procCallLike;
}

export class ProcCallResult {
    setNext(procCallLike: ProcCallLike) {}
    setValue(value: Value) {}
    getNextProcCallLike(): ProcCallLike|null {
        return null;
    }
}