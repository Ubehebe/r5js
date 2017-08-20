goog.module('r5js.Branch');

const ProcCall = goog.require('r5js.ProcCall');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');

class Branch extends ProcCallLike {
  /**
   * @param {string} testResultName
   * @param {!ProcCall} consequent
   * @param {!ProcCall} alternate
   */
  constructor(testResultName, consequent, alternate) {
    super();
    /** @const @private */ this.testResultName_ = testResultName;
    /** @const @private */ this.consequent_ = consequent;
    /** @const @private */ this.alternate_ = alternate;
    /** @const @private */ this.consequentLastContinuable_ = ProcCallLike.getLast(this.consequent_);
    /** @const @private */ this.alternateLastContinuable_ = ProcCallLike.getLast(this.alternate_);
  }

  /** @override */
  setStartingEnv(env) {
    if (!this.consequent_.getEnv()) {
      this.consequent_.setStartingEnv(env);
    }
    if (!this.alternate_.getEnv()) {
      this.alternate_.setStartingEnv(env);
    }
  }

  /** @override */
  getEnv() {
    return null;
  }

  /**
  * @override
  * TODO bl: this method relies on the fact that this.next_ can be null.
  * The casts are incorrect. Investigate and correct.
  */
  evalAndAdvance(resultStruct, env, parserProvider) {
    /* Branches always use the old environment left by the previous action
      on the trampoline. */
    const testResult = env.get(this.testResultName_);
    if (testResult === false) {
      this.alternateLastContinuable_.setNext(
          /** @type {!ProcCallLike} */(this.getNext()));
      this.alternateLastContinuable_.setResultName(this.getResultName());
      resultStruct.setNext(this.alternate_);
      /* We must clear the environment off the non-taken branch.
           See comment at {@link r5js.Continuation.rememberEnv}.
           TODO bl: clearEnv is defined only on {@link r5js.ProcCall},
          yet all of the tests pass. This suggests either test coverage
          is insufficient or that I don't understand the type of subtype. */
      this.consequent_.clearEnv();
    } else {
      this.consequentLastContinuable_.setNext(
          /** @type {!ProcCallLike} */(this.getNext()));
      this.consequentLastContinuable_.setResultName(this.getResultName());
      resultStruct.setNext(this.consequent_);
      /* We must clear the environment off the non-taken branch.
           See comment at {@link r5js.Continuation.rememberEnv}, and above. */
      this.alternate_.clearEnv();
    }
  }
}

exports = Branch;
