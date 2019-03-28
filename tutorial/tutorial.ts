/* todo bl: this should all be written in Scheme :)
 The reason I haven't is because I'm not sure how to reconcile
 Scheme's IO primitives, which seem to be synchronous, with JavaScript's
 asynchronous event model. The way IO seems to work in Scheme is
 to have a big event loop that checks for char-ready?, and calls read-char
 if a character is ready. I suppose we could extend the language with a
 primitive procedure "on-event" that wraps up the state of the trampoline in
 a closure, and halts it, so that it can be restarted from a JavaScript callback. */

export class Step {

  congratulate = true;
  pause = 0;

  constructor(
      readonly questionArray: any[],
      private readonly advanceWhen: (input: string) => boolean,
      readonly explanationArray: any[] = [],
  ) {}

  disableRandomCongrat(): this {
    this.congratulate = false;
    return this;
  }

  pauseFor(ms: number): this {
    this.pause = ms;
    return this;
  }

  inputOk(input: string): boolean {
    try {
      return this.advanceWhen(input);
    } catch (x) {
      return false;
    }
  }
}

export class Tutorial {
  private readonly steps: Step[] = [];
  private curStep = 0;
  private localVars: {[key: string]: any /* TODO tighten? */} = {};
  private congratulations: ReadonlyArray<string> = [];
  private errorMessageFormatter: (badInput: string) => string = x => x;
  private goodbye: ReadonlyArray<string> = [];

  constructor() {
  }

  getIntroMessage() {
    return this.steps[0].questionArray;
  }

  setErrorMessage(errorMessage: (badInput: string) => string): this {
    this.errorMessageFormatter = errorMessage;
    return this;
  }

  setGoodbye(goodbye: string[]): this {
    this.goodbye = goodbye;
    return this;
  }

  addStep(step: Step): this {
    this.steps.push(step);
    return this;
  }

  setLocalVar(name: string, val: any): this {
    this.localVars[name] = val;
    return this;
  }

  /* This is a convenience method so that a Tutorial can be laid out in a
   straight line without having to indent every time we want to set
   a local variable. Example:

   var foo;
   tut.addStep(
   new Step(
   "enter foo: ",
   function(input) { foo = input; return true; },
   "you entered: " + foo));

   This is wrong; when foo is referenced in the last line, it hasn't been set yet.
   Instead the last line should be:

   tut.withLocalVar("foo", function(foo) { return "you entered: " + foo; })
   */
  withLocalVar(name: string, cb: (x: string) => string /* TODO: generic */): (x: string) => string {
    return () => cb(this.localVars[name] || '');
  }

  getCurStep(): Step | null {
    return this.curStep < this.steps.length
        ? this.steps[this.curStep]
        : null;
  }

  advanceToNextStep(): Step | null {
    this.curStep++;
    return this.getCurStep();
  }

  Eval(input: string, terminal: any /* TODO */) {
    const curStep = this.getCurStep();
    if (curStep) {
      if (curStep.inputOk(input)) {

        if (curStep.congratulate) {
          terminal.println(this.getCongratulation());
        }

        if (curStep.explanationArray) {
          terminal.println(curStep.explanationArray);
        }

        if (curStep.pause) {
          terminal.pause(curStep.pause);
        }

        const nextStep = this.advanceToNextStep();

        terminal.println(nextStep ? nextStep.questionArray : this.goodbye);

        return ' '; // todo bl eliminate
      } else {
        terminal.println(this.errorMessageFormatter);
        return ' '; // todo bl eliminate
      }
    } else {
      terminal.popInterpreter();
      return null;
    }
  }

  setRandomCongrats(messages: ReadonlyArray<string>): this {
    this.congratulations = messages;
    return this;
  }

  getCongratulation(): string {
    return this.congratulations[
        Math.floor(
            Math.random() * this.congratulations.length)];
  }
}
