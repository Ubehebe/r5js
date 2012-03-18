/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* todo bl: this should all be written in Scheme :)
 The reason I haven't is because I'm not sure how to reconcile
 Scheme's IO primitives, which seem to be synchronous, with JavaScript's
 asynchronous event model. The way IO seems to work in Scheme is
 to have a big event loop that checks for char-ready?, and calls read-char
 if a character is ready. I suppose we could extend the language with a
 primitive procedure "on-event" that wraps up the state of the trampoline in
 a closure, and halts it, so that it can be restarted from a JavaScript callback. */

function Tutorial() {
    this.steps = [];
    this.curStep = 0;
    this.localVars = {};
}

function Step(questionArray, advanceWhen, explanationArray) {
    this.questionArray = questionArray;
    this.advanceWhen = advanceWhen;
    this.explanationArray = explanationArray;
    this.congratulate = true;
}

Step.prototype.disableRandomCongrat = function() {
    this.congratulate = false;
    return this;
};

Step.prototype.inputOk = function(input) {
    try {
        return this.advanceWhen(input);
    } catch (x) {
        return false;
    }
};

Tutorial.prototype.getIntroMessage = function() {
     return this.steps[0].questionArray;
};

Tutorial.prototype.setErrorMessage = function(errorMessage) {
    this.errorMessage = errorMessage;
    return this;
};

Tutorial.prototype.setGoodbye = function(goodbye) {
    this.goodbye = goodbye;
    return this;
};

Tutorial.prototype.addStep = function(step) {
    this.steps.push(step);
    return this;
};

Tutorial.prototype.setLocalVar = function(name, val) {
    this.localVars[name] = val;
    return this;
};

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
Tutorial.prototype.withLocalVar = function(name, cb) {
    var self = this;
    return function() {
        return cb(self.localVars[name] || '');
    };
};

Tutorial.prototype.getCurStep = function() {
    return this.curStep < this.steps.length
        ? this.steps[this.curStep]
        : null;
};

Tutorial.prototype.advanceToNextStep = function() {
    this.curStep++;
    return this.getCurStep();
};

Tutorial.prototype.eval = function(input, terminal) {
    var curStep = this.getCurStep();
    if (curStep) {
        if (curStep.inputOk(input)) {

            if (curStep.congratulate)
                terminal.println(this.getCongratulation());

            if (curStep.explanationArray)
                terminal.println(curStep.explanationArray);

            var nextStep = this.advanceToNextStep();

            terminal.println(nextStep ? nextStep.questionArray : this.goodbye);

            return ' '; // todo bl eliminate
        } else {
            terminal.println(this.errorMessage);
            return ' '; // todo bl eliminate
        }
    } else {
        terminal.popInterpreter();
        return null;
    }
};

Tutorial.prototype.setRandomCongrats = function(messages) {
    this.congratulations = messages;
    return this;
};

Tutorial.prototype.getCongratulation = function() {
    return this.congratulations[
        Math.floor(
            Math.random() * this.congratulations.length)];
};



