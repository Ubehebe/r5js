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
}

function Step(prompt, advanceWhen, customFeedback) {
    this.prompt = prompt;
    this.advanceWhen = advanceWhen;
    this.customFeedback = customFeedback;
}

Step.prototype.inputOk = function(input) {
    try {
        return this.advanceWhen(input);
    } catch (x) {
        return false;
    }
};

Tutorial.prototype.getIntroMessage = function() {
     return this.steps[0].prompt;
};

Tutorial.prototype.setErrorMessage = function(errorMessage) {
    this.errorMessage = errorMessage;
    return this;
};

Tutorial.prototype.setGoodbye = function(goodbye) {
    this.goodbye = goodbye;
    return this;
};

Tutorial.prototype.addStep = function(prompt, advanceWhen, customFeedback) {
    this.steps.push(new Step(prompt, advanceWhen, customFeedback));
    return this;
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
            if (curStep.customFeedback)
                terminal.println(curStep.customFeedback, false);
            else
                terminal.println(this.getArbitraryFeedback(), true);

            var nextStep = this.advanceToNextStep();

            if (nextStep)
                terminal.println(nextStep.prompt, true);
            else
                terminal.println(this.goodbye, true);
            return ' ';
        } else {
            terminal.println(this.errorMessage, true);
            return ' ';
        }
    } else {
        terminal.popInterpreter();
        return null;
    }
};

Tutorial.prototype.setVapidFeedbackMessages = function(messages) {
    this.vapidFeedbackMessages = messages;
    return this;
};

Tutorial.prototype.getArbitraryFeedback = function() {
    return this.vapidFeedbackMessages[
        Math.floor(
            Math.random() * this.vapidFeedbackMessages.length)];
};



