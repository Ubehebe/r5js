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

Tutorial.prototype.eval = function(input) {
    var curStep = this.getCurStep();
    if (curStep) {
        if (curStep.inputOk(input)) {
            var feedback = curStep.customFeedback
                || '\n' + this.getArbitraryFeedback() + '\n\n';
            var nextStep = this.advanceToNextStep();
            return nextStep
                ? feedback + nextStep.prompt + '\n'
                : feedback + '\n' + this.goodbye;
        } else {
            return this.errorMessage + '\n';
        }
    } else return GayLisp.repl(input);
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

var tutorial = new Tutorial()
    .setErrorMessage("Sorry, that's not quite what I had in mind.")
    .setVapidFeedbackMessages([
    'Great.',
    'OK.',
    'Good.',
    'Good job.',
    'Keep up the good work.',
    "That's right."]
    ).addStep(
    'Welcome to Gay Lisp!\n'
    + 'To get started, please type your name in double quotes, like this:\n'
    + '"Cordelia"',
    function(input) {
        if (GayLisp.eval('(string? ' + input + ')') === '#t') {
            var name = (GayLisp.eval(input).substr(0, input.length-1));
            tutorial.setErrorMessage("Sorry " + name + ", that's not quite what I had in mind.")
                .setGoodbye("Congratulations, " + name + "!\n"
            + "You now know pretty much all the syntax of Lisp."
            ).addStep(
                "Hey, " + name + ", welcome!\n"
            + "Lisp is so much fun. You're going to love it.\n"
                + "We've already covered how to enter strings -- you just put them between double quotes.\n"
                + "Numbers are pretty easy too. Type 10 to get the number 10.\n",
                function(input) { return GayLisp.eval(input) === '10'; }
            ).addStep(
                "Negative numbers, decimals, exponents, different bases -- no problem.\n"
                + "Type 1e6 for a shorter way to get a million.\n"
                + "(It's short for 1 x 10^6, 1 times 10 raised to the sixth power.)",
                function(input) { return input === '1e6'; }
                )
            .addStep(
                "It's easy to make lists out of things.\n"
                    + "If you want to make a list that looks like (blah1 blah2),\n"
                    + "just type (list blah1 blah2).\n"
                    + "For example, type (list -10 3 \"hello\") to get the list (-10 3 \"hello\").\n",
                function(input) {
                    return GayLisp.eval("(equal? " + input + "'(-10 3 \"hello\"))") === '#t';
                }
            ).addStep(
                "As you can see, the things in the list don't have to be all the same kind.\n"
                + "They can be anything. Numbers, strings...\n"
                + "\n...other lists...\n\n"
                + "Can you make a list whose first element is the number 75,\n"
                + "and whose second element is the list (1 2)?",
                function(input) {
                    return GayLisp.eval("(equal? " + input + "'(75 (1 2)))") === '#t';
                },
                "\nAwesome, " + name + "! That was tricky.\n"
            ).addStep(
                "If you typed (list 45 60 \"Goneril\" \"Regan\"), how many\n"
                + "elements would there be in the list?",
                function(input) {
                    return GayLisp.eval(input) === '4';
                }
            ).addStep(
                "And how many elements were in that tricky list --\n"
                + "the one you created by typing (list 75 (list 1 2))?",
                function(input) {
                    return GayLisp.eval(input) === '2';
                },
                "\nExactly! The big list is made up of two elements:\n"
                    + "the number 75 and the list (1 2).\n"
            ).addStep(
                "Here's a challenge for you, " + name + ".\n"
                + "Can you think of a way to make a list with zero elements?\n",
                function(input) {
                    return GayLisp.eval('(null? ' + input + ')') === '#t';
                }
            ).addStep(
                "Let's talk about how to get things out of lists.\n"
                + "To get the first thing out of a list L, you type (list-ref L 0).\n"
                + "To get the second thing out of L, you type (list-ref L 1).\n"
                + "(Computers usually start counting from 0 instead of 1. Sorry!)\n"
                + "I've just made a list X that looks like this: (4 6 (1 2) 3).\n"
                + "Can you get the second thing out of X?",
                function(input) {
                    return input.indexOf("X") !== -1
                    && GayLisp.eval("(define X '(4 6 (1 2) 3))" + input) === '6';
                }
            ).addStep(
                "Numbers, strings, lists -- we've already covered\n"
                + "the most important kinds of things in Lisp.\n"
                + "Lists are the most important, because they can contain anything.\n\n"
                    + "And this brings us to the SECRET OF LISP, the one thing that\n"
                + "sets it apart from almost every other language on the planet.\n"
                    + name + ", are you ready to learn the SECRET OF LISP?\n"
                + "(Type \"yes\".)",
                function(input) { return GayLisp.eval(input) === 'yes'; },
                '\n\n'
            ).addStep(
                "THE PROGRAM IS A LIST\n"
            + "THE PROGRAM IS A LIST\n"
            + "THE PROGRAM IS A LIST\n"
            + "THE PROGRAM IS A LIST\n"
            + "THE PROGRAM IS A LIST\n"
            + "THE PROGRAM IS A LIST",
                function() { return true; },
                '\n\n'
            );
            return true;
        } else return false;
    }, '\n');

