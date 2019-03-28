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

/* It would be nice if the tutorial was in Scheme (see comments at the
 top of the Tutorial class), or even in JSON, but JSON doesn't allow
 function literals, which I need for custom control over the tutorial. */


var tutorial = (function() {

    function evalTrue(input) {
        return GayLisp.Eval(input) === '#t';
    }

    var theSecretOfLisp = " _    ___ ___ _____ ___ \n| |  |_ _/ __|_   _/ __|\n| |__ | |\\__ \\ | | \\__ \\\n|____|___|___/ |_| |___/\n                        \n   _   ___ ___ \n  /_\\ | _ \\ __|\n / _ \\|   / _| \n/_/ \\_\\_|_\\___|\n               \n ___ ___  ___   ___ ___    _   __  __ ___ \n| _ \\ _ \\/ _ \\ / __| _ \\  /_\\ |  \\/  / __|\n|  _/   / (_) | (_ |   / / _ \\| |\\/| \\__ \\\n|_| |_|_\\\\___/ \\___|_|_\\/_/ \\_\\_|  |_|___/\n                                          \n";
    var theSecretOfLispFallback = "Lists are programs.";

    var tut = new Tutorial(
    ).setRandomCongrats([
        'Great.',
        'OK.',
        'Good.',
        'Good job.',
        "That's right!",
        'Yes.',
        'Keep up the good work.',
        "That's right."]
    );

    tut.setErrorMessage(tut.withLocalVar('name', function(name) {return "Sorry " + name +", that's not quite what I had in mind."; })
    ).addStep(new Step([
        'Welcome to Gay Lisp! To get started, please type your name in double quotes, like this: "Cordelia"'],
        function(input) {
            tut.setLocalVar('tutStart', new Date());
            var ans = evalTrue('(string? ' + input + ')');
            if (ans)
                tut.setLocalVar('name', GayLisp.Eval(input));
            return ans;
        },
        [tut.withLocalVar('name', function(name) { return "Hey " + name + ", welcome! Gay Lisp is a program that understands the language Scheme. It's so much fun. You're going to love it.";})]
    ).disableRandomCongrat()
    ).addStep(new Step([
        "We've already covered how to enter strings -- you just put them between double quotes.",
        "Numbers are pretty easy too. Type 10 to get the number 10."],
        function(input) {
            return GayLisp.Eval(input) === '10';
        })
    ).addStep(new Step([
        "Negative numbers, decimals, exponents, different bases -- no problem.",
        "Type 1e6 for a shorter way to get a million. (It's short for 1 x 10^6, 1 times 10 raised to the sixth power.)"],
        function(input) {
            return input === '1e6';
        })
    ).addStep(new Step([
        "It's easy to make lists out of things.",
        "If you want to make a list that looks like (blah1 blah2), just type (list blah1 blah2).",
        "For example, type (list -10 3 \"Cornwall\") to get the list (-10 3 \"Cornwall\")."],
        function(input) {
            return evalTrue("(equal? " + input + "'(-10 3 \"Cornwall\"))");
        })
    ).addStep(new Step([
        "As you can see, the things in the list don't have to be all the same kind.",
        "They can be anything. Numbers, strings...even other lists.",
        "Can you make a list whose first element is the number 75, and whose second element is the list (1 2)?"],
        function(input) {
            return evalTrue("(equal? " + input + "'(75 (1 2)))");
        },
        [tut.withLocalVar('name', function(name) { return "Awesome, " + name + "! That was tricky. ";})]
    ).disableRandomCongrat()
    ).addStep(new Step([
        "If you typed (list 45 60 \"Goneril\" \"Regan\"), how many elements would there be in the list it would create?"],
        function(input) {
            return evalTrue('(= 4 ' + input + ')');
        })
    ).addStep(new Step([
        "And how many elements were in that tricky list -- the one you created by typing (list 75 (list 1 2))?"],
        function(input) {
            return evalTrue('(= 2 ' + input + ')');
        },
        ["Exactly! The big list is made up of two elements: the number 75 and the list (1 2)."]
    ).disableRandomCongrat()
    ).addStep(new Step([
        tut.withLocalVar('name', function(name) { return "Here's a challenge for you, " + name + ".";}),
        "Can you think of a way to make a list with zero elements?"],
        function(input) {
            return evalTrue('(null? ' + input + ')');
        })
    ).addStep(new Step([
        "Numbers, strings, lists -- we've covered the most important kinds of things in Scheme.",
        "Lists are the most important, because they can contain anything.",
        "And this brings us to the secret of Scheme, the thing that sets it apart from almost every other language.",
        tut.withLocalVar('name', function(name) { return name + ", are you ready to learn the secret of Scheme?";}),
        '(Type "yes", with the quotes.)'],
        function(input) {
            return GayLisp.Eval('(string=? "yes" ' + input + ')') === '#t';
        },
        ["The secret of Scheme is:",
            /* This custom object will get passed to a terminal implementation,
             which ought to decide whether to display the banner or the fallback
             based on its own capabilities. There's probably a more OO way
             to do this. */
            new function() {
                this.fancy = theSecretOfLisp;
                this.unfancy  = theSecretOfLispFallback;
                this.bannerToString = function() { return this.fancy; };
                this.toString = function() { return this.unfancy; };
            }]
    ).disableRandomCongrat().pauseFor(5000)
    ).addStep(new Step([
        "That sounds banal, but it really is the secret of Scheme, and the more you use Scheme, the more profound it gets.",
        "Type \"tell me more\" (in quotes) when you're ready to go on. No rush."
    ], function(input) {
        return input ==='"tell me more"';
    },
        ["OK. Lists are programs. What does that mean?",
            "It means you tell the computer what to do by typing lists.",
            'For example, when you type',
            '(list "Gloucester" "Kent")',
            'you tell the computer to create a list of two elements, "Gloucester" and "Kent". But the text you type in is itself a list of *three* elements: the command named "list", and the two strings "Gloucester" and "Kent".',
            'When the computer sees a list starting with the "list" command, it knows to create a list out of the remaining elements.']
    ).disableRandomCongrat()
    ).addStep(new Step([
        "Type \"okay\" (with the quotes) when you're ready to go on."],
        function(input) {
            return input === '"okay"';
        }
    ).disableRandomCongrat()
    ).addStep(new Step(['There\'s nothing special about the "list" command.',
        "Any command you put at the beginning of a list, the computer will try to run it on the other elements.",
        "Here are some commands you probably already know: + - * /.",
        "For example, if you type in",
        "(+ 3 (* 4 5))",
        "the computer will add 3 to the result of multiplying 4 and 5. In other words, 23.",
        'What do you think the computer prints when it sees the list (- 2 100)?'],
        function(input) {
            return evalTrue('(and (= -98 ' + input + ') (number? (quote ' + input + ')))');
        }, ["The first element of the list tells the computer to do subtraction, and the other elements are the numbers that get subtracted, in order. So it means 2 minus 100.",
            "This is how almost everything in Scheme works: you type in a list, and the computer uses the first element to decide what to do with the other elements."])
    ).addStep(new Step([
        "For example, to define a variable x to be the result of dividing 17 by 3, you type",
        "(define x (/ 17 3))",
        'How would you define bad-guys to be the list ("Albany" "Cornwall")?'],
        function(input) {
            return evalTrue(input + ' (equal? bad-guys (quote ("Albany" "Cornwall")))');
        }, ["So the main point is that you can use lists to make programs, and you can use programs to make lists.",
            "It's all the same stuff.",
            "In order to become a master Scheme programmer, you have to be able to switch between the two concepts all the time."])
    ).addStep(new Step([
        "This next question is challenging. If you get it, you're well on your way to understanding the secret of Scheme at a deep level.",
        "I want you to type in a program that makes a list whose only element is the command to make a list."
    ],
        function(input) {
            return evalTrue("(define x " + input + ") (and (list? x) (eq? list (car x)) (null? (cdr x)))");
        },
        [tut.withLocalVar('name', function(name) { return "Good, " + name + ". The first list command says to make a list, and the second list command is what actually gets put in that list.";})])
        .disableRandomCongrat()
    ).addStep(new Step([
        "Once you get used to switching mentally between the two concepts of lists, you might want to have the computer switch between them too.",
        "In other words, whenever you type in a list, the computer treats it as a program, and executes it. But what if you just wanted the computer to treat it as a plain old list?",
        "You can make the computer do that.",
        "All you do is put a single quotation mark ' in front of the list. Like this: '(1 2 3). The computer sees '(1 2 3) as pure data, not any kind of program. It's just a list.",
        "Use this trick to directly type in the list you made earlier: (-10 3 \"Cornwall\")."],
        function(input) {
            return input === "'(-10 3 \"Cornwall\")";
        }, ["The difference between (list -10 3 \"Cornwall\") and '(-10 3 \"Cornwall\") is that",
            '(list -10 3 \"Cornwall\")',
            "is a program that *creates* a list (-10 3 \"Cornwall\"), while",
            "'(-10 3 \"Cornwall\")",
            "*directly represents* the list (-10 3 \"Cornwall\")."])
    ).addStep(new Step([
        "Why do we need that weird single quote? Well, try typing in (-10 3 \"Cornwall\") directly. Something interesting is going to happen."],
        function(input) {
            if (evalTrue("(define x '" + input + ") (and (= -10 (car x)) (= 3 (cadr x)) (string=? \"Cornwall\" (caddr x)))")) {
                try {
                    GayLisp.Eval(input);
                    return false;
                } catch (x) {
                    tut.setLocalVar('list-error', x.toString());
                    return true;
                }
            } else return false;
        }, [tut.withLocalVar('list-error', function(listError) { return listError; }),
            "Oh! Some kind of error. What does it mean?",
            "Remember what happens when the computer reads a list. The first element is supposed to be a command that tells the computer what to do with the rest of the elements.",
            "But the first element of (-10 3 \"Cornwall\") isn't a command, it's the number -10. So the computer doesn't know what it's supposed to be doing with the other elements.",
            "That's why it's complaining. That's why you can't type in lists without single quotes -- unless you want the computer to treat them as programs."]
    ).disableRandomCongrat()
    ).addStep(new Step([tut.withLocalVar('name', function(name) { return "Think you understand, " + name + "? (There's no hurry. Since I'm living on your computer now, we can spend a lot of time together :))";})],
        function(input) {
            return evalTrue('(string=? "yes" ' + input + ')');
        }
    ).disableRandomCongrat()
    ).addStep(new Step(["Here are two final questions to really stretch your mind.",
        "Earlier, you typed in a program that made an empty list. The program looked like this:",
        "(list)",
        "Now, I want you to type in an empty list directly. Don't type a program that makes an empty list. Type something that represents the empty list itself."],
        function(input) {
            return evalTrue("(and (null? " + input + ") (not (eq? 'list (car (quote " + input + ')))))');
        })
    ).addStep(new Step([
        "Let's put all the pieces together.",
        "Type in a representation of a list that has four elements.",
        "The first element is an empty list.", // ()
        "The second element is a representation of an empty list.", // '()
        "The third element is a program that will make an empty list.", // (list)
        "The fourth element is a representation of a program that will make an empty list." // '(list)
    ],
        function(input) {
            return evalTrue("(define x " + input + ") (equal? x '(() '() (list) '(list)))");
        },
        [tut.withLocalVar('name', function(name) { return "YES!! You did it, " + name +"!";})]
    ).disableRandomCongrat()
    ).addStep(new Step([
        "By finishing this tutorial, you have prepared yourself to join an ancient society, one that stretches back before the dawn of time (1970) into the prehistory of our species (1958).",
        "Lisp (the parent language of Scheme) is more than 50 years old, but it continues to flourish and influence cutting-edge technology.",
        "The issues raised in this tutorial -- what something is versus what it does, instruction (code) versus representation (data), and self-reference -- are not just syntactic puzzles. They go straight to the heart of computer science.",
        "(Type \"go on\" when you're ready.)"],
        function(input) {
            if (input === '"go on"') {
                tut.withLocalVar('tutStart', function(tutStart) {
                    _gaq.push(['_trackEvent', 'tutorial', 'done', null, new Date() - tutStart]);
                })();
                tut.setGoodbye([
                    tut.withLocalVar('name', function(name) { return name + ", it's time for you to leave me behind and explore Lisp on your own."; }),
                    "Fortunately, there are tons of good books, articles, and websites about Lisp. Here are a few of the best:",
                    "=> Structure and Interpretation of Computer Programs: http://mitpress.mit.edu/sicp/. This is one of the best books about programming ever written. It uses Scheme.",
                    "=> Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I. http://www-formal.stanford.edu/jmc/recursive/recursive.html/. This is the first paper written about Lisp, published in 1960. It's highly readable.",
                    "=> Paul Graham's Lisp pages. http://paulgraham.com/lisp.html/",
                    "P.S. -- I've dropped you back into the main interpreter. You can type in programs using the full Scheme language. Please try to break as much stuff as you can -- it's the only way to learn."
                ]);
                return true;
            } else return false;
        }).disableRandomCongrat()
    );

    return function(string, terminal) {
        if (string === '(tutorial)') {
            terminal.reset()
                .println(tut.getIntroMessage(), true)
                .popInterpreter();
            terminal.pushInterpreter(function(string, terminal) {
                return tut.Eval(string, terminal);
            });
            return ' ';
        } else return null;
    }
}());


