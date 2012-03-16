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

var theSecretOfLisp = "_    ___ ___ _____ ___ \n| |  |_ _/ __|_   _/ __|\n| |__ | |\\__ \\ | | \\__ \\\n|____|___|___/ |_| |___/\n                        \n   _   ___ ___ \n  /_\\ | _ \\ __|\n / _ \\|   / _| \n/_/ \\_\\_|_\\___|\n               \n ___ ___  ___   ___ ___    _   __  __ ___ \n| _ \\ _ \\/ _ \\ / __| _ \\  /_\\ |  \\/  / __|\n|  _/   / (_) | (_ |   / / _ \\| |\\/| \\__ \\\n|_| |_|_\\\\___/ \\___|_|_\\/_/ \\_\\_|  |_|___/\n                                          \n";

var tutorial = new Tutorial()
    .setErrorMessage("Sorry, that's not quite what I had in mind.")
    .setVapidFeedbackMessages([
    'Great.',
    'OK.',
    'Good.',
    'Good job.',
    "That's right!",
    'Yes.',
    'Keep up the good work.',
    "That's right."]
    ).addStep(
    'Welcome to Gay Lisp! To get started, please type your name in double quotes, like this: "Cordelia"',
    function(input) {
        if (GayLisp.eval('(string? ' + input + ')') === '#t') {
            var name = (GayLisp.eval(input).substr(0, input.length-1));
            tutorial.setErrorMessage("Sorry " + name + ", that's not quite what I had in mind."
            ).addStep([
                "Hey, " + name + ", welcome! Lisp is so much fun. You're going to love it.",
                "We've already covered how to enter strings -- you just put them between double quotes.",
                "Numbers are pretty easy too. Type 10 to get the number 10. "],
                function(input) { return GayLisp.eval(input) === '10'; }
            ).addStep([
                "Negative numbers, decimals, exponents, different bases -- no problem.",
                "Type 1e6 for a shorter way to get a million. (It's short for 1 x 10^6, 1 times 10 raised to the sixth power.)"],
                function(input) { return input === '1e6'; }
                )
            .addStep([
                "It's easy to make lists out of things.",
                    "If you want to make a list that looks like (blah1 blah2), just type (list blah1 blah2).",
                    "For example, type (list -10 3 \"Cornwall\") to get the list (-10 3 \"Cornwall\")."],
                function(input) {
                    return GayLisp.eval("(equal? " + input + "'(-10 3 \"Cornwall\"))") === '#t';
                }
            ).addStep([
                "As you can see, the things in the list don't have to be all the same kind.",
                "They can be anything. Numbers, strings...even other lists.",
                "Can you make a list whose first element is the number 75, and whose second element is the list (1 2)?"],
                function(input) {
                    return GayLisp.eval("(equal? " + input + "'(75 (1 2)))") === '#t';
                },
                "Awesome, " + name + "! That was tricky. "
            ).addStep(
                "If you typed (list 45 60 \"Goneril\" \"Regan\"), how many elements would there be in the list it would create?",
                function(input) {
                    return GayLisp.eval(input) === '4';
                }
            ).addStep(
                "And how many elements were in that tricky list -- the one you created by typing (list 75 (list 1 2))?",
                function(input) {
                    return GayLisp.eval(input) === '2';
                },
                "Exactly! The big list is made up of two elements: the number 75 and the list (1 2)."
            ).addStep([
                "Here's a challenge for you, " + name + ".",
                "Can you think of a way to make a list with zero elements?"],
                function(input) {
                    return GayLisp.eval('(null? ' + input + ')') === '#t';
                }
            ).addStep([
                "Numbers, strings, lists -- we've covered the most important kinds of things in Lisp.",
                "Lists are the most important, because they can contain anything.",
                "And this brings us to the secret of Lisp, the one thing that sets it apart from almost every other language on the planet.",
                name + ", are you ready to learn the secret of Lisp?",
                "(Type \"yes\", with the quotes.)"],
                function (input) {
                    return GayLisp.eval('(string=? "yes" ' + input + ')') === '#t';
                },
                "The secret of Lisp is:\n" + theSecretOfLisp
            ).addStep([
                "What does this mean? It means you tell the computer what to do by typing lists.",
                'For example, when you type',
                '(list "Edmund" "Edgar")',
                'you tell the computer to create a list of two elements, "Edmund" and "Edgar". But the text you type in,',
                "(list \"Edmund\" \"Edgar\")",
                'is itself a list of *three* elements: the command named "list", and the two strings "Edmund" and "Edgar".',
                'When the computer sees a list starting with the "list" command, it knows to create a list out of the remaining elements.',
                'What do you think the computer prints when it sees the list (- 2 100)?'],
                function(input) {
                    return input === '-98';
                },
                "Right. The first element of the list tells the computer to do subtraction, and the other elements are the numbers that get subtracted, in order. So it is equivalent to 2 minus 100."
            ).addStep([
                "This is how almost everything in Lisp works: you type in a list, and the computer uses the first element to decide what to do with the other elements.",
                "For example, to define a variable x to be result of dividing 17 by 3, you type",
                "(define x (/ 17 3))",
                'How would you define bad-guys to be the list ("Albany" "Cornwall")?'],
            function(input) {
                return GayLisp.eval(input + ' (equal? bad-guys (quote ("Albany" "Cornwall")))') === '#t';
            }
            ).addStep([
                "So the main point is that you can use lists to make programs, and you can use programs to make lists.",
                "It's all the same stuff.",
                "In order to become a master Lisp programmer, you have to be able to switch between the two concepts all the time.",
                "This next question is challenging. If you get it, you're well on your way to understanding the secret of Lisp at a deep level.",
                "I want you to type in a program that makes a list whose only element is the command to make a list."
            ],
                function(input) {
                    return GayLisp.eval("(define x " + input + ") (and (list? x) (eq? list (car x)) (null? (cdr x)))") === '#t';
                },
                "Good, " + name + '. The first list command says to make a list, and the second list command is what actually gets put in that list.'
            ).addStep([
                "Once you get used to switching mentally between the two concepts of lists, you might want to have the computer switch between them too.",
                "In other words, whenever you type in a list, the computer treats it as a program, and executes it. But what if you just wanted the computer to treat it as a plain old list?",
                "You can make the computer do that. And it is really easy.",
                "All you do is put a single quotation mark ' in front of the list. Like this: '(1 2 3). The computer sees '(1 2 3) as pure data, not any kind of program. It's just a list.",
                "Use this trick to directly type in the list you made earlier: (-10 3 \"Cornwall\")."
            ],
                function(input) {
                    return input === "'(-10 3 \"Cornwall\")";
                }
            ).addStep([
                "The difference between (list -10 3 \"Cornwall\") and '(-10 3 \"Cornwall\") is that",
                '(list -10 3 \"Cornwall\")',
                "is a program that *creates* a list (-10 3 \"Cornwall\"), while",
                "'(-10 3 \"Cornwall\")",
                "*directly represents* the list (-10 3 \"Cornwall\").",
                "Why do we need that weird single quote? Well, try typing in (-10 3 \"Cornwall\") directly. Something interesting is going to happen."
            ],
                function(input) {
                    if (GayLisp.eval("(define x '" + input + ") (and (= -10 (car x)) (= 3 (cadr x)) (string=? \"Cornwall\" (caddr x)))") === '#t') {
                        try {
                            GayLisp.eval(input);
                            return false;
                        } catch (x) {
                            tutorial.addStep([x,
                                "Oh! Some kind of error. What does it mean?",
                                "Remember what happens when the computer reads a list. The first element is supposed to be a command that tells the computer what to do with the rest of the elements.",
                                "But the first element of (-10 3 \"Cornwall\") isn't a command, it's the number -10. So the computer doesn't know what it's supposed to be doing with the other elements.",
                                "That's why it's complaining. That's why you can't type in lists without single quotes -- unless you want the computer to treat them as programs.",
                                "Think you understand, " + name + "? (There's no hurry. Since I'm living on your computer now, we can spend a lot of time together :))"
                            ],
                                function (input) {
                                    return GayLisp.eval('(string=? "yes" ' + input + ')') === '#t';
                                }, ' '
                            ).addStep([
                                "Here are two final questions to really stretch your mind.",
                                "Earlier, you typed in a program that made an empty list. The program looked like this:",
                                "(list)",
                                "Now, I want you to type in an empty list directly. Don't type a program that makes an empty list. Type something that represents the empty list itself."],
                                function(input) {
                                    return GayLisp.eval("(and (null? " + input + ") (not (eq? 'list (car (quote " + input + ')))))') === '#t';
                                }
                            ).addStep([
                                "Okay, let's put all the pieces together.",
                                "Type in a representation of a list that has four elements.",
                                "The first element is an empty list.", // ()
                                "The second element is a representation of an empty list.", // '()
                                "The third element is a program that will make an empty list.", // (list)
                                "The fourth element is a representation of a program that will make an empty list." // '(list)
                            ],
                                function(input) {
                                    if (GayLisp.eval("(define x " + input + ") (equal? x '(() '() (list) '(list)))") === '#t') {
                                        tutorial.setGoodbye([
                                            "By finishing this tutorial, you have prepared yourself to join an ancient society, one that stretches back before the dawn of time (1970) into the prehistory of our species (1958).",
                                            "Lisp is more than 50 years old, but it continues to flourish and influence cutting-edge technology.",
                                            "The issues raised in this tutorial -- what something is versus what it does, instruction (code) versus representation (data), and self-reference -- are not just syntactic puzzles. They go straight to the terrible heart of computer science.",
                                            "Here are some links you might find pleasurable on your journey. Bon voyage!",
                                            "=> Structure and Interpretation of Computer Programs: http://mitpress.mit.edu/sicp/. This is an introduction to programming using Scheme. It is very, very well written.",
                                            "=> Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I. http://www-formal.stanford.edu/jmc/recursive/recursive.html/. This is the first paper written about Lisp, published in 1960. It's highly readable.",
                                            "=> Paul Graham's Lisp pages. http://paulgraham.com/lisp.html/"
                                        ]);
                                        return true;
                                    } else return false;
                                },
                                "YES!! You did it, " + name +"!"
                            );
                            return true;
                        }
                    } else return false;
                }, ' '
            );
            return true;
        } else return false;
    }, ' ');
