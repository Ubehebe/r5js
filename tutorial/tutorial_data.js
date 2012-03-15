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

var theSecretOfLisp = " _    ___ ___ _____ ___     _   ___ ___ \n| |  |_ _/ __|_   _/ __|   /_\\ | _ \\ __|\n| |__ | |\\__ \\ | | \\__ \\  / _ \\|   / _| \n|____|___|___/ |_| |___/ /_/ \\_\\_|_\\___|\n                                        \n ___ ___  ___   ___ ___    _   __  __ ___ \n| _ \\ _ \\/ _ \\ / __| _ \\  /_\\ |  \\/  / __|\n|  _/   / (_) | (_ |   / / _ \\| |\\/| \\__ \\\n|_| |_|_\\\\___/ \\___|_|_\\/_/ \\_\\_|  |_|___/\n                                          \n";

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
    'Welcome to Gay Lisp! To get started, please type your name in double quotes, like this: "Cordelia"',
    function(input) {
        if (GayLisp.eval('(string? ' + input + ')') === '#t') {
            var name = (GayLisp.eval(input).substr(0, input.length-1));
            tutorial.setErrorMessage("Sorry " + name + ", that's not quite what I had in mind.")
                .setGoodbye("Congratulations, " + name + "! You now know pretty much all the syntax of Lisp."
            ).addStep(
                "Hey, " + name + ", welcome! Lisp is so much fun. You're going to love it. "
                + "We've already covered how to enter strings -- you just put them between double quotes. "
                + "Numbers are pretty easy too. Type 10 to get the number 10. ",
                function(input) { return GayLisp.eval(input) === '10'; }
            ).addStep(
                "Negative numbers, decimals, exponents, different bases -- no problem. "
                + "Type 1e6 for a shorter way to get a million. "
                + "(It's short for 1 x 10^6, 1 times 10 raised to the sixth power.)",
                function(input) { return input === '1e6'; }
                )
            .addStep(
                "It's easy to make lists out of things. "
                    + "If you want to make a list that looks like (blah1 blah2), just type (list blah1 blah2). "
                    + "For example, type (list -10 3 \"hello\") to get the list (-10 3 \"hello\").",
                function(input) {
                    return GayLisp.eval("(equal? " + input + "'(-10 3 \"hello\"))") === '#t';
                }
            ).addStep(
                "As you can see, the things in the list don't have to be all the same kind. "
                + "They can be anything. Numbers, strings...even other lists. "
                + "Can you make a list whose first element is the number 75, and whose second element is the list (1 2)?",
                function(input) {
                    return GayLisp.eval("(equal? " + input + "'(75 (1 2)))") === '#t';
                },
                "Awesome, " + name + "! That was tricky. "
            ).addStep(
                "If you typed (list 45 60 \"Goneril\" \"Regan\"), how many elements would there be in the list?",
                function(input) {
                    return GayLisp.eval(input) === '4';
                }
            ).addStep(
                "And how many elements were in that tricky list -- the one you created by typing (list 75 (list 1 2))?",
                function(input) {
                    return GayLisp.eval(input) === '2';
                },
                "Exactly! The big list is made up of two elements: the number 75 and the list (1 2). "
            ).addStep(
                "Here's a challenge for you, " + name + ". Can you think of a way to make a list with zero elements?",
                function(input) {
                    return GayLisp.eval('(null? ' + input + ')') === '#t';
                }
            ).addStep(
                "Numbers, strings, lists -- we've covered the most important kinds of things in Lisp. "
                + "Lists are the most important, because they can contain anything. "
                    + "And this brings us to the SECRET OF LISP, the one thing that sets it apart from almost every other language on the planet. "
                    + name + ", are you ready to learn the SECRET OF LISP? (Type \"yes\", with the quotes.)",
                function(input) { return GayLisp.eval('(string=? "yes" ' + input + ')') === '#t'; },
                theSecretOfLisp
            );
            return true;
        } else return false;
    }, '');