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


goog.provide('r5js.tmp.unit_test');

function testScanner() {

    if (!Function('return "console" in this;')())
            alert('The unit tests use the console for display, but your JavaScript environment doesn\'t have a console.');

    function assertValidToken(text, type) {
        var tokens = new Scanner(text).tokenize();
        if (tokens.length !== 1) {
            console.error('failed to scan token ' + text + ': expected 1 token, got ' + tokens.length);
            return false;
        } else if (tokens[0].type !== type) {
            console.error('failed to scan token ' + text + ': expected type ' + type + ', got ' + tokens[0].type);
            return false;
        } else return true;
    }

    function assertInvalidToken(text) {
        try {
            new Scanner(text).tokenize();
            console.error('error: successfully scanned input ' + text + ', but it is supposed to be rejected');
            return false;
        } catch (e) {
            return true;
        }
    }

    var validTokens = {
        'identifier': ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^', '&+', 'h+...@@@-.'],
        'character': ['#\\c', '#\\space', '#\\newline', '#\\\\'],
        'string': ['""', '"hello, world"', '" \\" "', '"\\\\"'],
        'boolean': ['#t', '#f', '#T', '#F']
    };

    var invalidTokens = {
        'identifier': ['|', '[', ']', '{', '}'],
        'character': [/* todo bl */],
        'string': ['"', "\\"],
        'boolean': ['##f', '#', '#'],
        'number': ['1+2']
    };

    validTokens['number'] = (function() {

        var bases = ['', '#b', '#B', '#o', '#O', '#d', '#D', '#x', '#X'];
        var exactnesses = ['', '#e', '#E', '#i', '#I'];

        var prefixes = [];
        for (var i = 0; i < bases.length; ++i) {
            for (var j = 0; j < exactnesses.length; ++j) {
                prefixes.push(bases[i] + exactnesses[j])
                prefixes.push(exactnesses[j] + bases[i]);
            }
        }

        var exponentMarkers = ['e', 's', 'f', 'd', 'l', 'E', 'S', 'F', 'D', 'L'];
        var signs = ['', '+', '-'];

        var suffixes = [''];
        for (var i = 0; i < exponentMarkers.length; ++i)
            for (var j = 0; j < signs.length; ++j)
                suffixes.push(exponentMarkers[i] + signs[j] + "2387");

        var decimals = ["8762",
            "-3",
            "4987566###",
            ".765",
            ".549867#",
            "0.",
            "37.###",
            "565.54",
            "3765.4499##",
            "4##.",
            "56#.",
            "587##.#"];

        var ans = [];
        for (var i = 0; i < decimals.length; ++i)
            for (var j = 0; j < suffixes.length; ++j)
                ans.push(decimals[i] + suffixes[j]);

        return ans;
    })();

    var numErrors = 0;
    var numTests = 0;
    for (var type in validTokens) {
        validTokens[type].forEach(function(text) {
            if (!assertValidToken(text, type))
                ++numErrors;
            ++numTests;
        });
    }

    for (var type in invalidTokens) {
        invalidTokens[type].forEach(function(text) {
            if (!assertInvalidToken(text))
                ++numErrors;
            ++numTests;
        });
    }

    console.log('testScanner: ' + numTests + ' tests, ' + numErrors + ' errors');
}

function testParser() {

    if (!Function('return "console" in this;')())
            alert('The unit tests use the console for display, but your JavaScript environment doesn\'t have a console.');

    // todo bl add lots of unit tests focusing on headless clauses (sequence, body)


    var tests = {};

    tests['variable'] = {
        '...': true,
        '+': true,
        '-': true,
        'x': true,
        '(': false
    };

    tests['quotation'] = {
        "'1": true,
        "''1": true,
        '(quote quote)': true,
        "'()": true,
        '(quote ())': true,
        "'quote": true,
        'quote': false,
        "''": false
    };

    tests['self-evaluating'] = {
        '#t': true,
        '1': true,
        '#\\a': true,
        '#\\space': true,
        '3.14159': true,
        '"hello, world"': true,
        '"(define foo x y)"': true,
        '(define foo (+ 1 2))': false,
        '+': false
    };

    tests['procedure-call'] = {
        '(+)': true,
        '+': false,
        '(foo x': false,
        'foo x)': false,
        '()': false,
        '(define x)': true,
        '(foo x y . z)': false,
        '((foo) (foo))': true,
        '((define) foo)': true,
        /* todo bl parses as a macro use '((define) define)': false, */
        '((lambda () +) 1 2)': true
    };

    tests['lambda-expression'] = {
        '(lambda () 1)': true,
        '(lambda x 1)': true,
        '(lambda (x) y z)': true,
        '(lambda (x y) (x y))': true,
        '(lambda (x y))': false,
        '(lambda (x . y) z)': true,
        '(lambda x . y z)': false,
        '(lambda lambda)': false,
        '(lambda () (define x 1) (define y 2))': false,
        '(lambda () (define x 1) (define y 2) x)': true,
        '(lambda () (define x 1) (define y 2) x y)': true
    };

    tests['formals'] = {
        '(x y z)': true,
        'x': true,
        '(x . z)': true,
        '( . x)': false,
        '(x . y . z)': false
    };

    tests['definition'] = {
        '(define x x)': true,
        '(define (foo x y) (foo x y))': true,
        '(begin (define x x) (define y y))': true,
        '(define (x . y) 1)': true,
        'define': false,
        '(define)': false,
        '(define x)': false,
        '(begin 1)': false,
        '(begin ())': false,
        '(begin)': true,
        '(define (x) (define y 1) x)': true,
        '(begin (define x 1) (define y 2))': true
    };

    tests['conditional'] = {
        '(if x y z)': true,
        '(if x y)': true,
        '(if x)': false,
        '(if)': false,
        'if': false,
        // case sensitivity enabled for JS interop '(IF x y)': true,
        '(if x (define x 1))': true
    };

    tests['assignment'] = {
        '(set! let! met!)': true,
        '(set!)': false,
        '(set! set!)': false,
        '(set! x)': false
    };

    tests['transformer-spec'] = {
        '(syntax-rules ())': true,
        '(syntax-rules)': false
    };

    tests['pattern-identifier'] = {
        'define': true,
        '...': false,
        'x': true
    };

    tests['pattern'] = {
        '()': true,
        '(define)': true,
        '(define ...)': true,
        '(define . define)': true,
        '(define . ...)': false,
        '(...)': false,
        '#()': true,
        '#(define ...)': true
    };

    tests['pattern-datum'] = {
        'x': false,
        '"x"': true,
        "'x": false
    };

    tests['template'] = {
        '()': true,
        '#()': true,
        '(x...)': true,
        '(x... . x)': true,
        '(x... y...)': true
    };

    tests['quasiquotation'] = {
      "`(list ,(+ 1 2) 4)": true,
        "`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)": true,
        "(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)": false,
        "`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))": true,
        "`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)": true,
        "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)": true
    };

    tests['splicing-unquotation'] = {
        ",@(cdr '(c))": true,
        "(unquote-splicing (cdr '(c)))": true,
        ",@": false,
        "unquote-splicing": false
    };

    tests['macro-block'] = {
        "(let-syntax () 1)": true,
        "(let-syntax ())": false,
        "(letrec-syntax () 1)": true,
        "(letrec-syntax ())": false,
        "(let-syntax ((foo (syntax-rules () ((foo x) 'x)))) 1)": true,
        "(letrec-syntax ((foo (syntax-rules (x) ((foo x) 'x)))) (foo))": true,
        "(let-syntax ((foo (syntax-rules () ((foo) (+ 1 2 3))))) (define x 12) x)": true
    };

    var numErrors = 0;
    var numTests = 0;

    for (var type in tests) {
        var testsForType = tests[type];
        for (var toParse in testsForType) {
            var datumRoot = new Reader(new Scanner(toParse)).read();
            var actualResult = (datumRoot instanceof Datum) && new Parser(datumRoot).rhs({type: type});
            var expectedResult = testsForType[toParse];

            // Expected success...
            if (expectedResult) {

                if (actualResult) { // ...got some kind of success...

                    if (actualResult.peekParse() !== type) { // ...but it was an incorrect parse
                        ++numErrors;
                        console.log('testParser ' + type + ': ' + toParse + ': mis-parsed as');
                        console.log(actualResult);
                    }

                    else {} // ...got the correct parse, do nothing
                }

                else { // ...but unexpectedly got failure
                    ++numErrors;
                    console.log('testParser ' + type + ': ' + toParse + ': expected success, got failure');
                }

            }

            // Expected failure...
            else {
                if (!actualResult || actualResult.peekParse() !== type) {
                    ; // ...and got failure, according to expectation
                } else { // ...but unexpectedly got success
                    ++numErrors;
                    console.log('testParser ' + type + ': ' + toParse + ': expected failure, got');
                    console.log(actualResult);
                }
            }
            ++numTests;
        }
    }
    console.log('testParser: ' + numTests + ' tests, ' + numErrors + ' errors');
}