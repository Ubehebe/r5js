function testScanner() {

    // todo bl add negative tests (properly raises errors)

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

    var validTokens = {
        'identifier': ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '~', '_', '^', '&+', 'h+...@@@-.'],
        'character': ['#\\c', '#\\space', '#\\newline', '#\\\\'],
        'string': ['""', '"hello, world"', '" \\" "', '"\\\\"'],
        'boolean': ['#t', '#f', '#T', '#F']
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
    console.log('testScanner: ' + numTests + ' tests, ' + numErrors + ' errors');
}

function testParser() {

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
        '(IF x y)': true,
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

                    else ; // ...got the correct parse, do nothing
                }

                else { // ...but unexpectedly got failure
                    ++numErrors;
                    console.log('testParser ' + type + ': ' + toParse + ': expected success, got failure');
                }

            }

            // Expected failure...
            else {
                if (!actualResult || actualResult.peekParse() !== type)
                    ; // ...and got failure, according to expectation
                else { // ...but unexpectedly got success
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

function testEvaluator() {
    /* todo bl: migrate to self-hosting Scheme when the interpreter is
     mature enough */
    var tests = {};
    tests['sanity-checks'] = {
        "()": false,
        "(1 1)": false,
        "(1 . 1)": false,
        "(cdr (x . y))": false,
        "(car '())": false,
        "(cdr '())": false,
        "(cadr '((1 2) . 3))": false,
        "let": false,
        "`(1 2 ,@(+ 3 4))": false,
        '(let ())': false,
        "(let (x ()) 1)": false,
    };

    /* These tests exercise various macro features that the standard talks about
     but doesn't give actual examples of. */
    tests['macros'] = {
        "(define-syntax x (syntax-rules () ((x) 'macro))) (define x 'procedure-call) (x)": false,
        '(define-syntax foo (syntax-rules () ((foo x) x))) (foo (1 2))': false,
        /* todo bl: i'm not sure why this one isn't supposed to work,
         but it doesn't in PLT Scheme and it doesn't in my implementation. */
        "(define-syntax foo (syntax-rules (x) ((foo x) 'literal-id))) (define (bar x) (foo x)) (bar 32)": false,
        "(define-syntax foo (syntax-rules () ((foo (x . y)) y))) (foo (1 2))": false, // tricky!
        "(define-syntax foo (syntax-rules () ((foo (x)) x))) (foo 2)": false,
        "(define-syntax foo (syntax-rules () ((foo (x y ...) (z w ...)) (+ y ... w ...)))) (foo () ())": false,
        "(define-syntax foo (syntax-rules () ((foo) x))) ((lambda (x) (foo)) 2)": false,

        /* Proper list and vector patterns can match only proper list
        and vector inputs respectively, but dotted list patterns can match
        proper and dotted list inputs. */
        "(define-syntax foo (syntax-rules () ((foo (x ...)) 'ok))) (foo ())": 'ok',
        "(define-syntax foo (syntax-rules () ((foo (x ...)) 'ok))) (foo (1 . 2))": false,
        "(define-syntax foo (syntax-rules () ((foo (x ...)) 'ok))) (foo #())": false,
        "(define-syntax foo (syntax-rules () ((foo #(x ...)) 'ok))) (foo #())": 'ok',
        "(define-syntax foo (syntax-rules () ((foo #(x ...)) 'ok))) (foo ())": false,
        "(define-syntax foo (syntax-rules () ((foo #(x ...)) 'ok))) (foo (1 . 2))": false,
        "(define-syntax foo (syntax-rules () ((foo (x . y)) 'ok))) (foo (a b))": 'ok',
        "(define-syntax foo (syntax-rules () ((foo (x . y)) 'ok))) (foo (a . b))": 'ok',
        "(define-syntax foo (syntax-rules () ((foo (x . y)) 'ok))) (foo #())": false
    };

    // R5RS 6.4
    tests['control-features'] = {
        "(apply apply '(+ (3 4 5)))": false, // tricky!
        "(eval '+ (null-environment 5))": false, // tricky!
        "(eval () (null-environment 5)": false,
        "(eval '() (null-environment 5))": false,
        "(eval '(()) (null-environment 5))": false,
        "(eval (()) (null-environment 5))": false,
    };

    tests['syntax-rebinding'] = {
        "(define x let)": false,
        "(define x define)": false,
        "(let ((x let*)) 1)": false,
        "(let ((x let)) 1)": false,
    };

    var numErrors = 0;
    var numTests = 0;

    for (var type in tests) {
        var testsOfType = tests[type];
        for (var input in testsOfType) {
            var expectedOutput = testsOfType[input];
            try {
                /* Running each test twice is a good sanity check for certain
                 kinds of bugs, for example clone failures.
                 var actualOutput = R5JS.eval(input + ' ' + input); */
                var actualOutput = R5JS.eval(input);
                if (expectedOutput !== actualOutput) {
                    ++numErrors;
                    console.log('testEvaluator '
                        + type
                        + ': '
                        + input
                        + ': expected '
                        + (expectedOutput === false ? 'error' : expectedOutput)
                        + ', got '
                        + actualOutput);
                }
            } catch (x) {
                // Got an evaluation error, but that's ok because we expected an error.
                if (expectedOutput === false)
                    ;
                else {
                    console.log('testEvaluator '
                        + type
                        + ': exception evaluating '
                        + input
                        + ': '
                        + x);
                    ++numErrors;
                }
            }
            ++numTests;
        }
    }
    console.log('testEvaluator: ' + numTests + ' tests, ' + numErrors + ' errors');
}