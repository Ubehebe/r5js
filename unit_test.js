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
        '=>': false,
        'cond': false,
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
        '(define x)': false,
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
        '(define define 1)': false,
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

    /*tests['derived-expression'] = {
     '(cond (else #t))': true,
     '(cond (else (define x 1))': false,
     '(cond (else 1 2 3))': true,
     '(case)': false,
     '(case x (else 1 2 3))': true,
     '(and)': true,
     '(or)': true,
     '(let () x)': true,
     '(let () (define x 1))': false,
     '(let x () 1 2 3))': true,
     '(let x () (define x 1))': false,
     '(begin)': false,
     '(do () (#t))': true
     };*/

    tests['cond-clause'] = {
        '(1 2 3)': true,
        '(1)': true,
        '(x => (x x x))': true,
        '1': false,
        '(x => =>)': false
    };

    tests['case-clause'] = {
        "(('1 '1 '()) 1)": true,
        '(() 1)': true,
        '((x y))': false
    };

    tests['binding-spec'] = {
        '(x 1)': true,
        '(1 x)': false,
        '(define x)': false,
        '(x define)': false,
        '(x (define))': true
    };

    tests['iteration-spec'] = {
        '(x 1 1)': true,
        '(1 x 1)': false,
        '(x 1)': true,
        '(1 x)': false
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
                if (!actualResult)
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
        '(+ 1 1)': '2',
        '(define (foo x y) (+ x (* 2 y))) (foo 3 4)': '11',
        '(define (foo) "hi") (define bar (foo)) bar': '"hi"',
        '(define (foo x . y) y) (foo 3 4 5)': "(4 5)",
        "(define (foo x) (* x x)) (+ (foo 3) (foo 4))": '25',
        "(define (fac n) (if (= n 0) 1 (* n (fac (- n 1))))) (fac 10)": '3628800',
        '(((lambda (x) x) (lambda (y) y)) "hello!")': '"hello!"',
        "(define x 1) (define y 2) (+ x y)": '3',
        "(define x 1) (define y 1) (set! x (+ x 100)) (set! y (+ x 100)) (+ x y)": '302',
        "((lambda x x) 32)": '(32)',
        "(((lambda (x) +) 3) 100 1)": '101',
        "(((lambda (x) (lambda (y) (/ x y))) 10) 4)": '2.5',
        "(define (div-me x) (lambda (y) (/ x y))) ((div-me 10) 4)": '2.5',
        "((lambda (x) ((lambda (y) ((lambda (z) (+ x y z z z)) 3)) 2)) 1)": '12',
        "(string? (make-string 0))": '#t',
        "(= 4 (string-length (make-string 4)))": '#t',
        '(string-ref "hello!" 4)': '#\\o'
    };

    tests['macros'] = {
        "(define foo (lambda () 'procedure)) (define-syntax foo (syntax-rules () ((foo) 'macro))) (foo)": 'macro',
        "(define-syntax foo (syntax-rules () ((foo) 'macro))) (define foo (lambda () 'procedure)) (foo)": 'procedure',
        "(define (foo) 'procedure) (define-syntax foo (syntax-rules () ((foo) 'macro))) (foo)": 'macro',
        "(define-syntax foo (syntax-rules () ((foo) 'macro))) (define (foo) 'procedure) (foo)": 'procedure',
        "(define-syntax x (syntax-rules () ((x) 'macro))) (define x 'procedure-call) (x)": false,
        "(define-syntax foo (syntax-rules () ((foo y) (+ y y)))) (foo 100)": '200',
        "(define-syntax foo (syntax-rules () ((foo x) 'nonliteral-id))) (foo foo)": 'nonliteral-id',
        "(define-syntax foo (syntax-rules (x) ((foo x) 'literal-id))) (foo x)": 'literal-id',
        "(define x 1) (define-syntax foo (syntax-rules (x) ((foo x) 'literal-id))) (foo x)": 'literal-id',
        /* todo bl: i'm not sure why this one isn't supposed to work,
            but it doesn't in PLT Scheme and it doesn't in my implementation. */
        "(define-syntax foo (syntax-rules (x) ((foo x) 'literal-id))) (define (bar x) (foo x)) (bar 32)": false,
        '(define-syntax foo (syntax-rules () ((foo x) "hi"))) (foo (1 2))': '"hi"',
        '(define-syntax foo (syntax-rules () ((foo x) x))) (foo "hi")': '"hi"',
        '(define-syntax foo (syntax-rules () ((foo x) x))) (foo (1 2))': false,
        "(define-syntax foo (syntax-rules () ((foo x) x))) (foo '(1 2))": "(1 2)",
        "(define-syntax foo (syntax-rules () ((foo x y) (+ x y)))) (foo 3 4)": '7',
        "(define x 1) (define-syntax foo (syntax-rules () ((foo) x))) ((lambda (x) (foo)) 2)": '1',
        "(define-syntax foo (syntax-rules () ((foo) x))) ((lambda (x) (foo)) 2)": false,
        "(define x 1) (define-syntax foo (syntax-rules () ((foo) x))) (define (bar x) (+ x (foo))) (bar 2)": '3',
        "(define x 1) (define-syntax foo (syntax-rules () ((foo) x))) (define (bar x) (+ (foo) x)) (bar 2)": '3',
        "(define x 1) (define-syntax foo (syntax-rules () ((foo) x))) (define (bar x) (+ x (foo) x)) (bar 2)": '5',
        "(define x 1) (define-syntax foo (syntax-rules () ((foo) x))) (define (bar x) (+ (foo) x (foo))) (bar 2)": '4'
    };

    // R5RS 6.4
    tests['control-features'] = {
        "(apply + '(1 2 3))": '6',
        "(procedure? procedure?)": "#t",
        "(procedure? +)": "#t",
        "(procedure? (lambda () 1))": "#t",
        "(procedure? 2)": "#f",
        "(define (list . xs) xs) (apply apply (list + (list 3 4 5)))": "12", // todo bl: hard!
        '(define (foo x) (x 3.14)) (call-with-current-continuation foo)': '3.14',
        "(call-with-values (lambda () (values '(1 2 3))) cdr)": '(2 3)'
    };

    tests['r5rs-examples'] = {
        "(eqv? 'a 'a)": '#t', // p. 18
        "(eqv? 'a 'b)": '#f', // p. 18
        "(eqv? 2 2)": '#t', // p. 18
        "(eqv? '() '())": '#t', // p. 18
        "(eqv? 100000000 100000000)": '#t', // p. 18
        "(eqv? (cons 1 2) (cons 1 2))": '#f', // p. 18
        "(eqv? (lambda () 1) (lambda () 2))": '#f', // p. 18
        "(eqv? #f 'nil)": '#f', // p. 18
        // todo bl "(let ((p (lambda (x) x))) (eqv? p p))": '#t', // p. 18
        "(pair? '(a . b))": '#t', // p. 26
        "(pair? '(a b c))": '#t', // p. 26
        "(pair? '())": '#f', // p. 26
        "(pair? '#(a b))": '#f', // p. 26

        /* todo bl: these tests test against the external representation
            of lists, which is sensitive to whitespace. A better idea would be
            to see if the result is equivalent to the expected result via a
            standard equivalence predicate, rather than directly comparing
            the actual and expected result strings. */
        "(cons 'a '())": '(a)', // p. 26
        "(cons '(a) '(b c d))": '((a) b c d)', // p. 26
        "(cons \"a\" '(b c))": '("a" b c)', // p. 26
        "(cons 'a 3)": '(a . 3)', // p. 26
        "(cons '(a b) 'c)": '((a b) . c)', // p. 26
        "(car '(a b c))": 'a', // p. 26
        "(car '((a) b c d))": '(a)', // p. 26
        "(car '(1 . 2))": '1', // p. 26
        "(car '())": false, // p. 26
        "(cdr '((a) b c d))": '(b c d)', // p. 26
        "(cdr '(1 . 2))": '2', // p. 26
        "(cdr '())": false, // p. 26
        "(symbol? 'foo)": '#t', // p. 28
        "(symbol? (car '(a b)))": '#t', // p. 28
        '(symbol? "bar")': '#f', // p. 28
        "(symbol? 'nil)": '#t', // p. 28
        "(symbol? '())": '#f', // p. 28
        "(symbol? #f)": '#f', // p. 28
        /* R5RS 6.3.3: "The following examples assume that the implementation's
            standard case is lower case." */
        "(symbol->string 'flying-fish)": '"flying-fish"', // p. 28
        "(symbol->string 'Martin)": '"martin"', // p. 28
        '(symbol->string (string->symbol "Malvina"))': '"Malvina"', // p. 28
        "(eq? 'mISSISSIppi 'mississippi)": '#t',
        "(eq? 'bitBlt (string->symbol \"bitBlt\"))": '#f',
        "(eq? 'JollyWog (string->symbol (symbol->string 'JollyWog)))": '#t',
        // todo bl '(string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D.")))': '#t',
        "(char<? #\\A #\\B)": '#t', // p. 29 todo bl ugh backslash escaping
        "(char<? #\\a #\\b)": '#t', // p. 29
        "(char<? #\\0 #\\9)": '#t', // p. 29
        "'#(0 (2 2 2 2) \"Anna\")": '#(0 (2 2 2 2) "Anna")', // p. 31
        "(vector-ref '#(1 1 2 3 5 8 13 21) 5)": '8', // p. 31
        // todo bl "(vector-ref '#(1 1 2 3 5 8 13 21) (let ((i (round (* 2 (acos -1))))) (if (inexact? i) (inexact->exact i) i)))": "13", // p. 31
        // todo bl "let ((vec (vector 0 '(2 2 2 2) \"Anna\"))) (vector-set! vec 1 '(\"Sue\" \"Sue\")) vec)": '#(0 (\"Sue" "Sue") "Anna")', // p. 31
        "(procedure? car)": '#t', // p. 31
        "(procedure? 'car)": '#f', // p. 31
        "(procedure? (lambda (x) (* x x)))": '#t', // p. 31
        "(procedure? '(lambda (x) (* x x)))": '#f', // p. 31
        "(call-with-current-continuation procedure?)": '#t', // p. 31
        // todo bl "(apply + (list 3 4))": '7', // p. 32
        "(define compose (lambda (f g) (lambda args (f (apply g args))))) ((compose sqrt *) 12 75)": '30', // p. 32
        "(call-with-values (lambda () (values 4 5)) (lambda (a b) b))": '5', // p. 34
        "(call-with-values * -)": '-1', // p. 34
        "(eval '(* 7 3) (scheme-report-environment 5))": '21' // p. 35
        // todo bl "(let ((f (eval '(lambda (f x) (f x x)) (null-environment 5)))) (f + 10))": '20' // p. 35
    };

    var numErrors = 0;
    var numTests = 0;

    for (var type in tests) {
        var testsOfType = tests[type];
        for (var input in testsOfType) {
            var expectedOutput = testsOfType[input];
            try {
                var actualOutput = doEval(input);
                if (expectedOutput !== actualOutput) {
                    ++numErrors;
                    console.log('testEvaluator '
                        + type
                        + ': '
                        + input
                        + ': expected '
                        + expectedOutput
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