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
        '(': false};

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
        '((define) define)': false,
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

    tests['derived-expression'] = {
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
        '(do () (#t))': true,
    };

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
            var ans = (datumRoot instanceof Datum) && new Parser(datumRoot).parse(type);
            if (!!ans ^ testsForType[toParse]) {
                ++numErrors;
                console.log('testParser ' + type + ': ' + toParse + ': expected ' + testsForType[toParse] + ', got ');
                console.log(ans);
            }
            ++numTests;
        }
    }
    console.log('testParser: ' + numTests + ' tests, ' + numErrors + ' errors');
}