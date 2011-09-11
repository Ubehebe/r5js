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
	'((define) foo)': false,
	'((lambda () +) 1 2)': true
    };

    tests['lambda-expression'] = {
	'(lambda () 1)': true,
	'(lambda x 1)': true,
	'(lambda (x y) (x y))': true,
	'(lambda (x y))': false,
	'(lambda (x . y) z)': true,
	'(lambda x . y z)': false,
	'(lambda lambda)': false
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
	'(begin)': true
    };

    tests['conditional'] = {
	'(if x y z)': true,
	'(if x y)': true,
	'(if x)': false,
	'(if)': false,
	'if': false,
	'(IF x y)': true
    };

    tests['assignment'] = {
	'(set! let! met!)': true,
	'(set!)': false,
	'(set! set!)': false, 
	'(set! x)': false
    };

    function assertSuccess(text, result) {
        if (result.fail) {
            console.log('failed parsing ' + text + ':');
            console.log(result);
        }
    }

    function assertFailure(text, result) {
        if (!result.fail) {
            console.log('mistakenly parsed malformed text ' + text + ':');
            console.log(result);
        }
    }

    var numErrors = 0;
    var numTests = 0;

    for (var type in tests) {
	var testsForType = tests[type];
        for (var toParse in testsForType) {
	    var datumRoot = new Reader(toParse).read();
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