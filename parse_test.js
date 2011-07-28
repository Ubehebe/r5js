// todo bl make templates for any arbitrary expression

function testParser() {

    var positiveTests = {};
    var negativeTests = {};

    positiveTests['variable'] = ['...', '+', '-', 'x'];
    negativeTests['variable'] = ['=>', 'cond', '('];

    positiveTests['quotation'] = ["'1", "''1", '(quote quote)', "'quote"];
    negativeTests['quotation'] = ['quote'];

    positiveTests['self-evaluating'] = ['#t', '1', '#\\a', '#\\space', '"(define foo x y)"'];
    negativeTests['self-evaluating'] = ['+'];

    positiveTests['list'] = ['()', '(())', '((()))', '(1 . ())', '(#())'];

    positiveTests['vector'] = ['#()', '#(#())', '#(())'];
    negativeTests['vector'] = ['##()'];

    positiveTests['procedure-call'] = ['(+)'];
    negativeTests['procedure-call'] = ['(foo x', 'foo x)', '()'];

    positiveTests['lambda-expression'] = ['(lambda () 1)', '(lambda x 1)', '(lambda (x y) (x y))'];
    negativeTests['lambda-expression'] = ['(lambda (x y))', '(lambda lambda)'];

    positiveTests['definition'] = ['(define x x)', '(define (foo x y) (foo x y))', '(begin (define x x) (define y y))', '(define (x . y) 1)'];
    negativeTests['definition'] = ['define', '(define)', '(define x)', '(begin 1)'];

    positiveTests['conditional'] = ['(if x y z)', '(if x y)'];
    negativeTests['conditional'] = ['(if x)', '(if)', 'if'];

    positiveTests['assignment'] = ['(set! let! met!)'];
    negativeTests['assignment'] = ['(set!)', '(set! set!)', '(set! x)'];

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

    for (var type in positiveTests)
        for (var i = 0; i < positiveTests[type].length; ++i)
            assertSuccess(positiveTests[type][i], new Parser(positiveTests[type][i]).parse(type));

    for (var type in negativeTests)
        for (var i = 0; i < negativeTests[type].length; ++i)
            assertFailure(negativeTests[type][i], new Parser(negativeTests[type][i]).parse(type));
}



