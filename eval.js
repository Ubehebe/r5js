function _Eval(tree, env, lhs) {

    var valueOfOnlyChild = function(tree, env) {
        for (var child in tree)
            return valueOf[tree[child].type](tree[child], env);
    };

    var valueOfLastChild = function(tree, env) {
        for (var child in tree) {
            var siblings = tree[child];
            var val;
            for (var i = 0; i < siblings.length; ++i)
                val = valueOf[siblings[i].type](siblings[i], env);
            return val;
        }
    };

    var valueOf = {};

    valueOf['expression'] = valueOfOnlyChild;

    /* 4.1.1: The value of the variable reference is the value stored in the location
     to which the variable is bound. It is an error to reference an unbound variable. */
    valueOf['variable'] = function(tree, env) {
        var maybeAns = env[tree['identifier']];
        if (maybeAns !== undefined)
            return maybeAns;
        else throw new UnboundVariable(tree['identifier']);
    };

    valueOf['literal'] = valueOfOnlyChild;

    /* 4.1.2: (quote <datum>) evaluates to <datum>.
     (quote <datum>) may be abbreviated as '<datum>.
     The two notations are equivalent in all respects. */
    valueOf['quotation'] = function(tree, env) {
        return externalRepresentation(tree['datum']);
    };

    /* 4.1.2: Numerical constants, string constants, character constants,
     and boolean constants evaluate "to themselves". */
    valueOf['self-evaluating'] = function(tree, env) {
        if (tree['boolean'] !== undefined)
            return tree['boolean'];
        else if (tree['number'])
            return parseFloat(tree['number']);
        else if (tree['string'])
            return new SchemeString(tree['string']);
        else if (tree['character'])
            return new SchemeChar(tree['character']); // todo bl scanner/parser uses 'character', eval/stdlib uses 'char'
    };

    /* 4.1.3: A procedure call is written by simply enclosing in parentheses
     expressions for the procedure to be called and the arguments to be passed to it.
     The operator and operand expressions are evaluated (in an unspecified order)
     and the resulting procedure is passed the resulting arguments. */
    valueOf['procedure-call'] = function(tree, env) {

        var proc = valueOf['expression'](tree['operator'], env);

        if (tree['operator'].variable && proc instanceof SchemeMacro) {
            var datums = tree['operand'].map(function(expr) {
                return exprToDatum(expr);
            });
            var syntheticMacroTree = {keyword: tree['operator'].variable.identifier, datum: datums };
            return valueOf['macro-use'](syntheticMacroTree, env);
        }

        var args = tree['operand'].map(function(node) { // ecmascript compatibility?
            return valueOf['expression'](node, env);
        });

        // A primitive procedure, represented by a JavaScript function.
        if (typeof proc === 'function') {
            return proc.apply(null, args);
        }

        // A non-primitive procedure, represented by a SchemeProcedure object.
        else if (proc instanceof SchemeProcedure) {
            proc.checkNumArgs(args.length);
            return valueOf['body'](proc.body, proc.bindArgs(args));
        }

        else throw new SemanticError('The object ' + proc + 'is not applicable');
    };

    /* 4.1.4: A lambda expression evaluates to a procedure.
     The environment in effect when the lambda expression was evaluated
     is remembered as part of the procedure. */
    valueOf['lambda-expression'] = function(tree, env) {
        var formals = tree['formals'];
        var requiredFormals = formals['variable'] instanceof Array
            ? formals['variable'].map(function(v) { // the usual case: e.g. (lambda (x y) ...)
            return v.identifier;
        })
            : formals['variable'].identifier; // e.g. (lambda x ...)
        var maybeDottedFormal = formals['.variable'] ? formals['.variable'].identifier : null;
        return new SchemeProcedure(requiredFormals, maybeDottedFormal, env, tree['body']);
    };

    valueOf['body'] = function(tree, env) {
        var definitions = tree['definition'];
        for (var i = 0; i < definitions.length; ++i)
            valueOf['definition'](definitions[i], env);
        return valueOf['sequence'](tree['sequence'], env);
    };

    valueOf['definition'] = function(tree, env) {

        // todo bl -- make the evaluator more functional?
        // perhaps return the new environment explicitly?

        // (begin (define x 1) (define y 2))
        if (tree['definition']) {
            valueOfLastChild(tree, env);
        }

        /* (define (foo x y) (+ x y))
         means
         (define foo (lambda (x y) (+ x y))) */
        else if (tree['variable'] instanceof Array) {
            var nameAndFormals = tree['variable'];
            var name = nameAndFormals[0].identifier;
            var requiredFormals = [];
            for (var i = 1; i < nameAndFormals.length; ++i)
                requiredFormals.push(nameAndFormals[i].identifier);
            var maybeDottedFormal = tree['.variable'] ? tree['.variable'].identifier : null;
            env[name] = new SchemeProcedure(requiredFormals, maybeDottedFormal, env, tree['body']);
        }

        // (define x 1)
        else {
            env[tree['variable'].identifier] = valueOf['expression'](tree['expression'], env);
        }

        // no useful "value" of a definition
        return undefined;
    };

    valueOf['sequence'] = valueOfLastChild;

    /* 4.1.5: first, <test> is evaluated. If it yields a true value, then <consequent>
     is evaluated and its value(s) is (are) returned. Otherwise <alternate> is evaluated
     and its value(s) is (are) returned. If <test> yields a false value and no <alternate>
     is specified, then the result of the expression is unspecified. */
    valueOf['conditional'] = function(tree, env) {
        /* 6.3.1: Of all the standard Scheme values, only #f counts
         as false in conditional expressions. */
        return (valueOf['expression'](tree['test'], env) === false)
            ? (tree['alternate'] ? valueOf['expression'](tree['alternate'], env) : undefined)
            : valueOf['expression'](tree['consequent'], env);
    };

    /* 4.1.6: (set! <variable> <expression>)
     <Expression> is evaluated, and the resulting value is stored
     in the location to which <variable> is bound. <Variable> must be bound either in
     some region enclosing the set! expression or at top level.
     The result of the set! expression is unspecified. */
    valueOf['assignment'] = function(tree, env) {
        var id = tree['variable'].identifier;
        if (env[id] !== undefined) {
            env[id] = valueOf['expression'](tree['expression'], env);
            return undefined;
        } else throw new UnboundVariable(id);
    };

    valueOf['program'] = valueOfLastChild;
    valueOf['command-or-definition'] = valueOfOnlyChild;

    /* 5.3: The top-level syntactic environment is extended by
     binding the <keyword> to the specified transformer. */
    valueOf['syntax-definition'] = function(tree, env) {
        env[tree.keyword] = new SchemeMacro(tree['keyword'], tree['transformer-spec']);
    };

// todo bl investigate doing this in the parser
    function externalRepresentation(datum) {
        var node;
        var ans = [];
        if (node = datum['simple-datum']) {
            return node['text'];
        } else if (node = datum['compound-datum']) {

            if (node = node['list']) {

                if (node['abbreviation']) {

                    console.log(node);

                    throw new Error("todo bl!");

                } else {

                    for (var i = 0; i < node['datum'].length; ++i)
                        ans.push(externalRepresentation(node['datum'][i]));
                    if (node['.datum']) {
                        ans.push('.');
                        ans.push(externalRepresentation(node['.datum']));
                    }
                    return '(' + ans.join(' ') + ')';
                }


            } else if (node = datum['compound-datum']['vector']) {
                for (var i = 0; i < node['datum'].length; ++i)
                    ans.push(externalRepresentation(node['datum'][i]));
                return '#(' + ans.join(' ') + ')';
            }

            else throw new Error('compound-datum that is neither list nor vector, ' +
                    'should never happen -- indicates logical bug in parser', datum['compound-datum']);

        } else throw new Error('datum that is neither simple-datum nor compound-datum, '
            + 'should never happen -- indicates logical bug in parser', datum);
    }

    return valueOf[lhs || 'program'](tree, env);
}

function recoverText(node, text) {
    var start = findStart(node);
    var stop = findStop(node);
    return text.substr(start, stop - start);
}

function findStart(node) {
    var ans;
    var candidate;
    for (var k in node) {
        var child = node[k];
        if (child.start !== undefined)
            candidate = child.start;
        // The first entry in the array is guaranteed to have the lowest start
        else if (child instanceof Array && child.length > 0)
            candidate = findStart(child[0]);
        else if (typeof child === 'object')
            candidate = findStart(child);
        if (ans === undefined || candidate < ans)
            ans = candidate;
    }
    return ans;
}

function findStop(node) {
    var ans;
    var candidate;
    for (var k in node) {
        var child = node[k];
        if (child.stop !== undefined)
            candidate = child.stop;
        // The last entry in the array is guaranteed to have the highest start
        else if (child instanceof Array && child.length > 0)
            candidate = findStop(child[child.length-1]);
        else if (typeof child === 'object')
            candidate = findStop(child);
        if (ans === undefined || candidate > ans)
            ans = candidate;
    }
    return ans;
}