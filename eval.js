function _Eval(tree, env, text) {

    var valueOfOnlyChild = function(tree, env, text) {
        for (var child in tree)
            return valueOf[tree[child].type](tree[child], env, text);
    };

    var valueOfLastChild = function(tree, env, text) {
        for (var child in tree) {
            var siblings = tree[child];
            var val;
            for (var i = 0; i < siblings.length; ++i)
                val = valueOf[siblings[i].type](siblings[i], env, text);
            return val;
        }
    };

    var valueOf = {};

    valueOf['expression'] = valueOfOnlyChild;

    /* 4.1.1: The value of the variable reference is the value stored in the location
     to which the variable is bound. It is an error to reference an unbound variable. */
    valueOf['variable'] = function(tree, env, text) {
        var maybeAns = env[tree['identifier']];
        if (maybeAns !== undefined)
            return maybeAns;
        else throw new UnboundVariable(tree['identifier']);
    };

    valueOf['literal'] = valueOfOnlyChild;

    /* 4.1.2: (quote <datum>) evaluates to <datum>.
     (quote <datum>) may be abbreviated as '<datum>.
     The two notations are equivalent in all respects. */
    valueOf['quotation'] = function(tree, env, text) {
        // Notice that this returns a parse tree!
        return new SchemeDatum(tree['datum'], text);
    };

    /* 4.1.2: Numerical constants, string constants, character constants,
     and boolean constants evaluate "to themselves". */
    valueOf['self-evaluating'] = function(tree, env, text) {
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
    valueOf['procedure-call'] = function(tree, env, text) {

        var proc = valueOf['expression'](tree['operator'], env, text);

        /* A macro use is syntactically indistinguishable from a procedure call, except
         tha the "operator" must be an identifier (while in a procedure call it can be
         any expression). So we evaluate the "operator". If it is syntactically an identifier
         that refers to a macro, we switch to the macro evaluation.

         7.1.2: Note that any string that parses as an <expression> will also parse
         as a <datum>. */
        if (tree['operator'].variable && proc instanceof SchemeMacro) {
            return valueOf['macro-use'](new Parser(recoverText(tree, text)).parse('macro-use'), env, text);
        }

        var args = tree['operand'].map(function(node) { // ecmascript compatibility?
            return valueOf['expression'](node, env, text);
        });

        // A primitive procedure, represented by a JavaScript function.
        if (typeof proc === 'function') {
            return proc.apply(null, args);
        }

        // A non-primitive procedure, represented by a SchemeProcedure object.
        else if (proc instanceof SchemeProcedure) {
            proc.checkNumArgs(args.length);
            return valueOf['body'](proc.body, proc.bindArgs(args), text);
        }

        else throw new SemanticError('The object ' + proc + 'is not applicable');
    };

    /* 4.1.4: A lambda expression evaluates to a procedure.
     The environment in effect when the lambda expression was evaluated
     is remembered as part of the procedure. */
    valueOf['lambda-expression'] = function(tree, env, text) {
        var formals = tree['formals'];
        var requiredFormals = formals['variable'] instanceof Array
            ? formals['variable'].map(function(v) { // the usual case: e.g. (lambda (x y) ...)
            return v.identifier;
        })
            : formals['variable'].identifier; // e.g. (lambda x ...)
        var maybeDottedFormal = formals['.variable'] ? formals['.variable'].identifier : null;
        return new SchemeProcedure(requiredFormals, maybeDottedFormal, env, tree['body']);
    };

    valueOf['body'] = function(tree, env, text) {
        var definitions = tree['definition'];
        for (var i = 0; i < definitions.length; ++i)
            valueOf['definition'](definitions[i], env);
        return valueOf['sequence'](tree['sequence'], env, text);
    };

    valueOf['definition'] = function(tree, env, text) {

        // (begin (define x 1) (define y 2))
        if (tree['definition']) {
            valueOfLastChild(tree, env, text);
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
            env[tree['variable'].identifier] = valueOf['expression'](tree['expression'], env, text);
        }

        // no useful "value" of a definition
        return undefined;
    };

    valueOf['sequence'] = valueOfLastChild;

    /* 4.1.5: first, <test> is evaluated. If it yields a true value, then <consequent>
     is evaluated and its value(s) is (are) returned. Otherwise <alternate> is evaluated
     and its value(s) is (are) returned. If <test> yields a false value and no <alternate>
     is specified, then the result of the expression is unspecified. */
    valueOf['conditional'] = function(tree, env, text) {
        /* 6.3.1: Of all the standard Scheme values, only #f counts
         as false in conditional expressions. */
        return (valueOf['expression'](tree['test'], env, text) === false)
            ? (tree['alternate'] ? valueOf['expression'](tree['alternate'], env, text) : undefined)
            : valueOf['expression'](tree['consequent'], env, text);
    };

    /* 4.1.6: (set! <variable> <expression>)
     <Expression> is evaluated, and the resulting value is stored
     in the location to which <variable> is bound. <Variable> must be bound either in
     some region enclosing the set! expression or at top level.
     The result of the set! expression is unspecified. */
    valueOf['assignment'] = function(tree, env, text) {
        var id = tree['variable'].identifier;
        if (env[id] !== undefined) {
            env[id] = valueOf['expression'](tree['expression'], env, text);
            return undefined;
        } else throw new UnboundVariable(id);
    };

    /* 4.3.2: A use of a macro whose keyword is associated with a transformer
        specified by syntax-rules is matched against the patterns contained in the
        <syntax rule>s, beginning with the leftmost <syntax rule>. When a match
        is found, the macro use is transcribed hygienically according to the template. */
    valueOf['macro-use'] = function(tree, env, text) {

        var macro = env[tree.keyword];
        var rule = macro.matchInput(tree.datum, env);
        if (rule) {
            var toReparse = rule.hygienicTranscription(tree); // todo bl
            return _Eval(new Parser(toReparse), env, text);
        } else throw new MacroError(tree.keyword, "macro use did not match any rule");
    };

    valueOf['program'] = valueOfLastChild;
    valueOf['command-or-definition'] = valueOfOnlyChild;

    /* 5.3: The top-level syntactic environment is extended by
     binding the <keyword> to the specified transformer. */
    valueOf['syntax-definition'] = function(tree, env, text) {
        env[tree.keyword] = new SchemeMacro(tree['keyword'], tree['transformer-spec'], env);
    };

    var ans = valueOf[tree.type || 'program'](tree, env, text);
    return (ans instanceof Object && ans.toString !== undefined)
        ? ans.toString()
        : ans;
}

function recoverText(node, text) {
    var start = findStart(node);
    var stop = findStop(node);
    return text.substr(start, stop - start);
}

function findStart(node) {
    var ans = node.start;
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
    var ans = node.stop;
    var candidate;
    for (var k in node) {
        var child = node[k];
        if (child.stop !== undefined)
            candidate = child.stop;
        // The last entry in the array is guaranteed to have the highest start
        else if (child instanceof Array && child.length > 0)
            candidate = findStop(child[child.length - 1]);
        else if (typeof child === 'object')
            candidate = findStop(child);
        if (ans === undefined || candidate > ans)
            ans = candidate;
    }
    return ans;
}