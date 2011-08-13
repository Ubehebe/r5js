function SemanticError(msg) {
    this.msg = msg;
}

function _Eval(tree, env, lhs) {

    var valueOfOnlyChild = function(tree, env) {
        for (var childType in tree)
            return valueOf[childType](tree[childType], env);
    };

    var valueOf = {};

    valueOf['expression'] = valueOfOnlyChild;
    valueOf['literal'] = valueOfOnlyChild;

    /* 4.1.1: The value of the variable reference is the value stored in the location
     to which the variable is bound. It is an error to reference an unbound variable. */
    valueOf['variable'] = function(tree, env) {
        var maybeAns = env[tree['identifier']];
        if (maybeAns !== undefined)
            return maybeAns;
        else throw new SemanticError('unbound variable ' + tree['identifier']);
    };

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

        var args = tree['operand'].map(function(node) { // ecmascript compatibility?
            return valueOf['expression'](node, env);
        });

        // A primitive procedure, represented by a JavaScript function.
        if (typeof proc === 'function') {
            return proc.apply(null, args);
        }

        // A non-primitive procedure, represented by a JavaScript hash.
        else if (proc instanceof SchemeProcedure) {
            for (var i = 0; i < proc.formals.length; ++i)
                proc.lexicalScope[proc.formals[i]] = args[i];
            return valueOf['body'](tree['body'], proc.lexicalScope);
        } else throw new SemanticError('The object ' + proc.name + 'is not applicable');
    };

    /* 4.1.4: A lambda expression evaluates to a procedure.
     The environment in effect when the lambda expression was evaluated
     is remembered as part of the procedure. */
    valueOf['lambda-expression'] = function(tree, env) {
        return {
            formals: tree['formals'],
            lexicalScope: env,
            body: tree['body']
        };
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

    try {
        return valueOf[lhs || 'program'](tree, env);
    } catch (x) {
        console.log(x);
    }

}