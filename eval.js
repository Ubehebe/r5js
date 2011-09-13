function Evaluator(root) {
    this.cur = root;
}

Evaluator.prototype.valueOfOnlyChild = function(env) {
    if (this.cur.firstChild && !this.cur.firstChild.nextSibling)
        return this[this.cur.firstChild.getParse()](env);
    else throw new InternalInterpreterError('expected exactly one child, got '
        + (this.cur.firstChild ? 'more than one' : 'none'));
};

Evaluator.prototype.matchChild = function(predicate) {
    for (var child = this.cur.firstChild; child; child = child.nextSibling)
        if (predicate(child))
            return child;
    return null;
};

Evaluator.prototype.valueOfUnderlying = function(env) {
    return this[this.cur.getParse()](env);
};

Evaluator.prototype.valueOfChildWithType = function(env, type) {
  var child = this.matchChild(function(node) { return node.peekParse() === type; });
    if (child) {
        this.cur = child;
        return this[type](env);
    } else return null;
};

Evaluator.prototype.valuesOfChildrenWithType = function(env, type) {

    var ans = [];
    for (var child = this.cur.firstChild; child; child = child.nextSibling) {
        if (child.peekParse() === type) {
            this.cur = child;
            ans.push(this[type](env));
        }
    }
    return ans;
};

Evaluator.prototype['expression'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['operator'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['operand'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['test'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['consequent'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['alternate'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['recipient'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['init'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['step'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['transformer-spec-identifier'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['template-datum'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['pattern-identifier'] = Evaluator.prototype.valueOfUnderlying;
Evaluator.prototype['command'] = Evaluator.prototype.valueOfUnderlying;


Evaluator.prototype['expression'] = function(env) {
    return this.valueOfUnderlying(env);
};

/* 4.1.1: The value of the variable reference is the value stored in the location
 to which the variable is bound. It is an error to reference an unbound variable. */
Evaluator.prototype['variable'] = function(env) {
    var ans = env[this.cur.payload];
    if (ans !== undefined)
        return ans;
    else throw new UnboundVariable(this.cur.payload);
};

Evaluator.prototype['literal'] = function(env) {
    return this.valueOfUnderlying(env);
};

Evaluator.prototype['self-evaluating'] = function(env) {
    switch (this.cur.type) {
        case 'boolean':
            return this.cur.payload === '#t'; // lowercase conversion already done
        case 'number':
            return parseFloat(this.cur.payload);
        case 'character':
            return new SchemeChar(this.cur.payload);
        case 'string':
            return new SchemeString(this.cur.payload);
        default:
            throw new InternalInterpreterError('unknown self-evaluating type '
                + this.cur.type);
    }
};

/* 4.1.2: (quote <datum>) evaluates to <datum>.
 (quote <datum>) may be abbreviated as '<datum>.
 The two notations are equivalent in all respects. */
Evaluator.prototype['quotation'] = function(env) {
    return this.valueOfChildWithType(env, 'datum');
};

Evaluator.prototype['datum'] = function(env) {
    this.cur.sanitize();
    return this.cur; // we are returning a parse tree as a value!
};

/* 4.1.3: A procedure call is written by simply enclosing in parentheses
 expressions for the procedure to be called and the arguments to be passed to it.
 The operator and operand expressions are evaluated (in an unspecified order)
 and the resulting procedure is passed the resulting arguments. */
Evaluator.prototype['procedure-call'] = function(env) {

    var begin = this.cur;

    var proc = this.valueOfChildWithType(env, 'operator');

    if (typeof proc === 'function') {
        this.cur = begin;
        var args = this.valuesOfChildrenWithType(env, 'operand');
        return proc.apply(null, args);
    }
};

Evaluator.prototype.evaluate = function(env, lhs) {
    return this[lhs || 'expression'](env);
};

function _Eval(tree, env) {

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
        // Notice that this returns a parse tree!
        return new SchemeDatum(tree['datum']);
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

        /* A macro use is syntactically indistinguishable from a procedure call, except
         tha the "operator" must be an identifier (while in a procedure call it can be
         any expression). So we evaluate the "operator". If it is syntactically an identifier
         that refers to a macro, we switch to the macro evaluation.

         7.1.2: Any string that parses as an <expression> will also parse as a <datum>. */
        if (tree['operator'].variable && proc instanceof SchemeMacro) {
            return valueOf['macro-use'](new Parser(recoverText(tree)).parse('macro-use'), env);
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

    /* 4.3.2: A use of a macro whose keyword is associated with a transformer
     specified by syntax-rules is matched against the patterns contained in the
     <syntax rule>s, beginning with the leftmost <syntax rule>. When a match
     is found, the macro use is transcribed hygienically according to the template. */
    valueOf['macro-use'] = function(tree, env) {

        var macro = env[tree.keyword];
        var patternMatch = macro.matchInput(tree.datum, env);
        if (patternMatch) {
            var expr = new Parser(patternMatch.hygienicTranscription());
            return valueOf['expression'](expr, merge(env, patternMatch.env));
        } else throw new MacroError(tree.keyword, "macro use did not match any rule");
    };

    valueOf['program'] = valueOfLastChild;
    valueOf['command-or-definition'] = valueOfOnlyChild;

    /* 5.3: The top-level syntactic environment is extended by
     binding the <keyword> to the specified transformer. */
    valueOf['syntax-definition'] = function(tree, env) {
        return env[tree.keyword] = new SchemeMacro(tree['keyword'], tree['transformer-spec'], env);
    };

    var ans = valueOf[tree.type || 'program'](tree, env);
    return ans;
    /*  (ans instanceof Object && ans.toString !== undefined)
     ? ans.toString()
     : ans;*/
}
