/* todo bl: this file should not exist.

 Short explanation: (define if +) (if 1 2 3) => 6 ; legal

 Longer explanation: having read the standard closely, I believe
 that Scheme allows every identifier to be rebound in principle*.
 This includes identifiers that are unbound in the default environment,
 like "foo"; identifiers that are bound to values in the default environment,
 like "+"; and identifiers that are bound to syntax in the default
 environment. This last group includes identifiers that are bound
 to macros, like "let", but also identifiers that are "bound to" builtin
 syntax, like "define".

 It is this last subgroup that makes having a "parser" a bad idea.
 In the example above, (define if +) must parse as a definition, which
 means if must parse as a variable. (if 1 2 3) must parse as a procedure
 call, not as a conditional.

 It might be possible to make the parser aware of the fundamental
 syntax identifiers that are currently rebound, but it seems like a lot
 of work. The real solution is to delete the parser, putting the datum
 tree straight on the trampoline. The fundamental syntax identifiers
 would merely point to "BuiltinSyntax" objects in the default environment.

 The drawback of this approach would be that all the "well-formedness"
 code currently in the parser, and expressed declaratively as grammar
 rules, would have to migrate somehow to the trampoline. For many
 cases, it would be okay for the trampoline to try its best and raise
 an error if it really couldn't evaluate something. For example,

 (if 1 2 3 4)

 would be an easy error to detect if if has its default "BuiltinSyntax"
 binding. However, there would be some tricky cases. How would
 we assure that all the definitions in a procedure body come before
 all the expressions, and conversely, how would we allow intermixing
 of definitions and expressions at the top level? How would we detect
 that the following is in fact ungrammatical:

 (define (foo) (begin (define x 1) (+ x x)))

 So there are many questions to think about before excising the parser.

 In the meantime, I've written a hack that is appropriate as long as
 rebinding of fundamental syntax identifiers is rare (which, arguably,
 it should be, since it decreases program readability). If the parser
 parses a fundamental syntax identifier as a variable, after parsing is
 complete, it renames the variable to something safe and does a complete
 re-parse.

 *footnote: the standard is somewhat contradictory on whether all
 identifiers can be variables. The rule in the lexical grammar (7.1.1) is

 <variable> -> <any <identifier> that isn’t also a <syntactic keyword>>

 Against this is the discussion in section 4.3 which states:

 "The syntactic keyword of a macro may shadow variable bindings,
 and local variable bindings may shadow keyword bindings."

 Additionally, the last example in section 4.3.1 appears to show
 if being rebound to #f. Finally, as circumstantial evidence both
 MIT Scheme and PLT Scheme support things like the example at the top of
 this comment.

 Note that rebinding an identifier is disallowed in certain cases that
 might lead to circularity (see section 5.3); this explicitly disallows
 (define define ...), though both MIT Scheme and PLT Scheme allow that.

 todo bl: implement the circularity-checking algorithm described
 in R6RS. */

function Parser(root) {
    /* The next datum to parse. When a parse of a node is successful,
     the next pointer advanced to the node's next sibling. Thus, this.next
     will only be null or undefined in two cases:

     1. EOF
     2. Advancing past the end of a nonempty list. (The empty-list corner case
     is handled by emptyListSentinel below.)
     */
    this.next = root;

    /* The last datum parsed. We only need this in order to figure out where
     to go next after finishing parsing a list. this.prev is only updated
     in two cases:

     1. Moving from a parent (= this.prev) to its first child (= this.next)
     2. Moving from a sibling (= this.prev) to its next sibling (= this.next)

     Thus, this.prev is only null until the first successful move from
     parent to first child or from sibling to next sibling, and is never
     thereafter null. */
    this.prev = null;

    /* We use a special sentinel object to handle the corner case of an empty
     list. According to the tree constructed by the reader (the structure of
     which the parser does not modify), an empty list is simply a datum
     of type '(' whose firstSibling is null or undefined. This presents a
     problem for the parser: when this.next is null, have we advanced past
     the end of a list, or was the list empty to begin with? We must
     distinguish these cases, because they affect what to parse next.
     (See comments in onDatum().)

     For a long time, I tried to distinguish them via some pointer trickery,
     but this concealed some very subtle bugs. So I decided it was clearer
     to compare against a dedicated sentinel object.

     The sentinel is an immutable object with no state; we use it only
     for direct identity comparisons. It is used only internally by the
     parser; it never enters the parse tree.
     */
    this.emptyListSentinel = new Object();

//    this.fixParserSensitiveIds = false;
}

/* When a parse of a node n succeeds, n is returned and this.next
 is advanced to the next node to parse. When a parse of n fails,
 null is returned and this.next still points to n. */
Parser.prototype.rhs = function() {
    var parseFunction;

    var root = this.next;

    /* This is a convenience function: we want to specify parse rules like
     (<variable>+ . <variable>) as if we don't know ahead of time whether
     the list is going to be dotted or not, but the reader already knows.
     Proper and improper lists are both represented as first-child-next-sibling
     linked lists; the only difference is the type ('(' vs. '.('). So we rewrite the
     parse rules to conform to the reader's knowledge. */
    this.rewriteImproperList(arguments);

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];

        // Process parsing actions
        if (element.type) {
            var parsed = (parseFunction = this[element.type])
                ? this.onNonterminal(element, parseFunction)
                : this.onDatum(element);
            if (!parsed) {
                /* This check is necessary because root may be the special
                 sentinel object for empty lists. */
                if (root instanceof Datum)
                    root.unsetParse();
                this.next = root;
                return null;
            }
        }

        if (element.desugar) {
            /* todo bl this is an error in the text of the grammar, and
                should be caught at startup time, not parsing time. It would be
                nice to add a preprocessing step to the interpreter to verify
                the integrity of all its data structures before it starts
                accepting input from the user. */
            if (i !== arguments.length - 1)
                throw new InternalInterpreterError('desugaring actions '
                + 'are only allowed as the last element in the right-hand '
                + 'side of a grammar rule.');
            /* If we are here, root must be an instance of Datum. The only
                other possibility is the emptyListSentinel, but that always
                causes parsing to fail, so we could not be here. */
            if (root)
                root.setDesugar(element.desugar);
        }

    }

    this.next = root /* just in case of an empty program */ && root.nextSibling;
    return root;
};

Parser.prototype.alternation = function() {
    var possibleRhs;
    for (var i = 0; i < arguments.length; ++i) {
        if (possibleRhs = this.rhs.apply(this, arguments[i]))
            return possibleRhs;
    }
    return null;
};

Parser.prototype.rewriteImproperList = function(rhsArgs) {
    // example: (define (x . y) 1) => (define .( x . ) 1)
    /* No RHS in the grammar has more than one dot.
     This will break if such a rule is added. */

    var indexOfDot = -1;
    for (var i = 0; i < rhsArgs.length; ++i) {
        if (rhsArgs[i].type === '.') {
            indexOfDot = i;
            break;
        }
    }

    if (indexOfDot !== -1) {
        /* Change the datum following the dot to be vacuous -- it has already
         been read as part of the list preceding the dot.
         todo bl: this will cause problems with exactly one part of the grammar:
         <template> -> (<template element>+ . <template>)
         I think it's easier to check for this in the evaluator. */
        rhsArgs[i + 1].type = '.';
        // Find the closest opening paren to the left of the dot and rewrite it as .(
        for (var i = indexOfDot - 1; i >= 0; --i) {
            if (rhsArgs[i].type === '(') {
                rhsArgs[i].type = '.(';
                return;
            }
        }
    }
};

Parser.prototype.onNonterminal = function(element, parseFunction) {

    var parsed;

    // Handle repeated elements
    if (element.atLeast !== undefined) { // explicit undefined since atLeast 0 should be valid
        var numParsed = 0;

        /* todo bl too hard to understand. Has to do with recovering the
         next pointer after falling off the end of a deeply-nested list. However,
         it only seems to be needed for the let-syntax and letrec-syntax
         nonterminals. This is an indication that I don't understand how the
         parser really works.

         The parser would be much simpler if each parsing action returned
         the datum it parsed on success and null on failure, rather than
         tinkering with the state pointers prev and next. I haven't done this
         so far because it would seem to require passing an additional
         node parameter around. Currently, all the parameters in the parsing
         functions are descriptions of the grammar. I probably need to
         factor the parser into parser logic and a grammar that the parser
         reads. */
        if (!this.next)
            this.next = this.prev.closestAncestorSibling();

        while (this.next // We haven't fallen off the end of the list
            && this.next !== this.emptyListSentinel // And we're not at an empty list
            && (parsed = parseFunction.apply(this))) { // And the parse succeeds
            // this.next has already been advanced by the success of parseFunction
            parsed.setParse(element.type);
            ++numParsed;
        }

        return numParsed >= element.atLeast;
    }

    // No repetition: exactly one of element.
    else {
        parsed = parseFunction.apply(this);
        if (parsed) {
            parsed.setParse(element.type);
            this.next = parsed.nextSibling;
        }
        return parsed;
    }
};

Parser.prototype.advanceToChildIf = function(predicate) {
    var ans = this.next && predicate(this.next);
    if (ans) {
        this.prev = this.next;
        /* See comments in body of Parser() for explanation of
            emptyListSentinel. */
        this.next = this.next.firstChild || this.emptyListSentinel;
    }
    return ans;
};

Parser.prototype.nextIf = function(predicate) {
    var ans = this.next && predicate(this.next);
    if (ans) {
        this.prev = this.next;
        this.next = this.next.nextSibling;
    }
    return ans;
};

Parser.prototype.onDatum = function(element) {

    if (typeof element.type === 'string') {

        switch (element.type) {
            case '.': // vacuous; we already rewrote ( ... . as .( ...
                return true;
            case '(': // the reader's notation for proper list
            case '.(': // the reader's notation for improper (dotted) list
            case '#(': // the reader's notation for vector
            case "'": // various other reader types
            case '`':
            case ',':
            case ',@':
                return this.advanceToChildIf(function(datum) {
                    return datum.type === element.type;
                });
            case ')':
                /* We have fallen off the end of a non-empty list.
                    For example, in

                    (a b (c d) e)

                    we have just finished parsing d. next is null, prev is d,
                    prev.parent is (c d), and prev.parent.nextSibling is e,
                    which is where we want to go next. */
                if (!this.next) {
                    this.next = this.prev.parent && this.prev.parent.nextSibling;
                    return true;
                }

                /*
                    We have fallen off the "end" of an empty list. For example, in

                    (a b () e)

                    we have just finished parsing (). next is emptyListSentinel,
                    prev is (), and prev.nextSibling is e, which is where we
                    want to go next. */
                else if (this.next === this.emptyListSentinel) {
                    this.next = this.prev.nextSibling;
                    return true;
                }

                // If we're not at the end of a list, this parse must fail.
                else return false;
            default:
                // Convenience for things like rhs({type: 'define'})
                return this.nextIf(function(datum) {
                    return datum.payload === element.type;
                });
        }

    } else if (typeof element.type === 'function') {
        return this.nextIf(element.type);
    }
};



/* <expression> -> <variable>
 | <literal>
 | <procedure call>
 | <lambda expression>
 | <conditional>
 | <assignment>
 | <derived expression> (these are all macros, not needed in grammar)
 | <macro use>
 | <macro block>
 */
Parser.prototype['expression'] = function() {
    /* In order to support shadowing of syntactic keywords,
    the order of the following rules is important. Consider:

    (define if 3)
    (+ 1 if) => 4

    For (+ 1 if) to parse as a procedure call, if must parse as a variable.
    This somewhat contradicts the rule at 7.1.1:

    <variable> -> <any <identifier> that isn't also a <syntactic keyword>>

    But if we can't ensure that variables aren't syntactic keywords, we
    must ensure that "built-in" syntax isn't accidentally captured
    as variables. If the procedure call RHS was listed before the lambda
    expression RHS, then for example

     (lambda () 1)

     would parse as a procedure call.

     todo bl: the real solution is to simplify the grammar even more
     so that (lambda () 1) parses as something like a macro use, then
     install a "super-macro" for lambda that contains custom logic in
     JavaScript. That way, the syntactic keyword could be shadowed
     appropriately. */
    return this.alternation(
        [
            {type: 'variable'}
        ],
        [
            {type: 'literal'}
        ],
        [
            {type: 'lambda-expression'}
        ],
        [
            {type: 'conditional'}
        ],
        [
            {type: 'assignment'}
        ],
        [
            {type: 'quasiquotation'},
            {desugar: function(node, env) {
                return node.normalizeInput().decorateQuasiquote(1);
            }
            }
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'expression', atLeast: 1},
            {type: ')'}
        ],
        [
            {type: 'macro-block'}
        ],
        [
            {type: 'procedure-call'}
        ],
        [
            {type: 'macro-use'}
        ]);
};

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
Parser.prototype['variable'] = function() {
    var self = this;
    return this.rhs(
        {type: function(datum) {
            var ans = datum instanceof Datum // because it may be emptyListSentinel
                && datum.isIdentifier();
            if (ans && isParserSensitiveId(datum.payload))
                self.fixParserSensitiveIds = true;
            return ans;
        }});
};

// <literal> -> <quotation> | <self-evaluating>
Parser.prototype['literal'] = function() {
    return this.alternation(
        [
            {type: 'self-evaluating'}
        ],
        [
            {type: 'quotation'}
        ]);
};

// <quotation> -> '<datum> | (quote <datum>)
Parser.prototype['quotation'] = function() {

    return this.alternation(
        [
            {type: "'"},
            {type: 'datum'},
            {desugar: function(node, env) {
                return node.normalizeInput();
            }
            }
        ],
        [
            {type: '('},
            {type: 'quote'},
            {type: 'datum'},
            {type: ')'},
            {desugar: function(node, env) {
                return node.normalizeInput();
            }
            }
        ]);
};

Parser.prototype['datum'] = function() {
    return this.rhs({type: function(datum) {
            return true;
        }});
};

// <self-evaluating> -> <boolean> | <number> | <character> | <string>
Parser.prototype['self-evaluating'] = function() {

    return this.rhs(
        {type: function(datum) {
            switch (datum.type) {
                case 'boolean':
                case 'number':
                case 'character':
                case 'string':
                    return true;
                default:
                    return false;
            }
        }
        }
    );
};

// <procedure call> -> (<operator> <operand>*)
// <operator> -> <expression>
// <operand> -> <expression>
Parser.prototype['procedure-call'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'operator'},
        {type: 'operand', atLeast: 0},
        {type: ')'},
        {desugar: function(node, env) {

            var operatorNode = node.at('operator');
            var operands = node.at('operand'); // will be null if 0 operands

            if (operatorNode.isLiteral()) {
                return newProcCall(operatorNode, operands, new Continuation(newCpsName()));
            }

            // Example: ((f x) y) => (f x [_0 (_0 y [_1 ...])])
            else {
                var desugaredOp = operatorNode.desugar(env);
                var lastContinuation = desugaredOp.getLastContinuable().continuation;
                var opName = lastContinuation.lastResultName;
                lastContinuation.nextContinuable = newProcCall(
                    newIdOrLiteral(opName),
                    operands,
                    new Continuation(newCpsName()));
                return desugaredOp;
            }
        }
        }
    );
};

Parser.prototype['operator'] = function() {
    return this.rhs({type: 'expression'});
};

Parser.prototype['operand'] = function() {
    return this.rhs({type: 'expression'});
};

// <lambda expression> -> (lambda <formals> <body>)
// <body> -> <definition>* <sequence>
// <sequence> -> <command>* <expression>
// <command> -> <expression>
Parser.prototype['lambda-expression'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'lambda'},
        {type: 'formals'},
        {type: 'definition', atLeast: 0},
        {type: 'expression', atLeast: 1},
        {type: ')'},
        {desugar: function(node, env) {
            var formalRoot = node.at('formals');
            var formals, treatAsDotted;

            // (lambda (x y) ...)
            if (formalRoot.isList()) {
                formals = formalRoot.mapChildren(function(child) {
                return child.payload;
            });
            }

            // (lambda (x y z . w) ...)
            else if (formalRoot.isImproperList()) {
                 formals = formalRoot.mapChildren(function(child) {
                return child.payload;
            });
                treatAsDotted = true;
            }

            /* (lambda <variable> <body>)
             R5RS 4.1.4:
             "The procedure takes any number of arguments; when the procedure
             is called, the sequence of actual arguments is converted into a
             newly allocated list, and the list is stored in the binding of the
             <variable>." */
            else {
                formals = [formalRoot.payload];
                treatAsDotted = true;
            }

            var name = newAnonymousLambdaName();
            env.addClosure(
                name,
                new SchemeProcedure(formals, treatAsDotted, formalRoot.nextSibling, env, name));
            return newIdShim(newIdOrLiteral(name), newCpsName());
        }
        }
    );
};

/* Why are there no <body> or <sequence> nonterminals?
 Because there is no datum associated with those nonterminals.
 For example, in (lambda () (define x 1) x), the text of <body>
 is (define x 1) x, which is not a datum.

 We could of course change the datum tree to accommodate this -- perhaps
 most easily by inserting a vacuous parent node. But as I understand it, the
 whole purpose of keeping the datum tree around during parsing is to make
 on-the-fly reinterpretation of the datum tree easy. For example, perhaps we
 parsed (foo x y) as a procedure call and now it needs to be reparsed as a
 macro use. If we had made changes to the datum tree, we might have to undo
 them now.

 I think that the only nonterminals that don't correspond to datums are:

 <body> -> <definition>* <sequence>
 <sequence> -> <command>* <expression>
 <def formals> -> <variable>* | <variable>* . <variable>
 <program> -> <command or definition>*

 So I decided to modify the grammar to replace these nonterminals by their
 RHSes.

 */

// <formals> -> (<variable>*) | <variable> | (<variable>+ . <variable>)
Parser.prototype['formals'] = function() {

    return this.alternation(
        [
            {type: '('},
            {type: 'variable', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: 'variable'}
        ],
        [
            {type: '('},
            {type: 'variable', atLeast: 1},
            {type: '.'},
            {type: 'variable'},
            {type: ')'}
        ]);
};

/*
<definition> -> (define <variable> <expression>)
| (define (<variable> <def formals>) <body>)
| (begin <definition>*)
 <def formals> -> <variable>* | <variable>* . <variable>
 */
Parser.prototype['definition'] = function() {

    return this.alternation(
        [
            {type: '('},
            {type: 'define'},
            {type: 'variable'},
            {type: 'expression'},
            {type: ')'},
            {desugar: function(node, env) {
                /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
                var variable = node.at('variable');
                var desugaredExpr = variable.nextSibling.desugar(env, true);
                var lastContinuable = desugaredExpr.getLastContinuable();
                var cpsName = lastContinuable.continuation.lastResultName;
                lastContinuable.continuation.nextContinuable =
                    newAssignment(variable.payload, cpsName, new Continuation(newCpsName()))
                        .setTopLevelAssignment();
                return desugaredExpr;
            }
            }
        ],
        [
            {type: '('},
            {type: 'define'},
            {type: '('},
            {type: 'variable', atLeast: 1},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'expression', atLeast: 1},
            {type: ')'},
            {desugar: function(node, env) {
                /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
                var def = node.extractDefinition();
                var name = def.firstChild;
                var lambda = name.nextSibling;
                var formalRoot = lambda.firstChild.nextSibling;
                var formals = formalRoot.mapChildren(function(child) {
                    return child.payload;
                });
                var anonymousName = newAnonymousLambdaName();
                env.addBinding(
                    anonymousName,
                    new SchemeProcedure(formals, false, formalRoot.nextSibling, env, name));
                return newAssignment(name.payload, anonymousName, new Continuation(newCpsName()))
                    .setTopLevelAssignment();
            }
            }
        ],
        [
            {type: '('},
            {type: 'define'},
            {type: '('},
            {type: 'variable', atLeast: 1},
            {type: '.'},
            {type: 'variable'},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'expression', atLeast: 1},
            {type: ')'},
            {desugar: function(node, env) {
                /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
                var def = node.extractDefinition();
                var name = def.firstChild;
                var lambda = name.nextSibling;
                var formalRoot = lambda.firstChild.nextSibling;
                var formals = formalRoot.firstChild
                    ? formalRoot.mapChildren(function(child) {
                    return child.payload;
                }) : [formalRoot.payload];
                var anonymousName = newAnonymousLambdaName();
                env.addBinding(
                    anonymousName,
                    new SchemeProcedure(formals, true, formalRoot.nextSibling, env, name));
                return newAssignment(name.payload, anonymousName, new Continuation(newCpsName()))
                    .setTopLevelAssignment();
            }
            }
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'definition', atLeast: 0},
            {type: ')'}
            // will be recursively desugared automatically by sequence()
        ]);

};

// <conditional> -> (if <test> <consequent> <alternate>)
Parser.prototype['conditional'] = function() {

    return this.alternation(
        [
            {type: '('},
            {type: 'if'},
            {type: 'test'},
            {type: 'consequent'},
            {type: 'alternate'},
            {type: ')'},
            {desugar: function(node, env) {
                var test = node.at('test').desugar(env, true);
                var consequent = node.at('consequent').desugar(env, true);
                var alternate = node.at('alternate').desugar(env, true);

                var testEndpoint = test.getLastContinuable();

                var testName = newIdOrLiteral(testEndpoint.continuation.lastResultName);
                var branch = newBranch(testName, consequent, alternate, new Continuation(newCpsName()));
                testEndpoint.continuation.nextContinuable = branch;
                return test;
            }
            }
        ],
        [
            {type: '('},
            {type: 'if'},
            {type: 'test'},
            {type: 'consequent'},
            {type: ')'},
            {desugar: function(node, env) {
                var test = node.at('test').desugar(env, true);
                var consequent = node.at('consequent').desugar(env, true);

                var testEndpoint = test.getLastContinuable();

                var testName = newIdOrLiteral(testEndpoint.continuation.lastResultName);
                var branch = newBranch(testName, consequent, null, new Continuation(newCpsName()));
                testEndpoint.continuation.nextContinuable = branch;
                return test;
            }
            }
        ]
    );
};

// <test> -> <expression>
Parser.prototype['test'] = function() {
    return this.rhs({type: 'expression'});
};

// <consequent> -> <expression>
Parser.prototype['consequent'] = function() {
    return this.rhs({type: 'expression'});
};

// <alternate> -> <expression> | <empty>
Parser.prototype['alternate'] = function() {
    return this.rhs({type: 'expression'});
};

// <assignment> -> (set! <variable> <expression>)
Parser.prototype['assignment'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'set!'},
        {type: 'variable'},
        {type: 'expression'},
        {type: ')'},
        {desugar: function(node, env) {
            // (set! x (+ y z)) => (+ y z [_0 (set! x _0 ...)])
            var variable = node.at('variable');
            var desugaredExpr = variable.nextSibling.desugar(env, true);
            var lastContinuable = desugaredExpr.getLastContinuable();
            var cpsName = lastContinuable.continuation.lastResultName;
            lastContinuable.continuation.nextContinuable =
                newAssignment(variable.payload, cpsName, new Continuation(newCpsName()));
            return desugaredExpr;
        }
        }
    );
};

// <quasiquotation> -> <quasiquotation 1>
// <quasiquotation D> -> `<qq template D> | (quasiquote <qq template D>)
Parser.prototype['quasiquotation'] = function() {
    return this.alternation(
        [
            {type: '`'},
            {type: 'qq-template'}
        ],
        [
            {type: '('},
            {type: 'quasiquote'},
            {type: 'qq-template'},
            {type: ')'}
        ]
    );
};

/* <qq template 0> -> <expression>
 <qq template D> -> <simple datum>
 | <list qq template D>
 | <vector qq template D>
 | <unquotation D>
 */
Parser.prototype['qq-template'] = function() {
    return this.alternation(
       /* [ todo bl do we need this?
            {type: 'expression', ifQqLevel: 0}
        ],*/
        [
            {type: function(datum) {
                switch (datum.type) {
                    case 'boolean':
                    case 'number':
                    case 'character':
                    case 'string':
                    case 'identifier':
                        return true;
                    default:
                        return false;
                }
            }
            }
        ],
        [
            {type: 'list-qq-template'}
        ],
        [
            {type: 'vector-qq-template'}
        ],
        [
            {type: 'unquotation'}
        ]
    );
};

/*<list qq template D> -> (<qq template or splice D>*)
 | (<qq template or splice D>+ . <qq template D>)
 | '<qq template D>
 | <quasiquotation D+1>
 */
Parser.prototype['list-qq-template'] = function() {
  return this.alternation(
    [
        {type: '('},
        {type: 'qq-template-or-splice', atLeast: 0},
        {type: ')'}
    ],
      [
          {type: '('},
          {type: 'qq-template-or-splice', atLeast: 1},
          {type: '.'},
          {type: 'qq-template-or-splice'},
          {type: ')'}
      ],
      [
          {type: "'"},
          {type: 'qq-template'}
      ],
      [
          {type: 'quasiquotation'}
      ]
  );
};

// <vector qq template D> -> #(<qq template or splice D>*)
Parser.prototype['vector-qq-template'] = function() {
    return this.rhs(
        {type: '#('},
        {type: 'qq-template-or-splice', atLeast: 0},
        {type: ')'}
    );
};

// <unquotation D> -> ,<qq template D-1> | (unquote <qq template D-1>)
Parser.prototype['unquotation'] = function() {
    return this.alternation(
        [
            {type: ','},
            {type: 'qq-template'}
        ],
        [
            {type: '('},
            {type: 'unquote'},
            {type: 'qq-template'},
            {type: ')'}
        ]
    );
};

// <qq template or splice D> -> <qq template D> | <splicing unquotation D>
Parser.prototype['qq-template-or-splice'] = function() {
    return this.alternation(
        [
            {type: 'qq-template'}
        ],
        [
            {type: 'splicing-unquotation'}
        ]
    );
};

/* <splicing unquotation D> -> ,@<qq template D-1>
 | (unquote-splicing <qq template D-1>)
 */
Parser.prototype['splicing-unquotation'] = function() {
    return this.alternation(
        [
            {type: ',@'},
            {type: 'qq-template'}
        ],
        [
            {type: '('},
            {type: 'unquote-splicing'},
            {type: 'qq-template'},
            {type: ')'}
        ]
    );
};


// <macro use> -> (<keyword> <datum>*)
Parser.prototype['macro-use'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'keyword'},
        {type: 'datum', atLeast: 0},
        {type: ')'},
        {desugar: function(node, env) {
            /* Desugaring of a macro use is trivial. We must leave the "argument"
                datums as-is for the macro pattern matching facility to use.
                The trampoline knows what to do with raw datums in such a
                context. */
            return newProcCall(
                node.at('keyword'),
                node.at('datum'),
                new Continuation(newCpsName()));
        }
        });
};

// <keyword> -> <identifier>
Parser.prototype['keyword'] = function() {
    return this.rhs({type: function(datum) {
        return datum.type === 'identifier';
    }});
};


/* <macro block> -> (let-syntax (<syntax spec>*) <body>)
 | (letrec-syntax (<syntax-spec>*) <body>) */
Parser.prototype['macro-block'] = function() {
    return this.alternation(
        [
            {type: '('},
            {type: 'let-syntax'},
            {type: '('},
            {type: 'syntax-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'expression', atLeast: 1},
            {type: ')'},
            {desugar: function(node, env) {
                return node.desugarMacroBlock(env, 'let');
            }
            }
        ],
        [
            {type: '('},
            {type: 'letrec-syntax'},
            {type: '('},
            {type: 'syntax-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'expression', atLeast: 1},
            {type: ')'},
            {desugar: function(node, env) {
                return node.desugarMacroBlock(env, 'letrec');
            }
            }
        ]);
};

// <syntax spec> -> (<keyword> <transformer spec>)
Parser.prototype['syntax-spec'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'keyword'},
        {type: 'transformer-spec'},
        {type: ')'}
    );
};

// <transformer spec> -> (syntax-rules (<identifier>*) <syntax rule>*)
Parser.prototype['transformer-spec'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'syntax-rules'}, // a terminal
        {type: '('},
        {type: 'pattern-identifier', atLeast: 0},
        {type: ')'},
        {type: 'syntax-rule', atLeast: 0}, // a nonterminal
        {type: ')'},
        {desugar: function(node, env) {
            /*4.3.2: It is an error for ... to appear in <literals>.
                So we can reuse the pattern-identifier nonterminal
                to check this in the parser. Win! */
            var ids = node.at('(').at('pattern-identifier');
            var rules = node.at('syntax-rule');
            // todo bl implement: It is an error for the same pattern
            // variable to appear more than once in a <pattern>.
            return new SchemeMacro(ids, rules, env);
        }
        }
    );
};

// <syntax rule> -> (<pattern> <template>)
Parser.prototype['syntax-rule'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'pattern'},
        {type: 'template'},
        {type: ')'}
    );
};

/* <pattern> -> <pattern identifier>
 | (<pattern>*)
 | (<pattern>+ . <pattern>)
 | (<pattern>+ <ellipsis>)
 | #(<pattern>*)
 | #(<pattern>+ <ellipsis>)
 | <pattern datum>
 */
Parser.prototype['pattern'] = function() {
    return this.alternation(
        [
            {type: '('},
            {type: 'pattern', atLeast: 1},
            {type: '...'},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('(');
                for (var cur = node.at('pattern'); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === '...') {
                        ans.addSubtransformer(new EllipsisTransformer(cur.desugar()));
                        break;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }
            }
        ],
        [
            {type: '#('},
            {type: 'pattern', atLeast: 1},
            {type: '...'},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('#(');
                for (var cur = node.at('pattern'); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === '...') {
                        ans.addSubtransformer(new EllipsisTransformer(cur.desugar()));
                        break;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }}
        ],
        [
            {type: 'pattern-identifier'},
            {desugar: function(node) {
                return new IdOrLiteralTransformer(node);
            }
            }
        ],
        [
            {type: '('},
            {type: 'pattern', atLeast: 0},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('(');
                for (var cur = node.at('pattern'); cur; cur = cur.nextSibling)
                    ans.addSubtransformer(cur.desugar());
                return ans;
            }}
        ],
        [
            {type: '('},
            {type: 'pattern', atLeast: 1},
            {type: '.'},
            {type: 'pattern'},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('.(');
                for (var cur = node.at('pattern'); cur; cur = cur.nextSibling)
                    ans.addSubtransformer(cur.desugar());
                return ans;
            }}
        ],
        [
            {type: '#('},
            {type: 'pattern', atLeast: 0},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('#(');
                for (var cur = node.at('pattern'); cur; cur = cur.nextSibling)
                    ans.addSubtransformer(cur.desugar());
                return ans;
            }}
        ],
        [
            {type: 'pattern-datum'},
            {desugar: function(node) {
                return new IdOrLiteralTransformer(node);
            }}
        ]
    );
};

// <pattern datum> -> <string> | <character> | <boolean> | <number>
Parser.prototype['pattern-datum'] = function() {
    return this.rhs(
        {type: function(datum) {
            switch (datum.type) {
                case 'boolean':
                case 'number':
                case 'character':
                case 'string':
                    return true;
                default:
                    return false;
            }
        }});
};

/* <template> -> <pattern identifier>
 | (<template element>*)
 | (<template element>+ . <template>)
 | #(<template element>*)
 | <template datum>
 <template element> -> <template> | <template> <ellipsis>

 The reader does not support (X+ . Y) where X != Y.
 (Internally, it converts this into something like .(X+), so it just keeps
 looking for X's.) The rule <template> -> (<template element>+ . <template>)
 appears to be the only part of the grammar where this occurs. So I have
 changed the rules for <template> to the following, which I believe is
 equivalent:

 <template> -> <pattern identifier>
 | (<template>*)
 | (<template>+ . <template>)
 | #(<template element>*)
 | <template datum>
 | <ellipsis>

 Anyway, the rules for validating templates with ellipses in them are vague
 (4.3.2: "It is an error if the output cannot be built up [from the template]
 as specified") and I can do this during evaluation of a macro if necessary. */
Parser.prototype['template'] = function() {
    return this.alternation(
        [
            {type: 'pattern-identifier'},
            {desugar: function(node) {
                return new IdOrLiteralTransformer(node);
            }
            }
        ],
        [
            {type: '...'}
        ],
        [
            {type: 'template-datum'},
            {desugar: function(node) {
                return new IdOrLiteralTransformer(node);
            }
            }

        ],
        [
            {type: '('},
            {type: 'template', atLeast: 1},
            {type: '.'},
            {type: 'template'},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('.(');
                for (var cur = node.at('template'); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === '...') {
                        ans.addSubtransformer(new EllipsisTransformer(cur.desugar()));
                        cur = cur.nextSibling;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }

                return ans;
            }
            }
        ],
        [
            {type: '('},
            {type: 'template', atLeast: 0},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('(');
                for (var cur = node.at('template'); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === '...') {
                        ans.addSubtransformer(new EllipsisTransformer(cur.desugar()));
                        cur = cur.nextSibling;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }
            }
        ],
        [
            {type: '#('},
            {type: 'template', atLeast: 0},
            {type: ')'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer('#(');
                for (var cur = node.at('template'); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === '...') {
                        ans.addSubtransformer(new EllipsisTransformer(cur.desugar()));
                        cur = cur.nextSibling;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }
            }
        ],
        [
            {type: "'"},
            {type: 'template'},
            {desugar: function(node) {
                var ans = new ListLikeTransformer("'");
                ans.addSubtransformer(node.at('template').desugar());
                return ans;
            }}
        ]
    );
};

// <template datum> -> <pattern datum>
Parser.prototype['template-datum'] = function() {
    return this.rhs({type: 'pattern-datum'});
};

// <pattern identifier> -> <any identifier except ...>
Parser.prototype['pattern-identifier'] = function() {
    return this.rhs(
        {type: function(datum) {
            return datum.type === 'identifier' && datum.payload !== '...';
        }}
    );
};

// <program> -> <command or definition>*
Parser.prototype['program'] = function() {
    return this.rhs(
        {type: 'command-or-definition', atLeast: 0},
        {desugar: function(node, env) {
            return node.sequence(env);
        }
        }
    );
};

/* <command or definition> -> <command>
 | <definition>
 | <syntax definition>
 | (begin <command or definition>*)
 */
Parser.prototype['command-or-definition'] = function() {
    return this.alternation(
        [
            {type: 'definition'}
        ],
        [
            {type: 'syntax-definition'}
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'command-or-definition', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: 'command'}
        ]);
};

// <command> -> <expression>
Parser.prototype['command'] = function() {
    return this.rhs({type: 'expression'});
};

// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
Parser.prototype['syntax-definition'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'define-syntax'},
        {type: 'keyword'},
        {type: 'transformer-spec'},
        {type: ')'},
        {desugar: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = node.at('transformer-spec').desugar(env);
            if (!macro.allPatternsBeginWith(kw))
                throw new MacroError(kw, "all patterns must begin with " + kw);
            var anonymousName = newAnonymousLambdaName();
            env.addBinding(anonymousName, macro);
            return newAssignment(kw, anonymousName, new Continuation(newCpsName()))
                .setTopLevelAssignment()
                .setSyntaxAssignment();
        }
        }
    );
};

Parser.prototype.parse = function(lhs) {
    var fun = this[lhs || 'program'];
    if (fun) {
        var ans = fun.apply(this);

        if (ans && ans.nonterminals) {
            // See comments at top of Parser.
            if (this.fixParserSensitiveIds) {
                var helper = new RenameHelper();
                ans.fixParserSensitiveIds(helper);
                if (helper.wasUsed()) {
                    /* todo bl inefficient, but i've had errors fusing this
                     into fixParserSensitiveIds() */
                    for (var cur = ans; cur; cur = cur.nextSibling)
                        cur.unsetParse();
                    return new Parser(ans).parse(lhs);
                } else return ans;
            } else return ans;
        } else {
            /* Do not return a node if its nonterminals haven't been set;
             this means parsing failed. Exception: if an lhs was passed in,
             this was for debugging, and we want to present whatever we
             finished with. */
            return lhs ? ans : null;
        }
    }
    else
        throw new InternalInterpreterError('unknown lhs: ' + lhs);
};