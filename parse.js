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

function isSyntacticKeyword(str) {
    /* todo bl: why are define-syntax, let-syntax, letrec-syntax not listed
     in 7.1.1 as syntactic keywords? */
    var kws = ['else', '=>', 'define', 'define-syntax', 'unquote', 'unquote-splicing', 'quote', 'lambda',
        'if', 'set!', 'begin', 'cond', 'and', 'or', 'case', 'let', 'let*', 'letrec', 'let-syntax', 'letrec-syntax', 'do',
        'delay', 'quasiquote'];

    for (var i = 0; i < kws.length; ++i)
        if (str === kws[i])
            return true;

    return false;
}

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
    return this.alternation(
        [
            {type: 'variable'}
        ],
        [
            {type: 'literal'}
        ],
        [
            {type: 'procedure-call'}
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
            {type: 'macro-block'}
        ],
        [
            {type: 'macro-use'}
        ]);
};

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
Parser.prototype['variable'] = function() {
    return this.rhs(
        {type: function(datum) {
            return datum instanceof Datum // because it may be emptyListSentinel
                && datum.isIdentifier()
                && !isSyntacticKeyword(datum.payload);
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

            // todo bl once desugaring is working we need to implement macro lookups:
            /* No luck? Maybe it's a macro use. Reparse the datum tree on the fly and
             evaluate. This may be the coolest line in this implementation.
             else {
             console.log(node.toString());
             // todo bl shouldn't have to go back to text
             return new Parser(
             new Reader(
             new Scanner(node.toString())
             ).read()
             ).parse('macro-use')
             .desugar(env);
             }*/

            var operatorNode = node.at('operator');

            /* Example: ((f x) y). (f x) will desugar to a Continuable
                object which is then handled appropriately by LocalStructure.*/
            if (!operatorNode.isIdentifier())
                operatorNode = operatorNode.desugar(env);

            var operands = node.at('operand'); // will be null if 0 operands

            /* Take a snapshot of the local (nonrecursive) procedure call structure,
             since operands.sequence might destroy that structure. */
            var localStructure = new LocalStructure(operatorNode, operands);
            var cpsNames = [];
            var maybeSequenced = operands && operands.sequenceOperands(env, cpsNames);
            return localStructure.toProcCall(maybeSequenced, cpsNames);

            // Add the local procedure call to the tip of the sequence
            return maybeSequenced
                ? maybeSequenced.appendContinuable(localProcCall)
                : localProcCall;
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
            env.addBinding(
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

function desugarDefinition(name, desugaredExpr, isAssignment) {
    var lastContinuable = desugaredExpr.getLastContinuable(); // (+ 1 2 [_0 ...])
    var argToUse = lastContinuable.continuation.lastResultName; // _0

    var anonymousName= newAnonymousLambdaName(); // proc0
    var procCall = newProcCall(anonymousName, newIdOrLiteral(argToUse), new Continuation(newCpsName()));

    lastContinuable.continuation.nextContinuable = procCall;
    desugaredExpr.definitionHelper = new DefinitionHelper(procCall, lastContinuable, name, isAssignment);
    return desugaredExpr; // (+ 1 2 [_0 (proc0 _0 [...])])
}

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

                /*
                    Example:

                    (define x (+ 1 2))

                    will desugar to

                    (+ 1 2 [_0 (proc0 _0 [...])])

                    where proc0 is a new procedure we made up with a single
                    formal parameter x.
                */

                return desugarDefinition(
                    node.at('variable').payload,
                    node.at('expression').desugar(env, true));
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

                /* Example:

                 (define (foo x y) (+ x y)) ...

                 should desugar to something like

                 ((lambda (foo) ...) (lambda (x y) (+ x y)))

                 */

                var formalRoot = node.at('(');
                var formals = formalRoot.mapChildren(function(child) {
                    return child.payload;
                });

                var name = formals.shift();


                env.addBinding(
                    name,
                    new SchemeProcedure(formals, false, formalRoot.nextSibling, env, name)
                    );
                var desugared = newIdShim(newIdOrLiteral(name), newCpsName());
                
                return desugarDefinition(name, desugared);
            }}
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

                var formalRoot = node.at('.(');
                var formals = formalRoot.mapChildren(function(child) {
                    return child.payload;
                });

                var name = formals.shift();

                env.addBinding(
                    name,
                    new SchemeProcedure(formals, true, formalRoot.nextSibling, env, name)
                    );
                var desugared = newIdShim(newIdOrLiteral(name), newCpsName());

               return desugarDefinition(name, desugared);
            }}
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'definition', atLeast: 0},
            {type: ')'}
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

            return desugarDefinition(
                node.at('variable').payload,
                node.at('expression').desugar(env, true),
                true);
        }
        }
    );
};

// todo bl quasiquotation?

/*
 <cond clause> -> (<test> <sequence>)
 | (<test>)
 | (<test> => <recipient>) */
Parser.prototype['cond-clause'] = function() {
    return this.alternation(
        [
            {type: '('},
            {type: 'test'},
            {type: 'expression', atLeast: 1},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'test'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'test'},
            {type: '=>'},
            {type: 'recipient'},
            {type: ')'}
        ]
    );
};

// <recipient> -> <expression>
Parser.prototype['recipient'] = function() {
    return this.rhs({type: 'expression'});
};

// <case clause> -> ((<datum>*) <sequence>)
Parser.prototype['case-clause'] = function() {
    return this.rhs(
        {type: '('},
        {type: '('},
        {type: 'datum', atLeast: 0},
        {type: ')'},
        {type: 'expression', atLeast: 1},
        {type: ')'}
    );
};

// <binding spec> -> (<variable> <expression>)
Parser.prototype['binding-spec'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'variable'},
        {type: 'expression'},
        {type: ')'}
    );
};

/* <iteration spec> -> (<variable> <init> <step>)
 | (<variable> <init>) */
Parser.prototype['iteration-spec'] = function() {
    return this.alternation(
        [
            {type: '('},
            {type: 'variable'},
            {type: 'init'},
            {type: 'step'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'variable'},
            {type: 'init'},
            {type: ')'}
        ]
    );
};

// <init> -> <expression>
Parser.prototype['init'] = function() {
    return this.rhs({type: 'expression'});
};

// <step> -> <expression>
Parser.prototype['step'] = function() {
    return this.rhs({type: 'expression'});
};


// <macro use> -> (<keyword> <datum>*)
Parser.prototype['macro-use'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'keyword'},
        {type: 'datum', atLeast: 0},
        {type: ')'},
        {desugar: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = env.get(kw);
            if (macro instanceof SchemeMacro) {
                var template = macro.selectTemplate(node, env);
                if (template) {
                    /* todo bl shouldn't have to go all the way back to the scanner;
                     should be able to say
                     template.hygienicTranscription().parse('expression').eval(env).
                     I suspect I'm not properly sanitizing the hygienic transcription...
                     */
                    var newText = template.hygienicTranscription().toString();
                    var newNode =
                        new Parser(
                            new Reader(
                                new Scanner(newText)
                            ).read()
                        ).parse('expression');

                    /* todo bl: right now, we have to hook up these pointers
                     to communicate tail context info to the macro. Perhaps
                     a cleaner way would be explicitly telling the macro it's
                     in tail position?
                     if (node.parent)
                     newNode.parent = node.parent;
                     else if (node.nextSibling)
                     newNode.nextSibling = node.nextSibling;*/
                    return newNode.desugar(env);
                }
                else throw new MacroError(kw, 'no template matching ' + node.toString());
            } else throw new UnboundVariable(kw);
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
            {type: ')'}
            /* todo bl {value: function(node, env) {
                node.at('(').at('syntax-spec').evalSiblingsReturnNone(env);
                node.at('definition').evalSiblingsReturnNone(env);
                return node.at('expression').evalSiblingsReturnLast(env);
            }
            }*/
        ],
        [
            {type: '('},
            {type: 'letrec-syntax'},
            {type: '('},
            {type: 'syntax-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'expression', atLeast: 1},
            {type: ')'}
            /* todo bl {value: function(node, env) {
                node.at('(').at('syntax-spec').evalSiblingsReturnNone(env);
                node.at('definition').evalSiblingsReturnNone(env);
                return node.at('expression').evalSiblingsReturnLast(env);
            }
            }*/
        ]);
};

// <syntax spec> -> (<keyword> <transformer spec>)
Parser.prototype['syntax-spec'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'keyword'},
        {type: 'transformer-spec'},
        {type: ')'}
        /* todo bl {value: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = node.at('transformer-spec').desugar(env);
            if (!macro.allPatternsBeginWith(kw))
                throw new MacroError(kw, 'all patterns must begin with keyword');
            else if (!macro.ellipsesMatch(kw))
                throw new MacroError(kw, 'ellipsis mismatch');
            else {
                env.addBinding(kw, macro);
                return null;
            }
        }
        }*/
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
        {type: ')'}
        /* todo bl {value: function(node, env) {
            *//* 4.3.2: It is an error for ... to appear in <literals>.
                So we can reuse the pattern-identifier nonterminal
                to check this in the parser. Win! *//*
            var ids = node.at('(').at('pattern-identifier');
            var rules = node.at('syntax-rule');
            // todo bl implement: It is an error for the same pattern
            // variable to appear more than once in a <pattern>.
            return new SchemeMacro(ids, rules, env);
        }
        }*/
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
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'pattern', atLeast: 1},
            {type: '...'},
            {type: ')'}
        ],
        [
            {type: 'pattern-identifier'}
        ],
        [
            {type: '('},
            {type: 'pattern', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'pattern', atLeast: 1},
            {type: '.'},
            {type: 'pattern'},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'pattern', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: 'pattern-datum'}
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
            {type: 'pattern-identifier'}
        ],
        [
            {type: '...'}
        ],
        [
            {type: 'template-datum'}
        ],
        [
            {type: '('},
            {type: 'template', atLeast: 1},
            {type: '.'},
            {type: 'template'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'template', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'template', atLeast: 0},
            {type: ')'}
        ],
        /* todo bl quotations appear in templates in the spec.
         Instead of adding ' to the RHSes here, they should already be
         indistinguishable from (quote ...). */
        [
            {type: "'"},
            {type: 'template'}
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
            return node.sequence(env, true);
        }
        });
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
        {type: ')'}
       /* todo bl {value: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = node.at('transformer-spec').eval(env);
            if (!macro.allPatternsBeginWith(kw))
                throw new MacroError(kw, 'all patterns must begin with ' + kw);
            else if (!macro.ellipsesMatch(kw))
                throw new MacroError(kw, 'ellipsis mismatch');
            else {
                env.addBinding(kw, macro);
                return null;
            }
        }
        }*/
    );
};

Parser.prototype.parse = function(lhs) {
    var fun = this[lhs || 'program'];
    if (fun)
        return fun.apply(this);
    else
        throw new InternalInterpreterError('unknown lhs: ' + lhs);
};