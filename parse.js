function Parser(root) {
    /* The next datum to parse. When a parse of a node is successful,
     the next pointer is advanced, principally through
     nextSiblingRecursive(). In this way, this.next will only be null
     or undefined in two cases:

     1. EOF
     2. first (nonexistent) child of an empty list.
     */
    this.next = root;

    /* The last datum parsed. We only need this in order to resolve
     certain corner cases, principally involving empty lists. this.prev
     is only updated in two cases:

     1. Moving from a parent (= this.prev) to its first child (= this.next)
     2. Moving from a sibling (= this.prev) to its next sibling (= this.next)

     Thus, this.prev is only null until the first successful move from
     parent to first child or from sibling to next sibling, and is never
     thereafter null. */
    this.prev = null;
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
                /* This check is necessary because root may be null in the
                 case of empty lists. */
                if (root)
                    root.unsetParse();
                this.next = root;
                return null;
            }
        }

        // Store semantic actions for later evaluation
        else if (element.value) {
            if (i !== arguments.length - 1)
                throw new InternalInterpreterError('unexpected semantic action '
                    + element.value
                    + ' at index ' + i + ', only allowed in last position');
            if (root) root.setValue(element.value);
        }

    }

    /* We do not need a null check here because, if root is null,
     parsing has already failed. */
    this.next = root.nextSiblingRecursive();
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

    var start = this.next;
    var parsed;

    // Handle * and +
    if (element.atLeast !== undefined) { // explicit undefined since atLeast 0 should be valid
        var numParsed = 0;
        var last;
        /* nextSiblingRecursive() is powerful because it advances to the "next"
         datum to parse, no matter where it may be. But when parsing
         repeated nonterminals, we have to be careful not to rise above
         the node we started at. For example, consider parsing ((1 2) 3)
         according to ((datum*) datum):

         1 parses ok as datum -> nextSiblingRecursive is 2
         2 parses ok as datum -> nextSiblingRecursive is 3
         3 parses ok as datum ... but in the same list as 1 and 2!

         So at the beginning of dealing with * and +, we remember
         we would escape to, and abort parsing if we reach there.

         todo: start.lastSibling() means we iterate over the siblings twice. */
        var escaped = start && start.lastSibling().nextSiblingRecursive();

        while (this.next !== escaped && (parsed = parseFunction.apply(this))) {
            parsed.setParse(element.type);
            last = parsed;
            ++numParsed;
        }

        if (numParsed >= element.atLeast) {

            /* In the case of an empty list, start will be null, and returning
             it would incorrectly indicate parsing failure. So we return true
             as a shim. */
            return start || true;
        } else {
            // todo bl is this dead code?
            this.errorMsg = 'expected at least '
                + element.atLeast + ' ' + element.type + ', got ' + numParsed;
            return null;
        }
    }

    // The normal case is exactly one of element.
    else {
        parsed = parseFunction.apply(this);
        if (!parsed) {
            return null;
        } else {
            parsed.setParse(element.type);
            this.next = parsed.nextSiblingRecursive();
            return start;
        }
    }
};

Parser.prototype.advanceToChildIf = function(predicate) {
    var ans = this.next && predicate(this.next);
    if (ans) {
        this.prev = this.next;
        this.next = this.next.firstChild;
    }
    return ans;
};

Parser.prototype.nextIf = function(predicate) {
    var ans = this.next && predicate(this.next);
    if (ans) {
        this.prev = this.next;
        this.next = this.next.nextSiblingRecursive();
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
	    /* This is subtlest (and thus most likely to be wrong) part of
	       the whole interpreter. When we are at the end of a list
	       (vector, etc.), we have to update the next pointer
	       in a way that cannot be local (the next datum to parse
	       could be the next sibling of the great-great-...-great
	       grandmother of the current node). */

	    /* this.next is null/undefined in two cases, EOF or empty list
	       (see discussion inside the Parser class definition).
	       This clause handles the empty list/vector/etc, when
	       this.prev exists but has no first child. In that case, we
	       should just move to the (recursive) next sibling of the empty
	       list.

	       todo bl it seems like this might foul up on an empty
	       list near the end of input, since this.next would be set to
	       null forever. But I don't observe this in practice. Is it
	       because my tests aren't good enough?
	       */
            if (!this.next && this.prev && !this.prev.firstChild) {
                this.next = this.prev.nextSiblingRecursive();
                return true;
            }

	    /* This clause handles the normal case of a non-empty list (etc.).
	       In this case, this.prev points to the last element of the list,
	       this.prev.parent points to the head of the list, and, since
	       we have successfully parsed the last element of the list,
	       this.next already points to the (recursive) next sibling of the
	       head of the list. */
            else if (this.prev
		     && this.prev.parent 
		     && this.prev.parent.nextSiblingRecursive() === this.next) {
                    return true;
                } else {
                    return false;
                }
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
    return this.rhs({type: function(datum) {
            return datum.isIdentifier() && !isSyntacticKeyword(datum.payload);
        }},
        {value: function(node, env, continuation) {
            var val = env[node.payload];
            if (val !== undefined)
                return continuation.inject(val);
            else throw new UnboundVariable(node.payload);
        }
        }
    );
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
            {value: function(node, env, continuation) {
                return continuation.inject(node.at('datum').sanitize());
            }
            }
        ],
        [
            {type: '('},
            {type: 'quote'},
            {type: 'datum'},
            {type: ')'},
            {value: function(node, env, continuation) {
                return continuation.inject(node.at('datum').sanitize());
            }
            }
        ]);
};

Parser.prototype['datum'] = function() {
    return this.rhs({type: function(datum) {
            return true;
        } }
    );
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
        }},
        {value: function(node, env, continuation) {
            return continuation.inject(maybeWrapResult(node.payload, node.type));
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
        {value: function(node, env) {

            var proc = node.at('operator').eval(env);
            var args; //  We don't evaluate the operands unless we're sure it's a procedure

            // A builtin procedure: (+ 1 100)
            if (typeof proc === 'function') {
                args = node.at('operand').evalSiblingsReturnAll(env);
                return proc.apply(null, args);
            }

            /* A defined procedure: (let ((foo (lambda (x) x))) (foo 1)), or simply
             ((lambda (x) x) 1). Note that to get lambdas to work in other syntactic
             contexts (example: (cons (lambda () 1) (lambda () 2))), we have to wrap
             them in datum objects. For consistency, we store them in the environment
             wrapped as well. */
            else if (proc instanceof Datum && proc.isProcedure()) {
                /* We have to clone the procedure object every time we evaluate
                 because evaluation mutates it (specifically, it
                 pops semantic actions off the parse tree). */
                var unwrappedProc = proc.payload.clone();
                args = node.at('operand').evalSiblingsReturnAll(env);
                unwrappedProc.checkNumArgs(args.length);
                return unwrappedProc.eval(args);
            }

            /* No luck? Maybe it's a macro use. Reparse the datum tree on the fly and
             evaluate. This may be the coolest line in this implementation. */
            else {
                var savedParent = node.parent;
                node.unsetParse();
                node.parent = savedParent;
                return new Parser(node).parse('macro-use').eval(env);
            }
        }
        });
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
        {value: function(node, env) {
            var formalRoot = node.at('formals');
            var dotted = formalRoot.isImproperList();
            var formals = dotted || formalRoot.isList()
                ? formalRoot.mapChildren(function(child) {
                return child.payload;
            })
                : [formalRoot.payload];
            return newProcedureDatum(
                new SchemeProcedure(formals, dotted, formalRoot.nextSibling, env));
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
            {value: function(node, env, continuation) {
                // bl do we really need a new continuation here?
                env[node.at('variable').payload] = node.at('expression').eval(env, new Continuation());
                return continuation.inject(undefined);
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
            {value: function(node, env, continuation) {
                var formalsList = node.at('(');
                var formals = formalsList.mapChildren(function(child) {
                    return child.payload;
                });
                var name = formals.shift();
                /* Note that we store the procedure wrapped in a datum.
                 This is just for consistency; to use procedures generally, they have
                 to be wrapped in datums, so we can do things like
                 (cons (lambda () 0) (lambda () 1)). */
                env[name] =
                    newProcedureDatum(
                        new SchemeProcedure(
                            formals, false, formalsList.nextSibling, env, name));
                return undefined;
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
            {value: function(node, env) {
                var formalsList = node.at('(').at('variable');
                var formals = formalsList.mapChildren(function(child) {
                    return child.payload;
                });
                var name = formals.shift();
                /* Note that we store the procedure wrapped in a datum.
                 This is just for consistency; to use procedures generally, they have
                 to be wrapped in datums, so we can do things like
                 (cons (lambda () 0) (lambda () 1)). */
                env[name] =
                    newProcedureDatum(
                        new SchemeProcedure(
                            formals, true, formalsList.nextSibling, env));
                return undefined;
            }}
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'definition', atLeast: 0},
            {type: ')'},
            {value: function(node, env) {
                node.at('definition').evalSiblingsReturnNone(env);
                return undefined;
            }
            }
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
            {value: function(node, env) {
                /* 6.3.1: Except for #f, all standard Scheme values,
                 including #t, pairs, the empty list, symbols, numbers,
                 strings, vectors, and procedures, count as true. */
                return node.at('test').eval(env).unwrap() !== false
                    ? node.at('consequent').eval(env)
                    : node.at('alternate').eval(env);
            }
            }
        ],
        [
            {type: '('},
            {type: 'if'},
            {type: 'test'},
            {type: 'consequent'},
            {type: ')'},
            {value: function(node, env) {
                /* 6.3.1: Except for #f, all standard Scheme values,
                 including #t, pairs, the empty list, symbols, numbers,
                 strings, vectors, and procedures, count as true. */
                return node.at('test').eval(env).unwrap() !== false
                    ? node.at('consequent').eval(env)
                    : undefined;
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
        {value: function(node, env) {
            env[node.at('variable').payload] = node.at('expression').eval(env);
            return undefined;
        }
        });
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
        {value: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = env[kw];
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
                     in tail position? */
                    if (node.parent)
                        newNode.parent = node.parent;
                    else if (node.nextSibling)
                        newNode.nextSibling = node.nextSibling; // yikes
                    return newNode.eval(env);
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
            {type: ')'},
            {value: function(node, env) {
                node.at('(').at('syntax-spec').evalSiblingsReturnNone(env);
                node.at('definition').evalSiblingsReturnNone(env);
                return node.at('expression').evalSiblingsReturnLast(env);
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
            {value: function(node, env) {
                node.at('(').at('syntax-spec').evalSiblingsReturnNone(env);
                node.at('definition').evalSiblingsReturnNone(env);
                return node.at('expression').evalSiblingsReturnLast(env);
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
        {type: ')'},
        {value: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = node.at('transformer-spec').eval(env);
            if (!macro.allPatternsBeginWith(kw))
                throw new MacroError(kw, 'all patterns must begin with keyword');
            else if (!macro.ellipsesMatch(kw))
                throw new MacroError(kw, 'ellipsis mismatch');
            else {
                env[kw] = macro;
                return undefined;
            }
        }
        }
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
        {value: function(node, env) {
            /* 4.3.2: It is an error for ... to appear in <literals>.
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
// todo bl: maybe wrap in a begin-block?
Parser.prototype['program'] = function() {
    return this.rhs(
        {type: 'command-or-definition', atLeast: 0},
        {value: function(node, env, continuation) {
            return continuation.remember(node);
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
            {type: ')'},
            {value: function(node, env) {
                return node.at('command-or-definition').evalSiblingsReturnLast(env);
            }
            }
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
        {value: function(node, env) {
            var kw = node.at('keyword').payload;
            var macro = node.at('transformer-spec').eval(env);
            if (!macro.allPatternsBeginWith(kw))
                throw new MacroError(kw, 'all patterns must begin with ' + kw);
            else if (!macro.ellipsesMatch(kw))
                throw new MacroError(kw, 'ellipsis mismatch');
            else {
                env[kw] = macro;
                return undefined;
            }
        }
        }
    );
};

Parser.prototype.parse = function(lhs) {
    var fun = this[lhs || 'program'];
    if (fun)
        return fun.apply(this);
    else
        throw new InternalInterpreterError('unknown lhs: ' + lhs);
};