function Parser(root) {
    this.prev = null;
    this.next = root;
}

Parser.prototype.rhs = function() {
    var parseFunction;

    var root = this.next;

    /* This is a convenience function: we want to specify parse rules like
     (<variable>+ . <variable>) as if we don't know ahead of time whether
     the list is going to be dotted or not, but of course, the reader already knows.
     Proper and improper lists are both represented as first-child-next-sibling
     linked lists; the only difference is the type ('(' vs. '.('). So we rewrite the
     parse rules to conform to the reader's knowledge. */
    this.rewriteImproperList(arguments);

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        var parsed = (parseFunction = this[element.type])
            ? this.onNonterminal(element, parseFunction)
            : this.onDatum(element);
        if (!parsed) {
            if (root)
                root.unsetParse();
            this.next = root;
            return null;
        }
    }
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
        rhsArgs[i+1].type = '.';
    // Find the closest opening paren to the left of the dot and rewrite it as .(
        for (var i = indexOfDot-1; i >= 0; --i) {
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
        while (parsed = parseFunction.apply(this)) {
            parsed.setParse(element.type);
            ++numParsed;
        }

        if (numParsed >= element.atLeast) {
            return start ? start : true; // dummy value to prevent empty lists registering as failure
        } else {
            if (start)
                start.unsetParse();
            this.errorMsg = 'expected at least '
                + element.atLeast + ' ' + element.nodeName + ', got ' + numParsed;
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
            this.next = parsed.nextSibling;
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

Parser.prototype.advanceToSiblingIf = function(predicate) {
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
            // just for convenience; if the reader succeeded, everything is already a datum
            case 'datum':
                return this.advanceToSiblingIf(function (datum) {
                    return true;
                });
            // vacuous; we already rewrote ( ... . as .( ...
            case '.':
                return true;
            case '(': // the reader's notation for proper list
            case '.(': // the reader's notation for improper (dotted) list
            case '#(': // the reader's notation for vector
            case "'":
            case '`':
            case ',':
            case ',@':
                return this.advanceToChildIf(function(datum) {
                    return datum.type === element.type;
                });
            case ')':
                if (!this.next) {
                    /* This is subtle. A few invariants:
                     - this.prev is read only once in the parser, in this block.
                     - this.prev is updated only when we move from a parent
                     to a child or a sibling to a sibling; it is not updated when we
                     move from a child back up to the parent.
                     Thus, this.prev does not always point to the "correct" location.
                     For example, after reading the first closing paren in ((foo) 1),
                     this.prev points to foo, and it remains pointing to foo for the
                     rest of the expression. The reason this is okay is because we
                     will never look at this.prev until the conclusion of the next list,
                     by which time it *will* be correct. For example, in ((foo) (bar)),
                     this.prev will be set to bar by the time the second-to-last closing
                     paren is read.

                     If we are here, we are done parsing the current list.
                     Normally, this means that this.prev is the last element in the list
                     and this.next is null. this.prev.parent is the head of the current
                     list, so the next datum to parse should be
                     this.prev.parent.nextSibling.

                     A corner case arises with empty lists. In that case, this.next is
                     null, and this.prev points to the head of the empty list. When
                     the empty list has a next sibling, as in (lambda () "hi"), we should parse
                     that sibling next: this.prev.nextSibling.

                     What if the empty list has no next sibling? I am not sure if this
                     code is correct in that case, but I believe it is a nonissue; I can't
                     think of where that could occur in the grammar of Scheme.
                     */
                    this.next = this.prev.nextSibling
                        ? this.prev.nextSibling
                        : this.prev.parent.nextSibling;
                    return true;
                } else return false;
            default:
                return this.advanceToSiblingIf(function(datum) {
                    return datum.payload === element.type;
                });
        }

    } else if (typeof element.type === 'function') {
        return this.advanceToSiblingIf(element.type);
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
 | <derived expression>
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
            {type: 'derived-expression'}
        ],
        [
            {type: 'macro-use'}
        ],
        [
            {type: 'macro-block'}
        ]);
};

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
Parser.prototype['variable'] = function() {
    return this.rhs({type: function(datum) {
            return datum.type === 'identifier'
                && !isSyntacticKeyword(datum.payload);
        }}
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
            {type: 'datum'}
        ],
        [
            {type: '('},
            {type: 'quote'},
            {type: 'datum'},
            {type: ')'}
        ]);
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
        }});
};

// <procedure call> -> (<operator> <operand>*)
// <operator> -> <expression>
// <operand> -> <expression>
Parser.prototype['procedure-call'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'operator'},
        {type: 'operand', atLeast: 0},
        {type: ')'});
};

Parser.prototype['operator'] = function() {
    return this.rhs({type: 'expression'});
};

Parser.prototype['operand'] = function() {
    return this.rhs({type: 'expression'});
};

// <lambda expression> -> (lambda <formals> <body>)
// <body> -> <definition>* <sequence>
Parser.prototype['lambda-expression'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'lambda'},
        {type: 'formals'},
        {type: 'definition', atLeast: 0},
        {type: 'sequence'},
        {type: ')'});
};

// <sequence> -> <command>* <expression>
// <command> -> <expression>
Parser.prototype['sequence'] = function() {
    return this.rhs({type: 'expression', atLeast: 1});
};

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

/* <definition> -> (define <variable> <expression>)
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
            {type: ')'}
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
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'define'},
            {type: '('},
            {type: 'variable', atLeast: 1},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'expression', atLeast: 1},
            {type: ')'}
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
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'if'},
            {type: 'test'},
            {type: 'consequent'},
            {type: ')'}
        ]);
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
        {type: ')'});
};

/* <derived expression> -> (cond <cond clause>+ )
 | (cond <cond clause>* (else <sequence>))
 | (case <expression> <case clause>+)
 | (case <expression> <case clause>* (else <sequence>))
 | (and <test>*)
 | (or <test>*)
 | (let (<binding spec>*) <body>)
 | (let <variable> (<binding spec>*) <body>)
 | (let* (<binding spec>*) <body>)
 | (letrec (<binding spec>*) <body>)
 | (begin <sequence>)
 | (do (<iteration spec>*) (<test> <do result>) <command>*)
 | (delay <expression>)
 | <quasiquotation>
 <do result> -> <sequence> | <empty>
 */
Parser.prototype['derived-expression'] = function() {
    return this.alternation(
        [
            {type: '('},
            {type: 'cond'},
            {type: 'cond-clause', atLeast: 1},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'cond'},
            {type: 'cond-clause', atLeast: 0},
            {type: '('},
            {type: 'else'},
            {type: 'sequence'},
            {type: ')'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'case'},
            {type: 'expression'},
            {type: 'case-clause', atLeast: 1},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'case'},
            {type: 'expression'},
            {type: 'case-clause', atLeast: 0},
            {type: '('},
            {type: 'else'},
            {type: 'sequence'},
            {type: ')'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'and'},
            {type: 'test', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'or'},
            {type: 'test', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'let'},
            {type: '('},
            {type: 'binding-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'sequence'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'let'},
            {type: 'variable'},
            {type: '('},
            {type: 'binding-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'sequence'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'let*'},
            {type: '('},
            {type: 'binding-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'sequence'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'letrec'},
            {type: '('},
            {type: 'binding-spec', atLeast: 0},
            {type: ')'},
            {type: 'definition', atLeast: 0},
            {type: 'sequence'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'sequence'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'do'},
            {type: '('},
            {type: 'iteration-spec', atLeast: 0},
            {type: ')'},
            {type: '('},
            {type: 'test'},
            {type: 'sequence'},
            {type: ')'},
            {type: 'command', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'do'},
            {type: '('},
            {type: 'iteration-spec', atLeast: 0},
            {type: ')'},
            {type: '('},
            {type: 'test'},
            {type: ')'},
            {type: 'command', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'delay'},
            {type: 'expression'},
            {type: ')'}
        ]/*,
         [
         {type: 'quasiquotation'} // todo bl
         ]*/
    );
};

/*
 <cond clause> -> (<test> <sequence>)
 | (<test>)
 | (<test> => <recipient>) */
Parser.prototype['cond-clause'] = function() {
    return this.alternation(
        [
            {type: '('},
            {type: 'test'},
            {type: 'sequence'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'test'},
            {type: ')'}
        ],
        [
            {type: '('},
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
        {type: 'datum'},
        {type: ')'},
        {type: 'sequence'},
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
        {type: ')'});
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
        /* The parser currently doesn't support + and * applied to terminals.
         I decided it was easier to add a vacuous nonterminal
         'transformer-spec-identifier' to the grammar. */
        {type: 'transformer-spec-identifier', atLeast: 0},
        {type: ')'},
        {type: 'syntax-rule', atLeast: 0}, // a nonterminal
        {type: ')'}
    );
};

Parser.prototype['transformer-spec-identifier'] = function() {
    return this.rhs(
        {type: function(datum) {
            return datum.type === 'identifier';
        }}
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
            {type: '('},
            {type: 'pattern', atLeast: 1},
            {type: '...'},
            {type: ')'}
        ], // todo bl do I support the identifier ...?
        [
            {type: '#('},
            {type: 'pattern', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'pattern', atLeast: 1},
            {type: '...'},
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
 */
Parser.prototype['template'] = function() {
    return this.alternation(
        [
            {type: 'pattern-identifier'}
        ],
        [
            {type: '('},
            {type: 'template-element', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'template-element', atLeast: 1},
            {type: '.'},
            {type: 'template'},
            // todo bl: uh oh. Only spot in grammar where (A+ . B), A != B. Unsupported.
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'template-element', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: 'template-datum'}
        ]
    );
};

// <template datum> -> <pattern datum>
Parser.prototype['template-datum'] = function() {
    return this.rhs({type: 'pattern-datum'});
};

// <template element> -> <template> | <template> <ellipsis>
Parser.prototype['template-element'] = function() {
    return this.alternation(
        [
            {type: 'template'},
            {type: '...'}
        ],
        [
            {type: 'template'}
        ]
    );
};

// <pattern identifier> -> <any identifier except ...>
Parser.prototype['pattern-identifier'] = function() {
    return this.rhs(
        {type: function(datum) {
            return datum.type === 'identifier' && datum.payload !== '...';
        }}
    );
};

// todo bl headless!
// <program> -> <command or definition>*
Parser.prototype['program'] = function() {
    return this.rhs(
        {type: 'command-or-definition', atLeast: 0}
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
        {type: ')'}
    );
};

Parser.prototype.parse = function(lhs) {
    var fun = this[lhs || 'expression'];
    if (fun)
        return fun.apply(this);
    else
        throw new InternalInterpreterError('unknown lhs: ' + lhs);
};