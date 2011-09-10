function Parser(root) {
    this.root = root;
    this.prev = null;
    this.next = root;
}

Parser.prototype.rhs = function() {
    var parseFunction;

    var root = this.next;

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
    for (var i = 0; i < arguments.length; ++i)
        if (possibleRhs = this.rhs.apply(this, arguments[i]))
            return possibleRhs;
    return null;
};

Parser.prototype.rewriteImproperList = function(rhsArgs) {
    if (rhsArgs.length < 1 || rhsArgs[0].type !== '(')
        return;

    for (var i = 1; i < rhsArgs.length - 1; ++i) {
        if (rhsArgs[i].type === '.') {
            rhsArgs[0].type = '.(';
            rhsArgs[i + 1].type = '.';
            return;
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
            case '(':
                return this.advanceToChildIf(function(datum) {
                    return datum.type === 'list';
                });
            case '.(':
                return this.advanceToChildIf(function(datum) {
                    return datum.type === 'improper-list';
                });
            case '#(':
                return this.advanceToChildIf(function(datum) {
                    return datum.type === 'vector';
                });
            case '.':
                return true; // vacuous; we already rewrote ( ... . as .( ...
            case "'":
            case '`':
            case ',':
            case ',@':
                return this.advanceToChildIf(function(datum) {
                    return datum.type === element.type;
                });
            case ')':
                if (!this.next) {
                    // todo bl YIKES (to deal with empty lists)
                    if (!this.prev.nextSibling)
                        this.next = this.prev.parent.nextSibling;
                    else
                        this.next = this.prev.nextSibling;
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
    var kws = ['else', '=>', 'define', 'unquote', 'unquote-splicing', 'quote', 'lambda',
        'if', 'set!', 'begin', 'cond', 'and', 'or', 'case', 'let', 'let*', 'letrec', 'do',
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
        ]/*,
         [
         {type: 'derived-expression'} // todo bl!!!
         ],*/
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
            {type: function(datum) {
                return true;
            } }
        ],
        [
            {type: '('},
            {type: 'quote'},
            {type: function(datum) {
                return true;
            } },
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
// <sequence> -> <command>* <expression>
// <command> -> <expression>
Parser.prototype['lambda-expression'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'lambda'},
        {type: 'formals'},
        {type: 'definition', atLeast: 0},
        {type: 'expression', atLeast: 1},
        {type: ')'});
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

// <macro use> -> (<keyword> <datum>*)
Parser.prototype['macro-use'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'keyword'},
        {type: function(datum) {
            return true;
        }, atLeast: 0},
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
            {type: 'template'}, // todo bl: uh oh. Only spot in grammar where (A+ . B), A != B. Unsupported.
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