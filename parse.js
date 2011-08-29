function Parser(text) {
    this.scanner = new Scanner(text);
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
}

Parser.prototype.nextToken = function() {
    while (this.nextTokenToReturn >= this.readyTokens.length) {
        var token = this.scanner.nextToken();
        if (!token)
            return null;
        this.readyTokens.push(token);
    }
    return this.readyTokens[this.nextTokenToReturn++];
};

Parser.prototype.assertNextToken = function(predicate) {
    var token = this.nextToken();
    if (!token) return this.eof();
    return predicate(token) ? token : this.fail(token, 'expected ' + predicate);
};

Parser.prototype.assertNextTokenPayload = function(payload) {
    var token = this.nextToken();
    if (!token) return this.eof();
    // Tokens like "define" have token.payload = "define", token.type = "identifier"
    // Tokens like "(" have token.type = "(" and no token.payload
    var compareTo = token.payload || token.type;
    return compareTo === payload ? token : this.fail(token, 'expected ' + payload);
};

Parser.prototype.eof = function() {
    return {fail: true, stop: this.scanner.offset, msg: 'eof'}
};

Parser.prototype.fail = function(token, msg) {
    return {fail: true, stop: token.stop, msg: msg || 'parse error'};
};

Parser.prototype.rhs = function() {
    var ans = {};
    var parseFunction;
    var tokenStreamStart = this.nextTokenToReturn;

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        /* Prefer an explicit node name, but if none is given, default
         to the type. This is to simplify rules like <command> -> <expression>
         where we want to parse an expression but have the node say it is a "command". */
        element.nodeName = element.nodeName || element.type;
        var cur = (parseFunction = this[element.type]) // unfortunate the nonterminals share a namespace with other stuff
            ? this.onNonterminal(ans, element, parseFunction)
            : this.onTerminal(ans, element);
        if (cur.fail) {
            this.nextTokenToReturn = tokenStreamStart;
            return cur;
        }
    }

    return ans;
};

Parser.prototype.onNonterminal = function(ansBuffer, element, parseFunction) {

    // Handle * and +
    if (element.atLeast !== undefined) { // explicit undefined since atLeast 0 should be valid
        var repeated = [];
        var rep;
        while (!(rep = parseFunction.apply(this)).fail) {
            rep.type = element.type;
            repeated.push(rep);
        }

        if (repeated.length >= element.atLeast) {
            ansBuffer[element.nodeName] = repeated;
            return ansBuffer;
        } else {
            this.nextTokenToReturn -= repeated.length;
            return {fail: true, msg: 'expected at least '
                + element.atLeast + ' ' + element.nodeName + ', got ' + repeated.length};
        }
    }

    // The normal case is exactly one of element.
    else {
        var parsed = parseFunction.apply(this);
        if (parsed.fail)
            return parsed;
        else {
            parsed.type = element.type;
            ansBuffer[element.nodeName] = parsed;
            return ansBuffer;
        }
    }
};

Parser.prototype.onTerminal = function(ansBuffer, element) {
    var token;
    // Usually, we want to check the string value of the next token.
    if (typeof element.type === 'string')
        token = this.assertNextTokenPayload(element.type);
    // But in some situations, we check the next token against an arbitrary predicate.
    else if (typeof element.type === 'function') {
        token = this.assertNextToken(element.type);
    }

    if (!token.fail) {
        if (token.payload && !isSyntacticKeyword(token.payload))
            ansBuffer[element.nodeName] = token.payload;
        // Only the first terminal in a rule should record this
        if (ansBuffer.start === undefined)
            ansBuffer.start = token.start;
        // Only the last terminal in a rule should record this
        ansBuffer.stop = token.stop;
        return ansBuffer;
    } else return token;
};

Parser.prototype.alternation = function() {
    var possibleRhs;
    // The most informative error is probably the failed parse
    // that got furthest through the input.
    var mostInformativeError = null;
    for (var i = 0; i < arguments.length; ++i) {
        possibleRhs = this.rhs.apply(this, arguments[i]);
        if (!possibleRhs.fail)
            return possibleRhs;
        else if (!mostInformativeError || possibleRhs.stop > mostInformativeError.stop)
            mostInformativeError = possibleRhs;
    }
    return mostInformativeError;
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
        ],
        [
            {type: 'derived-expression'} // todo bl!!!
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
    return this.rhs({type: function(token) {
            return token.type === 'identifier'
                && !isSyntacticKeyword(token.payload);
        }, nodeName: 'identifier'}
    );
};

// <literal> -> <quotation> | <self-evaluating>
Parser.prototype['literal'] = function() {
    return this.alternation(
        [
            {type: 'quotation'}
        ],
        [
            {type: 'self-evaluating'}
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

    return this.alternation(
        [
            {type: function(token) {
                return token.type === 'boolean';
            }, nodeName: 'boolean'}
        ],
        [
            {type: function(token) {
                return token.type === 'number';
            }, nodeName: 'number'}
        ],
        [
            {type: function(token) {
                return token.type === 'string';
            }, nodeName: 'string'}
        ],
        [
            {type: function(token) {
                return token.type === 'character';
            }, nodeName: 'character'}
        ]);
};

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
Parser.prototype['datum'] = function() {
    return this.alternation(
        [
            {type: function(token) {
                switch (token.type) {
                    case 'identifier':
                    case 'boolean':
                    case 'number':
                    case 'character':
                    case 'string':
                    case 'symbol':
                        return true;
                    default:
                        return false;
                }
            }, nodeName: 'text'}
        ],
        [
            {type: 'list'}
        ],
        [
            {type: 'vector'}
        ])
};

// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
Parser.prototype['list'] = function() {

    return this.alternation(
        [
            {type: '('},
            {type: 'datum', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 1},
            {type: '.'},
            {type: 'datum', nodeName: '.datum'},
            {type: ')'}
        ],
        [
            {type: 'abbreviation'}
        ]);
};

// <vector> -> #(<datum>*)
Parser.prototype['vector'] = function() {

    return this.rhs(
        {type: '#('},
        {type: 'datum', atLeast: 0},
        {type: ')'}
    );
};

// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
// todo bl i have never used these abbreviations but they have to do with quasiquotation
Parser.prototype['abbreviation'] = function() {

    return this.rhs(
        {type: function(token) {
            switch (token.type) {
                case "'":
                case '`':
                case ',':
                case ',@':
                    return true;
                default:
                    return false;
            }
        }, nodeName: 'abbrev-prefix'},
        {type: 'datum'}
    );
};

// <procedure call> -> (<operator> <operand>*)
// <operator> -> <expression>
// <operand> -> <expression>
Parser.prototype['procedure-call'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'expression', nodeName: 'operator'},
        {type: 'expression', nodeName: 'operand', atLeast: 0},
        {type: ')'});
};

// <lambda expression> -> (lambda <formals> <body>)
Parser.prototype['lambda-expression'] = function() {

    return this.rhs(
        {type: '('},
        {type: 'lambda'},
        {type: 'formals'},
        {type: 'body'},
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
            {type: 'variable', nodeName: '.variable'},
            {type: ')'}
        ]);
};

// <body> -> <definition>* <sequence>
Parser.prototype['body'] = function() {

    return this.rhs(
        {type: 'definition', atLeast: 0},
        {type: 'sequence'});
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
            {type: 'variable', nodeName: '.variable'},
            {type: ')'},
            {type: 'body'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'define'},
            {type: '('},
            {type: 'variable', atLeast: 1},
            {type: ')'},
            {type: 'body'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'begin'},
            {type: 'definition', atLeast: 0},
            {type: ')'}
        ]);

};


// <sequence> -> <command>* <expression>
// <command> -> <expression>
Parser.prototype['sequence'] = function() {
    return this.rhs(
        {type: 'expression', nodeName: 'command+expr', atLeast: 1});
};

// <conditional> -> (if <test> <consequent> <alternate>)
// <test> -> <expression>
// <consequent> -> <expression>
// <alternate> -> <expression> | <empty>
Parser.prototype['conditional'] = function() {

    return this.alternation(
        [
            {type: '('},
            {type: 'if'},
            {type: 'expression', nodeName: 'test'},
            {type: 'expression', nodeName: 'consequent'},
            {type: 'expression', nodeName: 'alternate'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'if'},
            {type: 'expression', nodeName: 'test'},
            {type: 'expression', nodeName: 'consequent'},
            {type: ')'}
        ]);
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
// <keyword> -> <identifier>
Parser.prototype['macro-use'] = function() {

    return this.rhs(
        {type: '('},
        {type: function(token) {
            return token.type === 'identifier';
        }, nodeName: 'keyword'},
        {type: 'datum', atLeast: 0},
        {type: ')'});
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
            {type: 'body'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'letrec-syntax'},
            {type: '('},
            {type: 'syntax-spec', atLeast: 0},
            {type: ')'},
            {type: 'body'},
            {type: ')'}
        ]);
};

// <syntax spec> -> (<keyword> <transformer spec>)
// <keyword> -> <identifier>
Parser.prototype['syntax-spec'] = function() {
    return this.rhs(
        {type: '('},
        {type: function(token) {
            return token.type === 'identifier';
        }, nodeName: 'keyword'},
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
         This would require writing token stream backup logic that is built in to
         onNonterminal. I decided it was easier to add a vacuous nonterminal
         'transformer-spec-identifier' to the grammar, so we can reuse the token
         stream backup logic. */
        {type: 'transformer-spec-identifier', atLeast: 0},
        {type: ')'},
        {type: 'syntax-rule', atLeast: 0}, // a nonterminal
        {type: ')'}
    );
};

Parser.prototype['transformer-spec-identifier'] = function() {
    return this.rhs(
        {type: function(token) {
            return token.type === 'identifier';
        },
            nodeName: 'identifier'}
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
            {type: 'pattern', nodeName: '.pattern'},
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
    return this['self-evaluating'](); // any reason not to reuse this?
};

/* <template> -> <pattern identifier>
 | (<template element>*)
 | (<template element>+ . <template>)
 | #(<template element>*)
 | <template datum>
 <template datum> -> <pattern datum>
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
            {type: 'template', nodeName: '.template'},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'template-element', atLeast: 0},
            {type: ')'}
        ],
        [
            {type: 'pattern-datum', nodeName: 'template-datum'}
        ]
    );
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
        {type: function(token) {
            return token.type === 'identifier' && token.payload!== '...';
        },
            nodeName: 'identifier'}
    );
};

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
 <command> -> <expression>
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
            {type: 'expression', nodeName: 'command'}
        ]);
};

// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
Parser.prototype['syntax-definition'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'define-syntax'},
        {type: function(token) {
            return token.type === 'identifier';
        },
            nodeName: 'keyword'},
        {type: 'transformer-spec'},
        {type: ')'}
    );
};

Parser.prototype.parse = function(lhs) {
    var fun = this[lhs || 'program'];
    if (fun)
        return fun.apply(this);
    else
        throw new InternalInterpreterError('unknown lhs: ' + lhs);
};