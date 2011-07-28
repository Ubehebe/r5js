function Parser(text) {
    this.text = text;
    this.textOffset = 0;
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
    this.curLhs = 'program';
}

Parser.prototype.nextToken = function() {
    while (this.nextTokenToReturn >= this.readyTokens.length) {
        var token = scanner.nextToken(this.text, this.textOffset);
        this.readyTokens.push(token);
        this.textOffset = token.offset;
    }
    return this.readyTokens[this.nextTokenToReturn++];
};

Parser.prototype.assertNextToken = function(predicate) {
    var n = this.nextToken();
    return predicate(n) ? n : this.fail(n, 'expected ' + predicate);
};

Parser.prototype.assertNextTokenValue = function(value) {
    var n = this.nextToken();
    return n.value === value ? n : this.fail(n, 'expected ' + value);
};

Parser.prototype.fail = function(token, msg) {
    return {fail: true, offset: token.offset, msg: msg || 'parse error'};
};

Parser.prototype.rhs = function() { // varargs
    var ans = {};
    var parseFunction;

    var oldLhs = this.curLhs;
    var tokenStreamStart = this.nextTokenToReturn;

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        /* Prefer an explicit node name, but if none is given, default
         to the type. This is to simplify rules like <command> -> <expression>
         where we want to parse an expression but have the node say it is a "command". */
        element.nodeName = element.nodeName || element.type;
        this.curLhs = (typeof element.type === 'function') ? element.nodeName : element.type; // todo bl cleanup
        var cur = (parseFunction = this[element.type]) // unfortunate the nonterminals share a namespace with other stuff
            ? this.handleNonterminal(ans, element, parseFunction)
            : this.handleTerminal(ans, element);
        if (cur.fail) {
            this.nextTokenToReturn = tokenStreamStart;
            this.curLhs = oldLhs;
            return cur;
        }
    }

    return ans;
};

Parser.prototype.handleNonterminal = function(ansBuffer, element, parseFunction) {

    // Handle * and +
    if (element.atLeast !== undefined) {
        var repeated = [];
        var rep;
        while (!(rep = parseFunction.apply(this)).fail)
            repeated.push(rep);

        if (repeated.length >= element.atLeast) {
            ansBuffer[element.nodeName] = repeated;
            return ansBuffer;
        } else {
            this.nextTokenToReturn -= repeated.length;
            return {fail: true, msg: 'expected at least '
                + element.atLeast + element.nodeName + ', got ' + repeated.length};
        }
    }

    // The normal case is exactly one of element.
    else {
        var parsed = parseFunction.apply(this);
        if (parsed.fail)
            return parsed;
        else {
            ansBuffer[element.nodeName] = parsed;
            return ansBuffer;
        }
    }
};

Parser.prototype.handleTerminal = function(ansBuffer, element) {


    // Note that we don't support + or * applied to terminals.
    // This is just because the grammar of Scheme doesn't require it.

    var token;
    // Usually, we want to check the string value of the next token.
    if (typeof element.type === 'string')
        token = this.assertNextTokenValue(element.type);
    // But in some situations, we check the next token against an arbitrary predicate.
    else if (typeof element.type === 'function') {
        token = this.assertNextToken(element.type);
    }

    if (!token.fail) {
        /* Most terminals, like ( and ), can be left out of the parse tree.
         But things like identifiers we need to remember.
         todo bl: are identifiers the *only* thing we need to remember?
         If so, we could dispense with rememberTerminalText. */
        if (element.rememberTerminalText)
            ansBuffer[element.nodeName] = token.value;
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
        else if (!mostInformativeError || possibleRhs.offset > mostInformativeError.offset)
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
    return this.rhs({type: function(token) {
            return token.tokenType === 'identifier'
                && !isSyntacticKeyword(token.value);
        }, nodeName: 'identifier', rememberTerminalText: true}
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
                return token.tokenType === 'boolean';
            }, nodeName: 'boolean', rememberTerminalText: true}
        ],
        [
            {type: function(token) {
                return token.tokenType === 'number';
            }, nodeName: 'number', rememberTerminalText: true}
        ],
        [
            {type: function(token) {
                return token.tokenType === 'string';
            }, nodeName: 'string', rememberTerminalText: true}
        ],
        [
            {type: function(token) {
                return token.tokenType === 'character';
            }, nodeName: 'character', rememberTerminalText: true}
        ]);
};

// <datum> -> <simple datum> | <compound datum>
Parser.prototype['datum'] = function() {
    return this.alternation(
        [
            {type: 'simple-datum'}
        ],
        [
            {type: 'compound-datum'}
        ]);
};

// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <symbol> -> <identifier>
Parser.prototype['simple-datum'] = function() {

    return this.rhs(
        {type: function(token) {
            switch (token.tokenType) {
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
        }, nodeName: 'text', rememberTerminalText: true});
};

// <compound datum> -> <list> | <vector>
Parser.prototype['compound-datum'] = function() {
    return this.alternation(
        [
            {type: 'list'}
        ],
        [
            {type: 'vector'}
        ]);
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
            switch (token.tokenType) {
                case "'":
                case '`':
                case '.':
                case ',@':
                    return true;
                default:
                    return false;
            }
        }, nodeName: 'abbrev-prefix', rememberTerminalText: true},
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
            {type: 'variable', atLeast: 0},
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
            {type: 'variable', atLeast: 0},
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

    /* The following breaks the parser due to the greediness of *:
     return this.rhs({type: 'expression', nodeName: 'command', atLeast: 0},
     {type: 'expression'});
     */

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
            return token.tokenType === 'identifier';
        }, nodeName: 'keyword', rememberTerminalText: true},
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
            return token.tokenType === 'identifier';
        }, nodeName: 'keyword', rememberTerminalText: true
        },
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
        {type: function(token) {
            return token.tokenType === 'identifier';
        },
            rememberTerminalText: true, atLeast: 0},
        {type: ')'},
        {type: 'syntax-rule', atLeast: 0}, // a nonterminal
        {type: ')'}
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
            return token.tokenType === 'identifier' && token.value !== '...';
        },
            rememberTerminalText: true
        }
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
            {type: 'expression', nodeName: 'command'}
        ],
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
        ]);
};

// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
Parser.prototype['syntax-definition'] = function() {
    return this.rhs(
        {type: '('},
        {type: 'define-syntax'},
        {type: 'identifier', nodeName: 'keyword', rememberTerminalText: true},
        {type: 'transformer-spec'},
        {type: ')'}
    );
};


Parser.prototype.parse = function(lhs) {
    return this[this.curLhs = (lhs || 'program')]();
};