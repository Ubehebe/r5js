function Parser(text) {
    this.text = text;
    this.textOffset = 0;
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
    this.curLhs = 'expression'; // 'program'?
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
    console.log('READ TOKEN ' + n.value);
    return predicate(n) ? n : this.fail(n, 'expected ' + predicate);
};

Parser.prototype.assertNextTokenValue = function(value) {
    var n = this.nextToken();
    console.log('READ TOKEN ' + n.value);
    return n.value === value ? n : this.fail(n, 'expected ' + value);
};

Parser.prototype.fail = function(token, msg) {
    return {fail: true, offset: token.offset, msg: msg || 'parse error'};
};

Parser.prototype.rhs = function() { // varargs
    var ans = {type: this.curLhs};
    var parseFunction;

    var oldLhs = this.curLhs;

    var tokenStreamStart = this.nextTokenToReturn;

    console.log(this.curLhs + ' ->');

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        /* Prefer an explicit node name, but if none is given, default
         to the type. This is to simplify rules like <command> -> <expression>
         where we want to parse an expression but have the node say it is a "command".
         */
        element.nodeName = element.nodeName || element.type;
        console.log('-> ' + element.nodeName + '?')
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
            return {success: false, msg: 'expected at least '
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

    console.log('handleTerminal ' + element.nodeName);

    // Note that we don't support + or * applied to terminals.
    // This is just because the grammar of Scheme doesn't require it.

    var token;
    // Usually, we want to check the string value of the next token.
    if (typeof element.type === 'string')
        token = this.assertNextTokenValue(element.type);
    // But in some situations, we check the next token against an arbitrary predicate.
    else if (typeof element.type === 'function')
        token = this.assertNextToken(element.type);

    if (!token.fail) {
        console.log('success!');
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
    for (var i = 0; i < arguments.length; ++i) {
        possibleRhs = this.rhs.apply(this, arguments[i]); // todo bl possible varargs problem
        if (!possibleRhs.fail)
            return possibleRhs;
    }
    return possibleRhs;
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
        ]);
    /*  [
     {type: 'macro-block'}
     ]);*/
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

    return this.alternation(
        [
            {type: 'boolean', rememberTerminalText: true}
        ],
        [
            {type: 'number', rememberTerminalText: true}
        ],
        [
            {type: 'character', rememberTerminalText: true}
        ],
        [
            {type: 'string', rememberTerminalText: true}
        ],
        [
            {type: 'identifier', nodeName: 'symbol', rememberTerminalText: true}
        ]);
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
            {type: 'datum'},
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
            {type: 'variable'},
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
            {type: 'variable'},
            {type: 'def-formals'},
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

Parser.prototype.parse = function() {
    return this[this.curLhs = 'expression']();
};