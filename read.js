function Datum() {
}

function Reader(text) {
    this.scanner = new Scanner(text);
    this.readyTokens = [];
    this.nextTokenToReturn = 0;
    this.errorToken = null;
    this.errorMsg = '';
}

Reader.prototype.nextToken = function() {
    while (this.nextTokenToReturn >= this.readyTokens.length) {
        var token = this.scanner.nextToken();
        if (!token)
            return null;
        this.readyTokens.push(token);
    }
    return this.readyTokens[this.nextTokenToReturn++];
};

Reader.prototype.assertNextToken = function(predicate) {
    var token = this.nextToken();
    if (!token) {
        this.errorMsg = 'eof';
        return null;
    }
    if (predicate(token)) {
        return token;
    } else {
        this.errorToken = token;
        this.errorMsg = 'expected ' + predicate;
        return null;
    }
};

Reader.prototype.assertNextTokenPayload = function(payload) {
    var token = this.nextToken();
    if (!token) {
        this.errorMsg = 'eof';
        return null;
    }
    // Tokens like "define" have token.payload = "define", token.type = "identifier"
    // Tokens like "(" have token.type = "(" and no token.payload
    var compareTo = token.payload || token.type;
    if (compareTo === payload) {
        return token;
    } else {
        this.errorToken = token;
        this.errorMsg = 'expected ' + payload;
        return null;
    }
};

Reader.prototype.rhs = function() {
    var ansDatum = new Datum();
    var parseFunction;
    var tokenStreamStart = this.nextTokenToReturn;

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];
        /* Prefer an explicit node name, but if none is given, default
         to the type. This is to simplify rules like <command> -> <expression>
         where we want to parse an expression but have the node say it is a "command". */
        element.nodeName = element.nodeName || element.type;
        var cur = (parseFunction = this[element.type]) // unfortunate the nonterminals share a namespace with other stuff
            ? this.onNonterminal(ansDatum, element, parseFunction)
            : this.onTerminal(ansDatum, element);
        if (!cur) {
            this.nextTokenToReturn = tokenStreamStart;
            return null;
        }
    }

    return ansDatum;
};

Reader.prototype.onNonterminal = function(ansDatum, element, parseFunction) {

    // Handle * and +
    if (element.atLeast !== undefined) { // explicit undefined since atLeast 0 should be valid
        var repeated = [];
        var rep;
        while (rep = parseFunction.apply(this)) {
            repeated.push(rep);
        }

        if (repeated.length >= element.atLeast) {
            ansDatum[element.nodeName] = repeated;
            return ansDatum;
        } else {
            this.nextTokenToReturn -= repeated.length;
            this.errorMsg = 'expected at least '
                + element.atLeast + ' ' + element.nodeName + ', got ' + repeated.length;
            return null;
        }
    }

    // The normal case is exactly one of element.
    else {
        var parsed = parseFunction.apply(this);
        if (!parsed)
            return parsed;
        else {
            ansDatum[element.nodeName] = parsed;
            return ansDatum;
        }
    }
};

Reader.prototype.onTerminal = function(ansDatum, element) {
    var token;
    // Usually, we want to check the string value of the next token.
    if (typeof element.type === 'string')
        token = this.assertNextTokenPayload(element.type);
    // But in some situations, we check the next token against an arbitrary predicate.
    else if (typeof element.type === 'function') {
        token = this.assertNextToken(element.type);
    }

    if (token) {
        if (token.payload)
            ansDatum[element.nodeName] = token.payload;
        // Only the first terminal in a rule should record this
        /*if (ansDatum.start === undefined)
         ansDatum.start = token.start;
         // Only the last terminal in a rule should record this
         ansDatum.stop = token.stop;*/
        return ansDatum;
    } else return null;
};

Reader.prototype.alternation = function() {
    var possibleRhs;
    // The most informative error is probably the failed parse
    // that got furthest through the input.
    var mostInformativeErrorToken = null;
    var mostInformationErrorMsg = null;
    for (var i = 0; i < arguments.length; ++i) {
        possibleRhs = this.rhs.apply(this, arguments[i]);
        if (possibleRhs)
            return possibleRhs;
        else if (!mostInformativeErrorToken
            || (this.errorToken && this.errorToken.stop > mostInformativeErrorToken.stop)) {
            mostInformativeErrorToken = this.errorToken;
            mostInformationErrorMsg = this.errorMsg;
        }
    }

    this.errorToken = mostInformativeErrorToken;
    this.errorMsg = mostInformationErrorMsg;
    return null;
};

// <datum> -> <simple datum> | <compound datum>
// <simple datum> -> <boolean> | <number> | <character> | <string> | <symbol>
// <compound datum> -> <list> | <vector>
// <symbol> -> <identifier>
// <list> -> (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <vector> -> #(<datum>*)
// <abbreviation> -> <abbrev prefix> <datum>
// <abbrev prefix> -> ' | ` | , | ,@
Reader.prototype['datum'] = function() {
    return this.alternation(
        [
            {type: function(token) {
                return token.type == 'identifier';
            }, nodeName: 'identifier'}
        ],
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
                return token.type === 'character';
            }, nodeName: 'character'}
        ],
        [
            {type: function(token) {
                return token.type === 'string';
            }, nodeName: 'string'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 0, nodeName: 'list'},
            {type: ')'}
        ],
        [
            {type: '('},
            {type: 'datum', atLeast: 1, nodeName: 'list'},
            {type: '.'},
            {type: 'datum', nodeName: '.list'},
            {type: ')'}
        ],
        [
            {type: '#('},
            {type: 'datum', atLeast: 0, nodeName: 'vector'},
            {type: ')'}
        ],
        [
            {type: function(token) {
                switch (token.type) {
                    // todo bl will need to remember these payloads!
                    case "'":
                    case '`':
                    case ',':
                    case ',@':
                        return true;
                    default:
                        return false;
                }
            }},
            {type: 'datum', nodeName: 'abbrev'}
        ]
    );
};

Reader.prototype.read = function() {
    ans = this['datum']();
    return ans ? ans : {token: this.errorToken, msg: this.errorMsg};
};