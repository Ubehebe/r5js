function Token(type, start, stop) {
    this.type = type;
    this.start = start;
    this.stop = stop;
}

Token.prototype.setPayload = function(payload) {
    /* As a small optimization, we 'evaluate' these payloads here, rather than in
     semantic actions attached in the parser. This should be a little more efficient
     when creating self-evaluating datums on the fly. For example:

     (let-syntax ((foo (syntax-rules () ((foo x) (+ x x x x x x))))) (foo 24))

     As the macro facility currently works, this will create a datum corresponding
     to 24, clone it six times, and insert the datums as siblings into the datum tree,
     bypassing the scanner. It is less work to parse the string "24" into the number
     24 before the cloning than after. */
    switch (this.type) {
        case 'identifier':
            this.payload = payload.toLowerCase();
            break;
        case 'boolean':
            this.payload = payload === '#t' || payload === '#T';
            break;
        case 'number':
            this.payload = parseFloat(payload);
            break;
        case 'character':
            var afterSlash = payload.substr(2);
            if (afterSlash.length === 1)
                this.payload = afterSlash;
            else if (afterSlash === 'space')
                this.payload = ' ';
            else if (afterSlash === 'newline')
                this.payload = '\n';
            else throw new InternalInterpreterError('invalid character payload ' + payload);
            break;
        case 'string':
            this.payload = payload;
            break;
        default:
            throw new InternalInterpreterError('invalid token type ' + this.type);
    }
    return this;
};

/* 7.1.1: Tokens which require implicit termination (identifiers, numbers,
 characters, and dot) may be terminated by any <delimiter>, but not
 necessarily by anything else. */
Token.prototype.requiresDelimiterTermination = function() {
    switch (this.type) {
        case 'identifier':
        case 'number':
        case 'character':
        case '.':
            return true;
        default:
            return false;
    }
};

Token.prototype.shouldRememberPayload = function() {
    switch (this.type) {
        case 'identifier':
        case 'boolean':
        case 'number':
        case 'character':
        case 'string':
            return true;
        default:
            return false;
    }
};

function Scanner(text) {
    this.text = text;
    this.offset = 0;
    this.lastTypeScanned = null;
    this.payload = null;
    this.numberBase = null;
}

Scanner.prototype.nextToken = function() {

    // 7.1.1: <Intertoken space> may occur on either side of any token, but not within a token.
    while (!this.isEof() && this['intertoken-space']())
        ;

    var start = this.offset;

    if (!this.isEof() && this['token']()) {
        var token = new Token(this.lastTypeScanned, start, this.offset);
        if (token.shouldRememberPayload())
            token.setPayload(this.text.substr(start, this.offset - start));
        this.payload = null;
        if (token.requiresDelimiterTermination()
            && !this.isEof()
            && ' \n()";'.indexOf(this.text.charAt(this.offset)) === -1) {
            return null;
        } else return token;
    } else return null; // todo bl error reporting?
};

Scanner.prototype.tokenize = function() {
    var token;
    var ans = [];
    while (!this.isEof() && (token = this.nextToken()))
        ans.push(token);
    return ans;
};

Scanner.prototype.backupAndFail = function(returnTo) {
    this.offset = returnTo;
    return false;
};

Scanner.prototype.isEof = function() {
    return this.offset >= this.text.length;
};

Scanner.prototype.rhs = function() {

    var begin = this.offset;

    for (var i = 0; i < arguments.length; ++i) {
        var arg = arguments[i];

        if (arg.atLeast !== undefined) {
            if (!this.tryAtLeast(arg))
                return this.backupAndFail(begin);
        } else if (!this.tryOne(arg))
            return this.backupAndFail(begin);
    }

    return true;
};

Scanner.prototype.tryAtLeast = function(arg) {

    var atLeast = arg.atLeast;
    var endOfLastSuccess = this.offset;
    var numSuccess = 0;
    while (this.tryOne(arg)) {
        ++numSuccess;
        endOfLastSuccess = this.offset;
    }

    this.offset = endOfLastSuccess;

    return numSuccess >= atLeast;
};

Scanner.prototype.tryOne = function(arg) {

    // A token class specified by a string literal
    if (arg.is)
        return this.tryExactMatch(arg.is);

    // A token class specified by a function literal applying to the current character
    else if (arg.charPredicate)
        return this.tryCharPredicate(arg.charPredicate);

    // A token class specified by further rules
    else if (arg.type)
        return this.tryType(arg.type);
};

Scanner.prototype.tryExactMatch = function(exactString) {
    var s = this.text.substr(this.offset, exactString.length).toLowerCase();
    if (s === exactString) {
        this.offset += exactString.length;
        this.lastTypeScanned = exactString;
        return true;
    } else return false;
};

Scanner.prototype.tryCharPredicate = function(charPredicate) {
    var c = this.text.charAt(this.offset).toLowerCase();
    if (c.length === 1 && charPredicate(c)) {
        ++this.offset;
        return true;
    }
    else return false;
};

Scanner.prototype.tryType = function(type) {
    if (this[type]()) {
        this.lastTypeScanned = type;
        return true;
    } else return false;
};

Scanner.prototype.alternation = function() {
    for (var i = 0; i < arguments.length; ++i)
        if (this.rhs.apply(this, arguments[i]))
            return true;
    return false;
};

/* <token> -> <identifier> | <boolean> | <number> | <character>
 | <string> | ( | ) | #( | ' | ` | , | ,@ | .
 */
Scanner.prototype['token'] = function() {
    return this.alternation(
        [
            {type: 'number'} // must appear before identifier to catch leading +, -
        ],
        [
            {type: 'identifier'}
        ],
        [
            {type: 'boolean'}
        ],
        [
            {type: 'character'}
        ],
        [
            {type: 'string'}
        ],
        [
            {is: '('}
        ],
        [
            {is: ')'}
        ],
        [
            {is: '#('}
        ],
        [
            {is: "'"}
        ],
        [
            {is: '`'}
        ],
        [
            {is: ',@'} // must precede ',' rule below
        ],
        [
            {is: ','}
        ],
        [
            {is: '.'}
        ]
    );
};

// <delimiter> -> <whitespace> | ( | ) | " | ;
Scanner.prototype['delimiter'] = function() {
    return this.alternation(
        [
            {type: 'whitespace'}
        ],
        [
            {is: '('}
        ],
        [
            {is: ')'}
        ],
        [
            {is: '"'}
        ],
        [
            {is: ';'}
        ]
    );
};

// <whitespace> -> <space> | <newline>
Scanner.prototype['whitespace'] = function() {
    return this.alternation(
        [
            {is: ' '}
        ],
        [
            {is: '\n'}
        ]
    );
};

// <comment> -> ; <all subsequent characters up to a line break>
Scanner.prototype['comment'] = function() {
    return this.rhs(
        {is: ';'},
        {charPredicate: function(c) {
            return c !== '\n';
        }, atLeast: 0}
    );
};

// <atmosphere> -> <whitespace> | <comment>
Scanner.prototype['atmosphere'] = function() {
    return this.alternation(
        [
            {type: 'whitespace'}
        ],
        [
            {type: 'comment'}
        ]
    );
};

/* <intertoken space> -> <atmosphere>*
 (But I use atLeast 1, otherwise the scanner would report whitespace without
 consuming anything.) */
Scanner.prototype['intertoken-space'] = function() {
    return this.rhs(
        {type: 'atmosphere', atLeast: 1}
    );
};

// <identifier> -> <initial> <subsequent>* | <peculiar identifier>
Scanner.prototype['identifier'] = function() {
    return this.alternation(
        [
            {type: 'initial'},
            {type: 'subsequent', atLeast: 0}
        ],
        [
            {type: 'peculiar-identifier'}
        ]
    );
};

// <initial> -> <letter> | <special initial>
// <letter> -> a | b | ... | z | A | B | ... | Z
// <special initial> -> ! | $ | % | & | * | / | : | < | = | > | ? | ^ | _ | ~
Scanner.prototype['initial'] = function() {
    return this.alternation(
        [
            {charPredicate: function(c) {
                return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
            }}
        ],
        [
            {charPredicate: function(c) {
                return '!$%&*/:<=>?^_~'.indexOf(c) !== -1;
            }}
        ]
    );
};

// <subsequent> -> <initial> | <digit 10> | <special subsequent>
// <special subsequent> -> + | - | . | @
Scanner.prototype['subsequent'] = function() {
    return this.alternation(
        [
            {type: 'initial'}
        ],
        [
            {charPredicate: function(c) {
                return c >= '0' && c <= '9';
            }}
        ],
        [
            {charPredicate: function(c) {
                switch (c) {
                    case '+':
                    case '-':
                    case '.':
                    case '@':
                        return true;
                    default:
                        return false;
                }
            }}
        ]
    );

};

// <peculiar identifier> -> + | - | ...
Scanner.prototype['peculiar-identifier'] = function() {
    return this.alternation(
        [
            {is: '+'}
        ],
        [
            {is: '-'}
        ],
        [
            {is: '...'}
        ]
    );
};

// <boolean> -> #t | #f
Scanner.prototype['boolean'] = function() {
    return this.alternation(
        [
            {is: '#t'}
        ],
        [
            {is: '#f'}
        ]
    );
};

// <character> -> #\ <any character> | #\ <character name>
// <character name> -> space | newline
Scanner.prototype['character'] = function() {
    return this.alternation(
        [
            {is: '#\\'},
            {is: 'space'}
        ],
        [
            {is: '#\\'},
            {is: 'newline'}
        ],
        [
            {is: '#\\'},
            {charPredicate: function(c) {
                return true;
            }}
        ]
    );
};

// <string> -> " ⟨string element⟩* "
Scanner.prototype['string'] = function() {
    return this.rhs(
        {is: '"'},
        {type: 'string-element', atLeast: 0},
        {is: '"'}
    );
};

// <string element> -> <any character other than " or \> | \" | \\
Scanner.prototype['string-element'] = function() {
    return this.alternation(
        [
            {is: '\\"'}
        ],
        [
            {is: '\\\\'}
        ],
        [
            {charPredicate: function(c) {
                return c !== '"' && c !== '\\';
            }}
        ]
    );
};

// <number> -> <num 2> | <num 8> | <num 10> | <num 16>
Scanner.prototype['number'] = function() {
    return this.alternation(
        [
            {type: 'num-10'}
        ],
        [
            {type: 'num-16'}
        ],
        [
            {type: 'num-8'}
        ],
        [
            {type: 'num-2'}
        ]
    );
};

Scanner.prototype['num-2'] = function() {
    this.numberBase = 2;
    return this['num']();
};

Scanner.prototype['num-8'] = function() {
    this.numberBase = 8;
    return this['num']();
};

Scanner.prototype['num-10'] = function() {
    this.numberBase = 10;
    return this['num']();
};

Scanner.prototype['num-16'] = function() {
    this.numberBase = 16;
    return this['num']();
};

// <num R> -> <prefix R> <complex R>
// The prefix may be empty iff R = 10.
Scanner.prototype['num'] = function() {
    return this.numberBase === 10
        ? this.alternation(
        [
            {type: 'prefix'},
            {type: 'complex'}
        ],
        [
            {type: 'complex'}
        ]
    )
        : this.rhs(
        {type: 'prefix'},
        {type: 'complex'}
    );
};

/* <complex R> -> <real R>
 | <real R> @ <real R>
 | <real R> + <ureal R> i
 | <realR> - <ureal R> i
 | <real R> + i
 | <real R> - i
 | + <ureal R> i
 | - <ureal R> i
 | + i
 | - i
 */
Scanner.prototype['complex'] = function() {
    return this.alternation(
        [
            {type: 'real'},
            {is: '@'},
            {type: 'real'}
        ],
        [
            {type: 'real'},
            {is: '+'},
            {type: 'ureal'}
        ],
        [
            {type: 'real'},
            {is: '-'},
            {type: 'ureal'}
        ],
        [
            {type: 'real'},
            {is: '+'},
            {is: 'i'}
        ],
        [
            {type: 'real'},
            {is: '-'},
            {is: 'i'}
        ],
        [
            {type: 'real'}
        ],
        [
            {is: '+'},
            {type: 'ureal'},
            {is: 'i'}
        ],
        [
            {is: '-'},
            {type: 'ureal'},
            {is: 'i'}
        ],
        [
            {is: '+i'}
        ],
        [
            {is: '-i'}
        ]
    );
};

// <real R> -> <sign> <ureal R>
Scanner.prototype['real'] = function() {
    return this.alternation(
        [
            {type: 'sign'},
            {type: 'ureal'}
        ],
        [
            {type: 'ureal'}
        ]
    );
};

/* <ureal R> -> <uinteger R>
 | <uinteger R> / <uinteger R>
 | <decimal R> */
Scanner.prototype['ureal'] = function() {
    return this.alternation(
        [
            {type: 'decimal'}
        ],
        [
            {type: 'uinteger'},
            {is: '/'},
            {type: 'uinteger'}
        ],
        [
            {type: 'uinteger'}
        ]
    );
};

/* <decimal 10> -> <uinteger 10> <suffix>
 | <uinteger 10>
 | . <digit 10>+ #* <suffix>
 | . <digit 10>+ #*
 | <digit 10>+ . <digit 10>* #* <suffix>
 | <digit 10>+ . <digit 10>* #*
 | <digit 10>+ #+ . #* <suffix>
 | <digit 10>+ #+ . #*
 (In the standard there are 4 RHSes, but since <suffix> can be empty,
 we've doubled them.)
 */
Scanner.prototype['decimal'] = function() {
    if (this.numberBase !== 10)
        return false;
    else
        return this.alternation(
            [
                {is: '.'},
                {type: 'digit', atLeast: 1},
                {is: '#', atLeast: 0},
                {type: 'suffix'}
            ],
            [
                {is: '.'},
                {type: 'digit', atLeast: 1},
                {is: '#', atLeast: 0}
            ],
            [
                {type: 'digit', atLeast: 1},
                {is: '.'},
                {type: 'digit', atLeast: 0},
                {is: '#', atLeast: 0},
                {type: 'suffix'}
            ],
            [
                {type: 'digit', atLeast: 1},
                {is: '.'},
                {type: 'digit', atLeast: 0},
                {is: '#', atLeast: 0}
            ],
            [
                {type: 'digit', atLeast: 1},
                {is: '#', atLeast: 1},
                {is: '.'},
                {is: '#', atLeast: 0},
                {type: 'suffix'}
            ],
            [
                {type: 'digit', atLeast: 1},
                {is: '#', atLeast: 1},
                {is: '.'},
                {is: '#', atLeast: 0}
            ],
            [
                {type: 'uinteger'},
                {type: 'suffix'}
            ],
            [
                {type: 'uinteger'}
            ]
        );
};

// <uinteger R> -> <digit R>+ #*
Scanner.prototype['uinteger'] = function() {
    return this.rhs(
        {type: 'digit', atLeast: 1},
        {is: '#', atLeast: 0}
    );
};

/* <prefix R> -> <radix R> <exactness>
 | <exactness> <radix R>
 (If R is 10, the prefix may be empty.
 We have taken care of this in the <num> LHS.) */
Scanner.prototype['prefix'] = function() {

    if (this.numberBase === 10)
        return this.alternation(
            [
                {type: 'radix'},
                {type: 'exactness'}
            ],
            [
                {type: 'radix'}
            ],
            [
                {type: 'exactness'}
            ],
            [
                {type: 'exactness'},
                {type: 'radix'}
            ]
        );

    else return this.alternation(
        [
            {type: 'radix'},
            {type: 'exactness'}
        ],
        [
            {type: 'radix'}
        ],
        [
            {type: 'exactness'},
            {type: 'radix'}
        ]
    );
};

/* <suffix> -> <empty>
 | <exponent marker> <sign> <digit 10>+
 (We have taken care of the <empty> RHS in the <decimal> LHS.) */
Scanner.prototype['suffix'] = function() {
    return this.alternation(
        [
            {type: 'exponent-marker'},
            {type: 'sign'},
            {type: 'digit', atLeast: 1}
        ],
        [
            {type: 'exponent-marker'},
            {type: 'digit', atLeast: 1}
        ]
    );
};

// <exponent marker> -> e|s|f|d|l
Scanner.prototype['exponent-marker'] = function() {
    return this.rhs(
        {charPredicate: function(c) {
            switch (c) {
                case 'e':
                case 's':
                case 'f':
                case 'd':
                case 'l':
                    return true;
                default:
                    return false;
            }
        }
        }
    );
};

// <sign> -> <empty> | + | -
// (We have taken care of the <empty> RHS in <real> and <suffix> LHSes.)
Scanner.prototype['sign'] = function() {
    return this.rhs(
        {charPredicate: function(c) {
            switch (c) {
                case '+':
                case '-':
                    return true;
                default:
                    return false;
            }
        }
        }
    );
};

// <exactness> -> <empty> | #i | #e
// (We have taken care of the <empty> RHS in the <prefix> LHS.)
Scanner.prototype['exactness'] = function() {
    return this.alternation(
        [
            {is: '#i'}
        ],
        [
            {is: '#e'}
        ]
    );
};

/* <radix 2> -> #b
 <radix 8> -> #o
 <radix 10> -> <empty> | #d
 <radix 16> -> #x
 (We have taken care of the empty RHS of <radix 10> in the <prefix> LHS.)
 */
Scanner.prototype['radix'] = function() {
    switch (this.numberBase) {
        case 10:
            return this.rhs({is: '#d'});
        case 16:
            return this.rhs({is: '#x'});
        case 8:
            return this.rhs({is: '#o'});
        case 2:
            return this.rhs({is: '#b'});
        default:
            throw new InternalInterpreterError('illegal number base ' + this.numberBase);
    }
};

/* <digit 2> -> 0 | 1
 <digit 8> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
 <digit 10> -> <digit>
 <digit 16> -> <digit 10> | a | b | c | d | e | f
 */
Scanner.prototype['digit'] = function() {
    switch (this.numberBase) {
        case 10:
            return this.rhs({charPredicate: function(c) {
                return c >= '0' && c <= '9';
            }
            });
        case 16:
            return this.rhs({charPredicate: function(c) {
                return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f');
            }
            });
        case 8:
            return this.rhs({charPredicate: function(c) {
                return c >= '0' && c <= '7';
            }
            });
        case 2:
            return this.rhs({charPredicate: function(c) {
                return c === '0' || c === '1';
            }
            });
        default:
            throw new InternalInterpreterError('illegal number base ' + this.numberBase);
    }
};

