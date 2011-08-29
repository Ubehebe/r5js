function Token(type, start, stop) {
    this.type = type;
    this.start = start;
    this.stop = stop;
}

Token.prototype.setPayload = function(payload) {
    this.payload = payload;
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

Token.prototype.rememberPayload = function() {
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
        if (token.rememberPayload())
            token.setPayload(this.text.substr(start, this.offset-start));
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
    var before = this.offset;
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
            {type: 'identifier'}
        ],
        [
            {type: 'boolean'}
        ],
        [
            {type: 'number'}
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

/* <syntactic keyword> -> <expression keyword> | else | => | define
 | unquote | unquote-splicing */
Scanner.prototype['syntactic-keyword'] = function() {
    return this.alternation(
        [
            {type: 'expression-keyword'}
        ],
        [
            {is: 'else'}
        ],
        [
            {is: '=>'}
        ],
        [
            {is: 'define'}
        ],
        [
            {is: 'unquote'}
        ],
        [
            {is: 'unquote-splicing'}
        ]
    );
};

// <expression keyword> -> quote | lambda | if | set! | begin | cond | and | or
// | case | let | let* | letrec | do | delay | quasiquote
Scanner.prototype['expression-keyword'] = function() {
    return this.alternation(
        [
            {is: 'quote'}
        ],
        [
            {is: 'lambda'}
        ],
        [
            {is: 'if'}
        ],
        [
            {is: 'set!'}
        ],
        [
            {is: 'begin'}
        ],
        [
            {is: 'cond'}
        ],
        [
            {is: 'and'}
        ],
        [
            {is: 'or'}
        ],
        [
            {is: 'case'}
        ],
        [
            {is: 'let'}
        ],
        [
            {is: 'let*'}
        ],
        [
            {is: 'letrec'}
        ],
        [
            {is: 'do'}
        ],
        [
            {is: 'delay'}
        ],
        [
            {is: 'quasiquote'}
        ]
    );
};

// <variable> -> <any <identifier> that isn’t also a <syntactic keyword>>
// bl belongs in parser? Scanner.prototype['variable']

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


var scanner = {}; // todo bl make object-oriented

scanner.nextToken = function(text, offset) {

    var scan = {};


    scan['token'] = function(text, offset) {

        var unigraph = text.charAt(offset);
        var digraph;

        /* The dot has to be handled differently because it can also be a decimal
         point or the beginning of an ellipsis. We have to peek ahead with
         requireTokenEnd() to figure out what to do. */
        if (unigraph === '.') {
            var maybeDot = requireTokenEnd(unigraph, unigraph, text, offset + 1);
            if (maybeDot.success)
                return maybeDot;
            /* If it is not the end of the token, fall through to below to try the
             decimal point and ellipsis interpretations. */


        } else if ("()'`,".indexOf(unigraph) !== -1) {
            return scanOk(unigraph, unigraph, offset + 1);
        } else if ((digraph = text.substr(offset, 2)) === '#(' || digraph === ',@') {
            return scanOk(digraph, digraph, offset + 2);
        }

        // no "else": we need to catch the fallthrough from above.

        var ans;

        // order by common case
        var toTry = ['identifier', 'boolean', 'character', 'string', 'number'];

        for (var i = 0; i < toTry.length; ++i)
            if ((ans = scan[toTry[i]](text, offset)).success)
                return ans;

        return ans;
    };


    scan['identifier'] = function(text, offset) {

        // <initial> -> <letter> | <special initial>
        var validInitial = function(c) {
            return c.length === 1
                && ((c >= 'a' && c <= 'z') // <letter> -> a | b | ... | z | A | B | ... | Z
                || (c >= 'A' && c <= 'Z')
                || '!$%&*/:<=>?^_~'.indexOf(c) !== -1); // <special initial>
        };

        // <subsequent> -> <initial> | <digit 10> | <special subsequent>
        var validSubsequent = function(c) {
            return c.length === 1
                && (validInitial(c)
                || (c >= '0' && c <= '9')
                || '+-.@'.indexOf(c) !== -1); // special subsequents
        };

        if (!validInitial(text.charAt(offset)))
            return scan['peculiar-identifier'](text, offset);

        var subsequentEnd = consumeWhile(text, offset + 1, validSubsequent);

        return requireTokenEnd('identifier', text.substr(offset, subsequentEnd - offset), text, subsequentEnd);
    };

    // <peculiar identifier> -> + | - | ...
    // Not clear to me whether the ... is an admissible peculiar identifier or
    // just an ellipsis.
    scan['peculiar-identifier'] = function(text, offset) {
        var maybePeculiar = text.charAt(offset);

        // we return type 'identifier' not 'peculiar-identifier'
        if (maybePeculiar === '+' || maybePeculiar === '-')
            return requireTokenEnd('identifier', maybePeculiar, text, offset + 1);
        /* todo bl: unreachable. we will always parse '...' as type '.' first.
         the grammar is ambiguous: is '...' an actual peculiar identfier or
         an ellipsis denoting an open class of peculiar identifiers? */
        else if (text.substr(offset, 3) === '...')
            return requireTokenEnd('identifier', '...', text, offset + 3);
        else
            return scanError('identifier', offset);
    };


    // <boolean> -> #t | #f
    /* Since booleans are self-evaluating, we go ahead and convert them
     to JavaScript booleans here, in the scanner. */
    scan['boolean'] = function(text, offset) {
        var maybeBool = text.substr(offset, 2);
        switch (maybeBool) {
            case '#t':
                return scanOk('boolean', true, offset + 2);
            case '#f':
                return scanOk('boolean', false, offset + 2);
            default:
                return scanError('boolean', offset);
        }
    };

    // <character> -> #\ <any character> | #\ <character name>
    // <character name> -> space | newline
    /* Since characters are self-evaluating, we go ahead and convert them
     to JavaScript strings here, in the scanner. */
    scan['character'] = function(text, offset) {
        var requiredPrefix = text.substr(offset, 2);
        if (requiredPrefix !== '#\\')
            return scanError('character', offset, "invalid character literal prefix " + requiredPrefix);
        else if (text.substr(offset, 7) === "#\\space")
            return requireTokenEnd('character', ' ', text, offset + 7);
        else if (text.substr(offset, 9) === "#\\newline")
            return requireTokenEnd('character', '\n', text, offset + 9);
        else
            return requireTokenEnd('character', text.charAt(offset + 2), text, offset + 3);
    };

    // <string> -> " <string element>* "
    scan['string'] = function(text, offset) {
        if (text.charAt(offset) !== '"')
            return scanError('string', offset, 'expected "');

        /* We can't use the generic consumeWhile() because of the valid
         digraphs \" and \\ */
        var len = 0;
        while (validStringElement(text, offset + ++len))
            ;

        return (text.charAt(offset + len) === '"')
            ? scanOk('string', text.substr(offset + 1, len - 1), offset + len + 1)
            : scanError('string', offset, 'unterminated string literal');

        // <string element> -> <any character other than " or \> | \" | \\
        function validStringElement(text, offset) {
            var cur = text.charAt(offset);
            if (cur.length === 1 && cur !== '"' && cur !== '\\')
                return true;
            else return (cur = text.substr(offset, 2)) === '\\"'
                || cur === '\\\\';
        }
    };

    // <number> -> <num 2> | <num 8> | <num 10> | <num 16>
    /* Since numbers are self-evaluating, we should go ahead and represent
     them as JavaScript numbers here, in the scanner. In simple cases we
     can parse them as native JavaScript numbers, but there are more complex
     cases. */
    scan['number'] = function(text, offset) {
        var bases = [10,16,8,2]; // order by common case
        var ans;
        for (var i = 0; i < bases.length; ++i) {
            if ((ans = scan['num'](bases[i], text, offset)).success) {
                return requireTokenEnd('number', ans.value, text, ans.offset);
            }
        }
        return ans;
    };

    // <num R> -> <prefix R> <complex R>
    scan['num'] = function(base, text, offset) {
        var prefix = scan['prefix'](base, text, offset);
        if (!prefix.success)
            return prefix;
        var complex = scan['complex'](base, text, prefix.offset);
        return complex.success
            ? requireTokenEnd('num', prefix.value + complex.value, text, complex.offset)
            : complex;
    };

    /*
     <complex R> -> <real R>
     | <real R> @ <real R>
     | <real R> + <ureal R> i
     | <real R> - <ureal R> i
     | <real R> + i
     | <real R> - i
     | + <ureal R> i
     | - <ureal R> i
     | + i
     | - i
     */
    scan['complex'] = function(base, text, offset) {


        var sign = scan['sign'](text, offset);
        if (!sign.success)
            return sign;

        // All the rules beginning with <real R> on the RHS
        else if (sign.value === '') {
            var real1 = scan['real'](base, text, offset);
            if (!real1.success)
                return real1;
            var maybeOp = text.charAt(real1.offset);
            if (maybeOp.length === 1 && '+-@'.indexOf(maybeOp) !== -1) {

                // <complex R> -> <real R> @ <real R>
                if (maybeOp === '@') {
                    var real2 = scan['real'](base, text, real1.offset + 1);
                    return real2.success
                        ? requireTokenEnd('complex', real1.value + '@' + real2.value, text, real2.offset)
                        : real2;
                }

                // <complex R> -> <real R> + i | <real R> - i
                else if (text.charAt(real1.offset + 1) === 'i' || text.charAt(real1.offset + 1) === 'I') {
                    return requireTokenEnd('complex', real1.value + maybeOp + 'i', text, real1.offset + 2);
                }

                // <complex R> -> <real R> + <ureal R> i | <real R> - <ureal R> i
                else {
                    var mustBeUrealAfterOp = scan['ureal'](base, text, real1.offset + 1);
                    if (!mustBeUrealAfterOp.success)
                        return mustBeUrealAfterOp;
                    var mustBeIAfterUreal = text.charAt(mustBeUrealAfterOp.offset);
                    return mustBeIAfterUreal === 'i' || mustBeIAfterUreal === 'I'
                        ? requireTokenEnd('complex', real1.value + maybeOp + mustBeUrealAfterOp.value + 'i', text, mustBeUrealAfterOp.offset + 1)
                        : scanError('complex', mustBeUrealAfterOp.offset, 'expected i');
                }
            } else return requireTokenEnd('complex', real1.value, text, real1.offset); // <complex R> -> <real R>
        }

        //  <complex R> -> + i | - i
        else if (text.charAt(sign.offset) === 'i' || text.charAt(sign.offset) === 'I') {
            return requireTokenEnd('complex', sign.value + 'i', text, sign.offset + 1);
        }

        // <complex R> -> + <ureal R> i | - <ureal R> i
        else {
            var mustBeUreal = scan['ureal'](base, text, sign.offset);
            if (!mustBeUreal.success)
                return mustBeUreal;
            var mustBeI = text.charAt(text, mustBeUreal.offset)
            return (mustBeI === 'i' || mustBeI === 'I')
                ? requireTokenEnd('complex', sign.value + mustBeUreal.value + 'i', text, mustBeUreal.offset + 1)
                : scanError('complex', mustBeUreal.offset, 'expected i');
        }
    };

    // <real R> -> <sign> <ureal R>
    scan['real'] = function(base, text, offset) {

        var sign = scan['sign'](text, offset);

        if (!sign.success)
            return sign;

        var ureal = scan['ureal'](base, text, sign.offset);

        return ureal.success
            ? scanOk('real', sign.value + ureal.value, ureal.offset)
            : ureal;
    };

    /*
     <ureal R> -> <uinteger R>
     | <uinteger R> / <uinteger R>
     | <decimal R>
     */
    scan['ureal'] = function(base, text, offset) {

        // <ureal 10> -> <decimal 10>
        if (base === 10) {
            var maybeDecimal = scan['decimal'](text, offset);
            if (maybeDecimal.success)
                return scanOk('ureal', maybeDecimal.value, maybeDecimal.offset);
        }

        var uint1 = scan['uinteger'](base, text, offset);
        if (!uint1.success)
            return uint1;

        // <ureal R> -> <uinteger R> / <uinteger R>
        if (text.charAt(uint1.offset) === '/') {
            var uint2 = scan['uinteger'](base, text, uint1.offset + 1);
            return uint2.success
                ? scanOk('ureal', uint1.value + '/' + uint2.value, uint2.offset)
                : uint2;
        } else return uint1; // <ureal R> -> <uinteger R>
    };

    /* <decimal 10> -> <uinteger 10> <suffix> (i.e. <digit 10>+ #* <suffix>)
     | . <digit 10>+ #* <suffix>
     | <digit 10>+ . <digit 10>+ #* <suffix>
     | <digit 10>+ #+ . #* <suffix>
     */
    scan['decimal'] = function(text, offset) {

        var leadingDot = text.charAt(offset) === '.';
        var firstDigitBlock = scan['digits'](10, text, leadingDot ? offset + 1 : offset);
        if (!firstDigitBlock.success)
            return firstDigitBlock;
        var afterHashes = consumeWhile(text, firstDigitBlock.offset, function(c) {
            return c === '#';
        });
        var suffix;

        // <decimal 10> -> . <digit 10>+ #* <suffix>
        if (leadingDot) {
            suffix = scan['suffix'](text, afterHashes);
            return suffix.success
                ? scanOk('decimal', '.' + firstDigitBlock.value + suffix.value, suffix.offset)
                : suffix;
        }

        else if (text.charAt(afterHashes) === '.') {
            // the second digit block may be empty, which is the meaning of the last parameter
            var secondDigitBlock = scan['digits'](10, text, afterHashes + 1, true);

            // <decimal 10> -> <digit 10>+ . <digit 10>* #* <suffix>
            if (secondDigitBlock.success) {
                var afterSecondHashes = consumeWhile(text, secondDigitBlock.offset, isHash);
                suffix = scan['suffix'](text, afterSecondHashes);
                return suffix
                    ? scanOk('decimal', firstDigitBlock.value + '.' + secondDigitBlock.value + suffix.value, suffix.offset)
                    : suffix;
            }

            // <decimal 10> -> <digit 10>+ #+ . #* <suffix>
            else if (afterHashes > firstDigitBlock.offset) {
                var afterSecondHashes = consumeWhile(text, afterHashes + 1, isHash);
                suffix = scan['suffix'](text, afterSecondHashes);
                return suffix.success
                    ? scanOk('decimal', firstDigitBlock.value + '.' + suffix.value, suffix.offset)
                    : suffix;
            }

            // Note that the above rule has #+, not #*, so we need at least one.
            else return scanError('decimal', firstDigitBlock.offset, 'expected #');
        }

        // <decimal 10> -> <uinteger 10> <suffix> (i.e. <digit 10>+ #* <suffix>)
        else {
            suffix = scan['suffix'](text, afterHashes);
            return suffix.success
                ? scanOk('decimal', firstDigitBlock.value + suffix.value, suffix.offset)
                : suffix;
        }
    };

    // <uinteger R> -> <digit R>+ #*
    scan['uinteger'] = function(base, text, offset) {

        var digits = scan['digits'](base, text, offset);
        if (!digits.success)
            return digits;

        var finalOffset = consumeWhile(text, digits.offset, function(c) {
            return c === '#';
        });

        return scanOk('uinteger', digits.value, finalOffset);
    };


    // <prefix R> -> <radix R> <exactness> | <exactness> <radix R>
    scan['prefix'] = function(base, text, offset) {
        var radix1 = scan['radix'](base, text, offset);
        if (radix1.success) {
            var exactness2 = scan['exactness'](text, radix1.offset);
            return exactness2.success
                ? scanOk('prefix', radix1.value + exactness2.value, exactness2.offset)
                : exactness2;
        } else {
            var exactness1 = scan['exactness'](text, offset);
            if (!exactness1.success)
                return exactness1;
            var radix2 = scan['radix'](base, text, offset);
            return radix2.success
                ? scanOk('prefix', exactness1.value + radix2.value, radix2.offset)
                : radix2;
        }
    };

    // <suffix> -> <empty> | <exponent marker> <sign> <digit 10>+
    scan['suffix'] = function(text, offset) {

        var marker = scan['exponent-marker'](text, offset);
        if (!marker.success)
            return scanOk('suffix', '', offset);

        var sign = scan['sign'](text, marker.offset);
        if (!sign.success)
            return sign;

        var digits = scan['digits'](10, text, sign.offset);
        if (!digits.success)
            return digits;

        return scanOk('suffix', marker.value + sign.value + digits.value, digits.offset);
    };

    // <exponent-marker> -> e | s | f | d | l
    scan['exponent-marker'] = function(text, offset) {
        var allowedMarkers = 'esfdlESFDL';
        var marker = text.charAt(offset);
        if (marker.length !== 1) {
            return eofError('exponent-marker', offset);
        } else if (allowedMarkers.indexOf(marker) === -1) {
            return scanError('exponent-marker', offset, 'invalid exponent-marker ' + marker);
        } else return scanOk('exponent-marker', marker, offset + 1);
    };

    // <sign> -> <empty> | + | -
    scan['sign'] = function(text, offset) {
        var maybeSign = text.charAt(offset);
        if (maybeSign.length !== 1)
            return eofError('sign', offset);
        else if ('+-'.indexOf(maybeSign) !== -1)
            return scanOk('sign', maybeSign, offset + 1);
        else return scanOk('sign', '', offset);
    };

    // <exactness> -> <empty> | #i | #e
    scan['exactness'] = function(text, offset) {
        var maybeExactness = text.substr(offset, 2);
        if (maybeExactness === '#i'
            || maybeExactness === '#e'
            || maybeExactness === "#I"
            || maybeExactness === "#E")
            return scanOk('exactness', maybeExactness, offset + 2);
        else return scanOk('exactness', '', offset);
    };

    /* <radix 2> -> #b
     <radix 8> -> #o
     <radix 10> -> <empty> | #d
     <radix 16> -> #x
     */
    scan['radix'] = function(base, text, offset) {

        var expected = {2: 'b', 8: 'o', 10: 'd', 16: 'x'};

        var maybeHash = text.charAt(offset);
        if (maybeHash !== '#')
            return base === 10
                ? scanOk('radix', '', offset)
                : scanError('radix', offset, 'expected #');

        var maybeBase = text.charAt(offset + 1);
        if (maybeBase.length !== 1)
            return eofError('radix', offset + 1);
        else if (maybeBase !== expected[base])
            return scanError('radix', offset + 1, 'invalid radix ' + maybeBase);
        else
            return scanOk('radix', base, offset + 2);
    };

    /* <digit 2> -> 0 | 1
     <digit 8> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
     <digit 10> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
     <digit 16> -> 0 | 1 | 2 | 3 |4 | 5| 6 | 7 | 8 | 9 | a | b | c | d | e | f
     */
    scan['digits'] = function(base, text, offset, zeroLengthAllowed) {

        var validDigit = function(c) {
            var acceptable = {2: '01',
                8: '01234567',
                10: '0123456789',
                16: '0123456789abcdefABCDEF'};
            return c.length === 1 && acceptable[base].indexOf(c) !== -1;
        };

        var finalOffset = consumeWhile(text, offset, validDigit);
        var numDigits = finalOffset - offset;

        return (zeroLengthAllowed || numDigits > 0)
            ? scanOk('digits', text.substr(offset, numDigits), finalOffset)
            : scanError('digits', offset, 'expected digits-' + base);
    };

    function scanError(type, offset, msg) {
        return {success: false, tokenType: type, offset: offset, msg: msg || 'parse error'};
    }

    function eofError(type, offset) {
        return scanError(type, offset, 'unexpected EOF while parsing ' + type);
    }

    function scanOk(type, value, nextOffset) {
        return {success: true, tokenType: type, value: value, offset: nextOffset}
    }

    /* 7.1.1: "Tokens which require implicit termination (identifiers, numbers,
     characters, and dot) may be terminated by any <delimiter>, but not
     necessarily by anything else." */
    function requireTokenEnd(type, value, text, nextOffset) {
        /* Note that 'blah'.indexOf('') is 0, so this will work for tokens
         ending at the end of a file. */
        return '()"; \t\n'.indexOf(text.charAt(nextOffset)) === -1
            ? scanError(type, nextOffset, 'expected end of token')
            : scanOk(type, value, nextOffset);
    }

    function consumeWhile(text, offset, predicate) {
        while (predicate(text.charAt(offset)))
            ++offset;
        return offset;
    }

    function consumeIntertokenSpace(text, offset) {
        var cur;
        var inComment = false;
        while (true) {
            if ((cur = text.charAt(offset)).length !== 1) {
                return offset;
            } else if (inComment) {
                ++offset;
                if (cur === '\n')
                    inComment = false;
            } else if (cur === ';') {
                ++offset;
                inComment = true;
            } else if (' \t\n'.indexOf(cur) !== -1) {
                ++offset;
            } else return offset;
        }
    }

    function isHash(c) {
        return c === '#';
    }

    return scan['token'](text, consumeIntertokenSpace(text, offset));

};

scanner.tokenize = function(raw) {
    var tokens = [];
    var offset = 0;
    var token;
    while (offset < raw.length) {
        token = scanner.nextToken(raw, offset);
        if (token.success) {
            tokens.push(token);
            offset = token.offset;
        } else return token;
    }
    return tokens;
};

