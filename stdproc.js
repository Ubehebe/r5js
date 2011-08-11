function requireArgTypes(primitiveName, args, type) {

    var ordinalNames = {
        0: 'first',
        1: 'second',
        2: 'third',
        3: 'fourth',
        4: 'fifth',
        5: 'sixth',
        6: 'seventh',
        7: 'eighth',
        8: 'ninth',
        9: 'tenth'
    };

    for (var i = 0; i < args.length; ++i)
        if (!builtins[type + '?'](args[i]))
            throw new SemanticError('The object '
                + args[i]
                + ', passed as the '
                + (ordinalNames[i] || (i + 1) + 'th')
                + ' argument to '
                + primitiveName
                + ', is not the correct type '
                + type);
}

function requireNumArgs(primitiveName, actual, atLeast, isExact) {
    if (actual.length < atLeast) {
        throw new SemanticError('The procedure '
            + primitiveName
            + ' has been called with '
            + actual.length
            + 'arguments; it requires at least '
            + atLeast
            + ' arguments.')
    }

    else if (isExact && actual.length !== atLeast) {
        throw new SemanticError('The procedure '
            + primitiveName
            + ' has been called with '
            + actual.length
            + 'arguments; it requires exactly '
            + atLeast
            + ' arguments.')
    }
}


builtins = {};

function Pair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
}

function listToArray(list) {
    var ans = [];
    while (list instanceof Pair) {
        ans.push(list.car);
        list = list.cdr;
    }
    return ans;
}

function Char(c) {
    this.c = c;
}

function Symbol(s) {
    this.s = s;
}

function Procedure(expr) {
    this.expr = expr;
}

function Port(portno) {
    this.portno = portno;
}

builtins['boolean?'] = function(x) {
    requireNumArgs('boolean?', arguments, 1, true);
    return typeof x === 'boolean';
};
builtins['number?'] = function(x) {
    requireNumArgs('number?', arguments, 1, true);
    return typeof x === 'number';
};

var builtinNumberProcs = {

    'complex?': {
        argc: 1,
        proc: function(x) {
            return false;
        }
    }, // Not required to support this. See 6.2.3.

    'real?': {
        argc: 1,
        proc: function(x) {
            return typeof x === 'number';
        }
    },
    'rational?': {
        argc: 1,
        proc: function(x) {
            return typeof x === 'number';
        }
    },

    'integer?': {
        argc: 1,
        proc: function(x) {
            return typeof x === 'number' && Math.round(x) === x;
        }
    },

    'exact?': {
        argc: 1,
        proc: function(x) {
            return false;
        }
    }, // In JavaScript every number is a float.

    'inexact?': {
        argc: 1,
        proc: function(x) {
            return typeof x === 'number';
        }
    },

    '=': {
        argtypes: 'number', // todo bl support type checking for varargs
        proc: function() {
            for (var i = 0; i < arguments.length - 1; ++i)
                if (arguments[i] !== arguments[i + 1])
                    return false;
            return true;
        }
    },

    '<': {
        argtypes: 'number',
        proc: function() {
            for (var i = 0; i < arguments.length - 1; ++i)
                if (arguments[i] >= arguments[i + 1])
                    return false;
            return true;
        }
    },

    '<=': {
        argtypes: 'number',
        proc: function() {
            for (var i = 0; i < arguments.length - 1; ++i)
                if (arguments[i] > arguments[i + 1])
                    return false;
            return true;
        }
    },

    '>': {
        argtypes: 'number',
        proc: function() {
            for (var i = 0; i < arguments.length - 1; ++i)
                if (arguments[i] <= arguments[i + 1])
                    return false;
            return true;

        }
    },

    '>=': {
        argtypes: 'number',
        proc: function() {
            for (var i = 0; i < arguments.length - 1; ++i)
                if (arguments[i] < arguments[i + 1])
                    return false;
            return true;
        }
    },

    '+': {
        argtypes: 'number',
        proc: function() {
            var sum = 0;
            for (var i = 0; i < arguments.length; ++i)
                sum += arguments[i];
            return sum;
        }
    },

    '*': {
        argtypes: 'number',
        proc: function() {
            var product = 1;
            for (var i = 0; i < arguments.length; ++i)
                product *= arguments[i];
            return product;
        }
    },

    '-': {
        argc: {min: 1}, // todo bl support this
        argtypes: 'number',
        proc: function() {
            // unary
            if (arguments.length === 1)
                return -1 * arguments[0];

            // varargs: (x1 - x2) - x3 etc
            else {
                var ans = arguments[0];
                for (var i = 1; i < arguments.length; ++i)
                    ans -= arguments[i];
                return ans;
            }
        }
    },

    '/': {
        argc: {min: 1},
        argtypes: 'number',
        proc: function() {
            // unary
            if (arguments.length === 1)
                return 1 / arguments[0];

            // varargs: (x1 / x2) / x3 etc
            else {
                var ans = arguments[0];
                for (var i = 1; i < arguments.length; ++i)
                    ans /= arguments[i];
                return ans;
            }
        }
    },

    'remainder': {
        argc: 2,
        argtypes: 'number',
        proc: function(p, q) {
            return p % q;
        }
    }

    // todo bl finish number builtins:
    'quotient': {},
    'modulo': {},
    'numerator': {},
    'denominator': {},
    'floor': {},
    'ceiling': {},
    'truncate': {},
    'round': {},
    'exp': {},
    'log': {},
    'sin': {},
    'cos': {},
    'tan': {},
    'asin': {},
    'acos': {},
    'atan': {},
    'sqrt': {},
    'expt': {},
    'make-rectangular': {},
    'make-polar': {},
    'real-part': {},
    'imag-part': {},
    'magnitude': {},
    'angle': {},
    'exact->inexact': {},
    'inexact->exact': {},
    'number->string': {},
    'string->number': {},


};


var builtinPairProcs = {

    'pair?': {
        argc: 1,
        proc: function(p) {
            return p instanceof Pair;
        }
    },

    'cons': {
        argc: 2,
        proc: function(car, cdr) {
            return new Pair(car, cdr);
        }
    },

    'car': {
        argc: 1,
        argtypes: ['pair'],
        proc: function(p) {
            return p.car;
        }
    },

    'cdr': {
        argc: 1,
        argtypes: ['pair'],
        proc: function(p) {
            return p.cdr;
        }
    },

    'set-car!': {
        argc: 2,
        argtypes: ['pair'],
        proc: function(p, car) {
            p.car = car;
        }
    },

    'set-cdr!': {
        argc: 2,
        argtypes: ['pair'],
        proc: function(p, cdr) {
            p.cdr = cdr;
        }
    }
};

var builtinSymbolProcs = {

    'symbol?': {
        argc: 1,
        proc: function(s) {
            return s instanceof Symbol;
        }
    },

    'symbol->string': {
        argc: 1,
        argtypes: ['symbol'],
        proc: function(sym) {
            return sym.s;
        }
    },

    'string->symbol': {
        argc: 1,
        argtypes: ['string'],
        proc: function(s) {
            return new Symbol(s);
        }
    }
};

var builtinCharProcs = {
    'char?': {
        argc: 1,
        proc: function(c) {
            return c instanceof Char;
        }
    },
    'char=?': {
        argc: 2,
        argtypes: ['character', 'character'],
        proc: function(c1, c2) {
            return c1.c === c2.c;
        }
    },
    'char<?': {
        argc: 2,
        argtypes: ['character', 'character'],
        proc: function(c1, c2) {
            return c1.c < c2.c;
        }
    },
    'char>?': {
        argc: 2,
        argtypes: ['character', 'character'],
        proc: function(c1, c2) {
            return c1.c > c2.c;
        }
    },
    'char<=?': {
        argc: 2,
        argtypes: ['character', 'character'],
        proc: function(c1, c2) {
            return c1.c <= c2.c;
        }
    },
    'char>=?': {
        argc: 2,
        argtypes: ['character', 'character'],
        proc: function(c1, c2) {
            return c1.c >= c2.c;
        }
    },
    'char->integer': {
        argc: 1,
        argtypes: ['character'],
        proc: function(c) {
            return c.charCodeAt(0);
        }
    },
    'integer->char': {
        argc: 1,
        argtypes: ['number'],
        proc: function(i) {
            return String.fromCharCode(i);
        }
    }
};

// todo bl: Scheme strings are mutable, but JavaScript strings aren't.
// Therefore it's a bad idea to represent Scheme strings by JavaScript ones.
// Perhaps use JavaScript strings for Scheme symbols or chars.
var builtinStringProcs = {
    'string?': {
        argc: 1,
        proc: function(s) {
            return typeof s === 'string';
        }
    },
    'make-string': {
        argc: {min: 1, max: 2}, // todo bl implement ranges of arguments
        argtypes: ['number', 'character'],
        proc: function(n, c) {
            var s = '';
            if (c) {
                for (var i = 0; i < n; ++i)
                    s += c;
            }
            return s;
        }
    },
    'string-length': {
        argc: 1,
        argtypes: ['string'],
        proc: function(s) {
            return s.length;
        }
    },
    'string-ref': {
        argc: 2,
        argtypes: ['string', 'number'],
        proc: function(s, i) {
            return s.charAt(i);
        }
    },
    'string-set!': {
        argc: 3,
        argtypes: ['string', 'number', 'character'],
        proc: function(s, k, c) {
            // uh oh!
        }
    }
};

var builtinVectorProcs = {
    'vector?': {
        argc: 1,
        proc: function(v) {
            return v instanceof Array;
        }
    },
    'make-vector': {
        argc: {min: 1, max: 2},
        argtypes: ['number'],
        proc: function(n, fill) {
            var ans = new Array(n);
            if (arguments.length === 2) {
                for (var i = 0; i < n; ++i)
                    ans[i] = fill;
            }
            return ans;
        }
    },
    'vector-length': {
        argc: 1,
        argtypes: ['vector'],
        proc: function(v) {
            return v.length;
        }
    },
    'vector-ref': {
        argc: 2,
        argtypes: ['vector', 'number'],
        proc: function(v, k) {
            return v[k];
        }
    },
    'vector-set!': {
        argc: 3,
        argypes: ['vector', 'number'],
        proc: function(v, k, fill) {
            v[k] = fill;
        }
    }
};

var builtinControlProcs = {
    'procedure?': {
        argc: 1,
        proc: function(p) {
            return typeof p === 'function' || p instanceof Procedure;
        }
    },
    'apply': {
        argc: 2,
        argtypes: ['procedure', 'list'], // todo bl list isn't a primitive type
        proc: function(p, args) {
            return typeof p === 'function'
                ? p.apply(null, listToArray(args))
                : false; // todo bl not yet implemented!
        }
    },

    // todo bl:
    'call-with-current-continuation': {},
    'values': {},
    'call-with-values': {},
    'dynamic-wind': {}
};

// todo bl
builtinEvalProcs = {
    'eval': {},
    'scheme-report-environment': {},
    'null-environment': {},
};

// todo bl
var builtinIOProcs = {
    'call-with-input-file': {},
    'call-with-output-file': {},
    'input-port?': {},
    'output-port?': {},
    'current-input-port': {},
    'current-output-port': {},
    'with-input-from-file': {},
    'with-output-to-file': {},
    'open-input-file': {},
    'open-output-file': {},
    'close-input-port': {},
    'close-output-port': {},
    'read-char': {},
    'peek-char': {},
    'eof-object?': {},
    'char-ready?': {},
    'write-char': {}
};









