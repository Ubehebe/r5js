function SchemePair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
}

function listToArray(list) {
    var ans = [];
    while (list instanceof SchemePair) {
        ans.push(list.car);
        list = list.cdr;
    }
    return ans;
}

function SchemeChar(c) {
    this.c = c;
}

function SchemeString(s) {
    this.s = s;
}

function SchemeProcedure(expr) {
    this.expr = expr;
}

function SchemePort(portno) {
    this.portno = portno;
}

var builtinTypeProcs = {

    // Represent Scheme booleans by JavaScript booleans
    'boolean?': {
        argc: 1,
        proc: function(x) {
            return typeof x === 'boolean';
        }
    },

    // Represent Scheme symbols by JavaScript strings since both are immutable
    'symbol?': {
        argc: 1,
        proc: function(s) {
            return typeof s === 'string';
        }
    },

    /* In JavaScript, characters are strings of length 1, but in Scheme they are
     *  distinct types. */
    'char?': {
        argc: 1,
        proc: function(c) {
            return c instanceof SchemeChar;
        }
    },

    // Represent Scheme vectors by JavaScript arrays
    'vector?': {
        argc: 1,
        proc: function(v) {
            return v instanceof Array;
        }
    },

    // Builtin procedures are just JavaScript procedures.
    'procedure?': {
        argc: 1,
        proc: function(p) {
            return typeof p === 'function' || p instanceof SchemeProcedure;
        }
    },

    'pair?': {
        argc: 1,
        proc: function(p) {
            return p instanceof SchemePair;
        }
    },

    // Represent Scheme numbers by JavaScript numbers
    'number?': {
        argc: 1,
        proc: function(x) {
            return typeof x === 'number';
        }
    },

    /* Scheme strings are mutable, but JavaScript strings are not.
     Therefore we cannot represent Scheme strings directly as JavaScript
     strings; we have to wrap them. */
    'string?': {
        argc: 1,
        proc: function(ss) {
            return ss instanceof SchemeString;
        }
    },

    'port?': {
        argc: 1,
        proc: function(p) {
            return p instanceof SchemePort;
        }
    }
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
    }, // In JavaScript every number is a double.

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
    },

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
    'string->number': {}
};

var builtinPairProcs = {



    'cons': {
        argc: 2,
        proc: function(car, cdr) {
            return new SchemePair(car, cdr);
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

    'symbol->string': {
        argc: 1,
        argtypes: ['symbol'],
        proc: function(sym) {
            return new SchemeString(sym);
        }
    },

    'string->symbol': {
        argc: 1,
        argtypes: ['string'],
        proc: function(ss) {
            return ss.s;
        }
    }
};

var builtinCharProcs = {

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

var builtinStringProcs = {


    'make-string': {
        argc: {min: 1, max: 2},
        argtypes: ['number', 'character'],
        proc: function(n, c) {
            var s = '';
            if (c) {
                for (var i = 0; i < n; ++i)
                    s += c;
            }
            return new SchemeString(s);
        }
    },
    'string-length': {
        argc: 1,
        argtypes: ['string'],
        proc: function(ss) {
            return ss.s.length;
        }
    },
    'string-ref': {
        argc: 2,
        argtypes: ['string', 'number'],
        proc: function(ss, i) {
            return ss.s.charAt(i);
        }
    },
    'string-set!': {
        argc: 3,
        argtypes: ['string', 'number', 'character'],
        proc: function(ss, k, c) {
            var ans = '';
            for (var i = 0; i < ss.s.length; ++i)
                ans += i === k ? c.c : ss.s.charAt(i);
            return new SchemeString(ans);
        }
    }
};

var builtinVectorProcs = {


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
        argtypes: ['vector', 'number'],
        proc: function(v, k, fill) {
            v[k] = fill;
        }
    }
};

var builtinControlProcs = {

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
    'null-environment': {}
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

function registerBuiltin(name, argc, argtypes, proc, targetEnv) {

    if (targetEnv[name])
        console.log('warning, redefining ' + name);

    requirePresenceOf(name, argtypes, targetEnv);

    targetEnv[name] = function() {

        // Check correct number of arguments
        if (argc) {
            if (typeof argc === 'number' && arguments.length !== argc)
                throw new IncorrectNumArgs(name, argc, arguments.length);
            else if (argc.min && arguments.length < argc.min)
                throw new TooFewArgs(name, argc.min, arguments.length);
            else if (argc.max && arguments.length > argc.max)
                throw new TooManyArgs(name, argc.max, arguments.length);
        }

        // Check correct argument types
        if (argtypes) {

            /* If argtypes is something like 'number', that means every argument
             must be a number. */
            if (typeof argtypes === 'string') {
                var classifier = targetEnv[argtypes + '?'];
                for (var i = 0; i < arguments.length; ++i)
                    if (!classifier(arguments[i]))
                        throw new ArgumentTypeError(arguments[i], i, name, argtypes);
            }

            /* If argtypes is something like ['number', 'string'], we should go down
             the arguments array and ensure each argument has its expected type. */
            else if (argtypes instanceof Array) {
                for (var i = 0; i < arguments.length; ++i)
                    if (argtypes[i] && !targetEnv[argtypes[i] + '?'](arguments[i]))
                        throw new ArgumentTypeError(arguments[i], i, name, argtypes[i]);
            }
        }

        // If everything checks out, call the JavaScript builtin
        return proc.apply(null, arguments);
    };
}

function requirePresenceOf(name, argtypes, targetEnv) {
    if (argtypes) {
        if (typeof argtypes === 'string' && !targetEnv[argtypes + '?'])
            throw new InternalInterpreterError('builtin procedure '
                + name
                + ' requires an argument to have type '
                + argtypes
                + ", but the default environment doesn't know about that type yet");
        else if (argtypes instanceof Array) {
            for (var i = 0; i < argtypes.length; ++i)
                if (!targetEnv[argtypes[i] + '?'])
                    throw new InternalInterpreterError('builtin procedure '
                        + name
                        + ' requires an argument to have type '
                        + argtypes[i]
                        + ", but the default environment doesn't know about that type yet");
        }
    }
}

function TooFewArgs(name, minNumArgs, actualNumArgs) {
    this.msg = 'The procedure '
        + name
        + ' has been called with '
        + actualNumArgs
        + ' arguments; it requires at least '
        + minNumArgs
        + 'argument'
        + (minNumArgs === 1 ? '' : 's');
}

function TooManyArgs(name, maxNumArgs, actualNumArgs) {
    this.msg = 'The procedure '
        + name
        + ' has been called with '
        + actualNumArgs
        + ' arguments; it requires at most '
        + maxNumArgs
        + 'argument'
        + (maxNumArgs === 1 ? '' : 's');
}

function IncorrectNumArgs(name, expectedNumArgs, actualNumArgs) {
    this.msg = 'The procedure '
        + name
        + ' has been called with '
        + actualNumArgs
        + ' arguments; it requires exactly '
        + expectedNumArgs
        + 'argument'
        + (expectedNumArgs === 1 ? '' : 's');
}

function InternalInterpreterError(msg) {
    this.msg = msg;
}

function ArgumentTypeError(argument, which, procName, expectedType) {
    this.msg = 'The object '
        + arguments[i].toString()
        + ', passed as argument '
        + which
        + ' to '
        + procName
        + ', is not of the correct type '
        + expectedType;
}











