var builtins = (function() {

    var builtinTypeProcs = {

        'boolean?': {
            argc: 1,
            proc: function(node) {
                return node.isBoolean();
            }
        },

        'symbol?': {
            argc: 1,
            proc: function(node) {
                return node.isIdentifier();
            }
        },

        'char?': {
            argc: 1,
            proc: function(node) {
                return node.isCharacter();
            }
        },

        /* 6.3.6: "Like list constants, vector constants must be quoted."
            (Neither lists nor vectors appear anywhere in the non-datum grammar).
            Thus (vector? '#()) => #t, but (vector? #()) is a parse error. Nevertheless,
            both PLT and MIT Scheme have it evaluate to #t. In those implementations,
            it seems vectors (but not lists?) are self-evaluating. */
        'vector?': {
            argc: 1,
            proc: function(node) {
                return node.isVector();
            }
        },

        'procedure?': {
            argc: 1,
            proc: function(p) {
                return typeof p === 'function' // builtin
                    || (p instanceof Datum && p.isProcedure()); // not builtin
            }
        },

        'pair?': {
            argc: 1,
            proc: function(node) {
                return (node.isList() || node.isImproperList())
                    && !!node.firstChild; // 3.2: (pair? '()) => #f
            }
        },

        'null?': {
            argc: 1,
            proc: function(node) {
                return node.isEmptyList();
            }
        },

        'number?': {
            argc: 1,
            proc: function(node) {
                return node.isNumber();
            }
        },

        'string?': {
            argc: 1,
            proc: function(node) {
                return node.isString();
            }
        },

        // todo bl have no idea...
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
            proc: function(node) {
                return node.isNumber();
            }
        },

        'rational?': {
            argc: 1,
            proc: function(node) {
                return node.isNumber();
            }
        },

        'integer?': {
            argc: 1,
            proc: function(node) {
                return node.isNumber() && Math.round(node.payload) === node.payload;
            }
        },

        'exact?': {
            argc: 1,
            argtypes: 'number',
            proc: function(x) {
                return false;
            }
        }, // In JavaScript every number is a double.

        'inexact?': {
            argc: 1,
            argtypes: 'number',
            proc: function(x) {
                return true;
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
                // todo bl this is really expensive! can we cut down on the copying?
                var realCar = car.stripParent().clone();
                var realCdr = cdr.stripParent().clone();
                // Since cdr already has a "head of list" node, reuse that. Convoluted eh?
                if (realCdr.isList() || realCdr.isImproperList()) {
                    realCdr.prependChild(realCar);
                    return realCdr;
                } else {
                    var ans = new Datum();
                    ans.type = '.(';
                    ans.appendChild(realCar);
                    ans.appendChild(realCdr);
                    // todo bl hmm the parent field isn't getting set...is that ok?
                    return ans;
                }
            }
        },

        'car': {
            argc: 1,
            argtypes: ['pair'],
            proc: function(p) {
                return p.firstChild;
            }
        },

        'cdr': {
            argc: 1,
            argtypes: ['pair'],
            proc: function(p) {
                var startOfCdr = p.firstChild.nextSibling;
                return startOfCdr
                    ? startOfCdr.siblingsToList(p.isImproperList())
                    : newEmptyList();
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
            argtypes: ['char', 'char'],
            proc: function(c1, c2) {
                return c1.c === c2.c;
            }
        },
        'char<?': {
            argc: 2,
            argtypes: ['char', 'char'],
            proc: function(c1, c2) {
                return c1.c < c2.c;
            }
        },
        'char>?': {
            argc: 2,
            argtypes: ['char', 'char'],
            proc: function(c1, c2) {
                return c1.c > c2.c;
            }
        },
        'char<=?': {
            argc: 2,
            argtypes: ['char', 'char'],
            proc: function(c1, c2) {
                return c1.c <= c2.c;
            }
        },
        'char>=?': {
            argc: 2,
            argtypes: ['char', 'char'],
            proc: function(c1, c2) {
                return c1.c >= c2.c;
            }
        },
        'char->integer': {
            argc: 1,
            argtypes: ['char'],
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
            argtypes: ['number', 'char'],
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
            argtypes: ['string', 'number', 'char'],
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
            argtypes: ['procedure', 'list'] // todo bl list isn't a primitive type
            /*proc: function(p, args) {
             return typeof p === 'function'
             ? p.apply(null, listToArray(args))
             : false; // todo bl not yet implemented!
             }*/
        },

        // todo bl:
        'call-with-current-continuation': {},
        'values': {},
        'call-with-values': {},
        'dynamic-wind': {}
    };

// todo bl
    var builtinEvalProcs = {
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

    function registerBuiltin(name, value, targetEnv) {

        var argc = value.argc;
        var argtypes = value.argtypes;
        var resultType = value.resultType;
        var proc = value.proc;

        if (!proc) {
            console.log('warning, builtin ' + name + ' unspecified, skipping');
            return targetEnv;
        }

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

            var maybeUnwrappedArgs;

            /* If type checking was requested, do the type checking and
                also unwrap the arguments (so that a datum representing 1
                gets unwrapped to a JavaScript number 1). This makes sense
                because if you're writing a procedure that doesn't do any type
                checking, you should be prepared to handle arbitrary objects
                (i.e. Datum objects) in the procedure itself. */
            if (argtypes) {
                maybeUnwrappedArgs = [];

                /* If argtypes is something like 'number', that means every argument
                 must be a number. */
                if (typeof argtypes === 'string') {
                    var classifier = targetEnv[argtypes + '?'];
                    for (var i = 0; i < arguments.length; ++i) {
                        if (classifier(arguments[i]))
                            maybeUnwrappedArgs.push(arguments[i].unwrap());
                        else
                            throw new ArgumentTypeError(arguments[i], i, name, argtypes);
                    }
                }

                /* If argtypes is something like ['number', 'string'], we should go down
                 the arguments array and ensure each argument has its expected type. */
                else if (argtypes instanceof Array) {
                    for (var i = 0; i < arguments.length; ++i) {
                        if (argtypes[i] && !targetEnv[argtypes[i] + '?'](arguments[i]))
                            throw new ArgumentTypeError(arguments[i], i, name, argtypes[i]);
                        else
                            maybeUnwrappedArgs.push(arguments[i].unwrap());
                    }
                }
            }

            // If no type checking was requested, don't unwrap the args
            else maybeUnwrappedArgs = arguments;

            return maybeWrapResult(proc.apply(null, maybeUnwrappedArgs), resultType);
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


    var builtins = {};

    [builtinTypeProcs,
        builtinCharProcs,
        builtinControlProcs,
        builtinEvalProcs,
        builtinIOProcs,
        builtinNumberProcs,
        builtinPairProcs,
        builtinStringProcs,
        builtinSymbolProcs,
        builtinVectorProcs].forEach(function(procs) {
        for (var name in procs)
            registerBuiltin(name, procs[name], builtins);
    });

    return builtins;
})();














