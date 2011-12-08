var newStdEnv = (function() {

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
                    || (p instanceof SchemeProcedure); // not builtin
            }
        },

        'pair?': {
            argc: 1,
            proc: function(node) {
                return (node.isList() || node.isImproperList())
                    && !!node.firstChild; // 3.2: (pair? '()) => #f
            }
        },

        /* This is a library procedure in R5RS, which means
            it ought to be written in Scheme. We provide it in JavaScript
            because some of the primitive procedures take list arguments
            (example: apply), so it's nice to define list? at the same time
            so we can do type-checking of the arguments. */
        'list?': {
            argc: 1,
            proc: function(node) {
                return node.isList();
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
            argc: {min: 1},
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
                /* Conversion from the internal first-child/next-sibling
                    representation to the internal car-cdr representation
                    is simple but a bit subtle. If we're at the end of the
                    siblings, we just return that last element as "cdr";
                    otherwise, we package up the remaining elements and return
                    that as "cdr". See also comments to
                    Datum.prototype.siblingsToList. */
                var startOfCdr = p.firstChild.nextSibling;
                if (startOfCdr) {
                    return startOfCdr.nextSibling
                        ? startOfCdr.siblingsToList(p.isImproperList())
                        : startOfCdr;
                } else return newEmptyList();
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
            argc: {min: 2},
            hasSpecialEvalLogic: true,
            proc: function() {

                /* R5RS 6.4: (apply proc arg1 ... args)
                "Proc must be a procedure and args must be a list.
                Calls proc with the elements of the list
                (append (list arg1 ...) args) as the actual arguments.*/

                var mustBeProc = arguments[0];
                if (!(typeof mustBeProc === 'function'
                    || mustBeProc instanceof SchemeProcedure))
                    throw new ArgumentTypeError(mustBeProc, 0, 'apply', 'procedure');

                var curProcCall = arguments[arguments.length-3];
                var procName = curProcCall.firstOperand.payload;
                var continuation = arguments[arguments.length-2];
                var resultStruct = arguments[arguments.length-1];

                var lastRealArgIndex = arguments.length-4;
                var mustBeList = arguments[lastRealArgIndex];
                if (!mustBeList.isList())
                    throw new ArgumentTypeError(mustBeList, lastRealArgIndex, 'apply', 'list');

                // (apply foo '(x y z))
                if (lastRealArgIndex === 1) {
                    var actualProcCall = newProcCall(procName, mustBeList.firstChild, continuation);
                    resultStruct.nextContinuable = actualProcCall;
                }

                // (apply foo a b c '(1 2 3))
                else {
                    for (var i = 1; i < lastRealArgIndex - 1; ++i)
                        arguments[i].nextSibling = arguments[i + 1];
                    arguments[lastRealArgIndex - 1].nextSibling = mustBeList.firstChild;

                    var newArgs = newEmptyList();
                    newArgs.appendChild(arguments[1]);
                    var actualProcCall = newProcCall(procName, newArgs.firstChild, continuation);
                    resultStruct.nextContinuable = actualProcCall;
                }
            }
        },

        // todo bl replace call/cc by full name
        'call/cc': {
            argc: 1,
            argtypes: ['procedure'],
            hasSpecialEvalLogic: true,
            proc: function(procedure, procCall, continuation, resultStruct) {
                /* Semantics of call/cc:

                 (call/cc foo)

                 means create a new procedure call,

                 (foo cc)

                 where cc is the current continuation. Then inside the procedure
                 body, if we see

                 (cc x)

                 (that is, if the trampoline determines that the identifier is
                 bound to a Continuation object), this means bind x to cc's
                 lastResultName and set the next continuable to cc's
                 nextContinuable. */
                var dummyProcCall = newProcCall(procCall.firstOperand, continuation, continuation);
                resultStruct.nextContinuable = dummyProcCall;
                resultStruct.primitiveName = this.operatorName;
            }
        },
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

    function registerBuiltin(name, definition, targetEnv) {

        var argc = definition.argc;
        var argtypes = definition.argtypes;
        var resultType = definition.resultType;
        var proc = definition.proc;

        if (!proc) {
            console.log('warning, builtin ' + name + ' unspecified, skipping');
            return targetEnv;
        }

        if (targetEnv.hasBinding(name))
            console.log('warning, redefining ' + name);

        if (argtypes)
            requirePresenceOf(name, argtypes, targetEnv);

        var binding = function() {

            // Check correct number of arguments
            if (argc) {
                var numArgsFromUser = arguments.length;
                /* If a builtin procedure has special evaluation logic,
                 the trampoline will pass it three additional arguments:
                 the ProcCall, the Continuation, and the TrampolineResultStruct. */
                if (definition.hasSpecialEvalLogic)
                    numArgsFromUser -= 3;

                // If argc is a number, it means exactly that many args are required
                if (typeof argc === 'number' && numArgsFromUser !== argc)
                    throw new IncorrectNumArgs(name, argc, numArgsFromUser);
                else if (argc.min && numArgsFromUser < argc.min)
                    throw new TooFewArgs(name, argc.min, numArgsFromUser);
                else if (argc.max && numArgsFromUser > argc.max)
                    throw new TooManyArgs(name, argc.max, numArgsFromUser);
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
                    var classifier = targetEnv.get(argtypes + '?');
                    for (var i = 0; i < arguments.length; ++i) {
                        /* todo bl this wrapping and unwrapping is getting
                            out of hand. */
                        if (classifier(arguments[i]).unwrap())
                            maybeUnwrappedArgs.push(arguments[i] instanceof Datum ? arguments[i].unwrap() : arguments[i]);
                        else
                            throw new ArgumentTypeError(arguments[i], i, name, argtypes);
                    }
                }

                /* If argtypes is an array like ['string', 'number'], we should
                 go down the arguments array and ensure each argument
                 has its expected type. */
                else if (argtypes instanceof Array) {
                    for (var i = 0; i < arguments.length; ++i) {
                        /* argtypes might be shorter than arguments. In that
                            case we can't typecheck the extra arguments, but
                            we still need to collect them. */
                        if (i < argtypes.length
                            && !targetEnv.get(argtypes[i] + '?')(arguments[i]).unwrap())
                            throw new ArgumentTypeError(arguments[i], i, name, argtypes[i]);
                        maybeUnwrappedArgs.push(arguments[i] instanceof Datum ? arguments[i].unwrap() : arguments[i]);
                    }
                }
            }

            // If no type checking was requested, don't unwrap the args
            else maybeUnwrappedArgs = arguments;

            var returnValue = proc.apply(null, maybeUnwrappedArgs);
            return definition.hasSpecialEvalLogic
                ? null /* A function with special eval logic will set the trampolineResultStruct directly. */
                : maybeWrapResult(returnValue, resultType);
        };
        /* We are setting a boolean flag on a JavaScript function object.
         Not sure this is good style, but it saves us having to wrap
         the function in some other object to signal to the trampoline
         that it has special evaluation logic. */
        if (definition.hasSpecialEvalLogic)
            binding.hasSpecialEvalLogic = true;
        targetEnv.addBinding(name, binding);
    }

    function requirePresenceOf(name, argtypes, targetEnv) {
        if (typeof argtypes === 'string' && !targetEnv.hasBinding(argtypes + '?'))
            throw new InternalInterpreterError('builtin procedure '
                + name
                + ' requires an argument to have type '
                + argtypes
                + ", but the default environment doesn't know about that type yet");
        else if (argtypes instanceof Array) {
            for (var i = 0; i < argtypes.length; ++i)
                if (typeof argtypes[i] === 'string' && !targetEnv.hasBinding(argtypes[i] + '?'))
                    throw new InternalInterpreterError('builtin procedure '
                        + name
                        + ' requires an argument to have type '
                        + argtypes[i]
                        + ", but the default environment doesn't know about that type yet");
        }
    }

    return function() {
        var builtins = new Environment();

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

    };
})();














