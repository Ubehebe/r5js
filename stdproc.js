var R5JS_builtins = {};

/* The names of the different categories of builtins are just for
 readability; they all get loaded into the same namespace. */

R5JS_builtins['equiv'] = {

    'eqv?': {
        argc: 2,
        proc: function(p, q) {
            /* This implementation closely follows the description of eqv?
             in R5RS 6.1, which explicitly leaves some comparisons undefined. */

            /* Since several of the below tests include pointer comparisons,
             we need to be sure we're working with the "master" versions of the
             datums and not defensive clones. */
            p = p.getCloneSource();
            q = q.getCloneSource();

            if (p.sameTypeAs(q)) {

                if (p.isBoolean())
                    return p.payload === q.payload;
                else if (p.isIdentifier())
                    return p.payload === q.payload;
                else if (p.isNumber())
                    return p.payload === q.payload; // todo bl numerical precision...
                else if (p.isCharacter())
                    return p.payload === q.payload;
                else if (p.isList())
                    return p === q || p.isEmptyList() && q.isEmptyList();
                else if (p.isImproperList())
                    return p === q;
                else if (p.isVector()) {
                    return (p.isArrayBacked() && q.isArrayBacked())
                        ? p.payload === q.payload
                        : p === q;
                }
                else if (p.isString())
                    return p === q;
                else if (p.isProcedure())
                    return p.payload === q.payload;
                else if (p.isQuasiquote()) {
                    /* todo bl: not sure this is the right thing to do.
                     We can't just "unescape" the quasiquotations. Example:

                     (equal? '(a `(b ,(+ 1 2))) '(a `(b ,(+ 1 2))))

                     This will eventually call

                     (eqv? `(b ,(+ 1 2)) `(b ,(+ 1 2)))

                     From this procedure call, it looks as if we should unescape
                     the quasiquotes, but that's incorrect; we've lost
                     the surrounding quotation level.

                     It may be possible to figure out what to do based on
                     the qqLevels, but it's been a while since I've looked at
                     that subsystem. */
                    return p.isEqual(q);
                }
                else return false;

            } else return false;
        }
    },
    /* From the description of eq? at R5RS 6.1, it looks like it is
     permissible for it to have exactly the same semantics as eqv?. */
    'eq?':{
        argc:2,
        proc: function(p, q) {
            // todo bl is there a faster and cleaner way to alias this?
            return R5JS_builtins['equiv']['eqv?'].proc(p, q);
        }
    }
};

R5JS_builtins['type'] = {

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
            /* R5RS 6.4: "The procedure call-with-current-continuation
             packages up the current continuation as an "escape procedure"
             and passes it as an argument to proc." Thus a Continuation
             must count as a procedure. */
            return (p instanceof Datum && p.isProcedure())
                || p instanceof Continuation;
        }
    },

    'pair?': {
        argc: 1,
        proc: function(node) {
            return (node.isList()
                || node.isImproperList()
                || node.isQuote())
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

R5JS_builtins['number'] = {

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
        argtypes: 'number',
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
            if (q === 0)
                throw new PrimitiveProcedureError('remainder: undefined for 0');
            // The JavaScript % semantics are precisely the Scheme remainder semantics.
            else return p % q;
        }
    },

    'quotient': {
        argc: 2,
        argtypes: 'number',
        proc: function(p, q) {
            if (q === 0)
                throw new PrimitiveProcedureError('quotient: undefined for 0');
            else {
                /* In Scheme, quotient rounds towards zero, which is unfortunately
                not what JavaScript's Math.round() does. */
                var unrounded = p / q;
                return unrounded > 0
                    ? Math.floor(unrounded)
                    : Math.ceil(unrounded);
            }
        }
    },
    'modulo': {
        argc: 2,
        argtypes: 'number',
        proc: function(p, q) {
            if (q === 0)
                throw new PrimitiveProcedureError('modulo: undefined for 0');
            else {
                var remainder = p % q;
                var sign = p*q;
                var ans = remainder;
                // Both positive or both negative: remainder and modulo are the same
                if (sign > 0)
                    return remainder;

                /* If p is positive and q is negative,
                remainder will be positive and modulo will be negative */
                else if (p > 0) {
                    while (ans > 0)
                        ans += q;
                    return ans;
                }

                /* If p is negative and q is positive,
                remainder will be negative and modulo will be positive */
                else {
                    while (ans < 0) {
                        ans += q;
                    }
                    return ans;
                }
            }
        }
    },
    'numerator': {
        // todo bl
    },
    'denominator': {
        // todo bl
    },
    'floor': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            return Math.floor(x);
        }
    },
    'ceiling': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            return Math.ceil(x);
        }
    },
    'truncate': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            /* R5RS 6.2.5: "Truncate returns the integer closest to x
            whose absolute value is not larger than the absolute value of x." */
            return x > 0 ? Math.floor(x) : Math.ceil(x);
        }
    },
    'round': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            /* R5RS 6.2.5: "Round returns the closest integer to x,
             rounding to even when x is halfway between two integers." */
            var down = Math.floor(x);
            var downDiff = Math.abs(x - down);
            var up = Math.ceil(x);
            var upDiff = Math.abs(up - x);

            if (upDiff < downDiff)
                return up;
            else if (downDiff < upDiff)
                return down;
            else return up % 2 ? down : up;
        }
    },
    'exp': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.exp(z); }
    },
    'log': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.log(z); }
    },
    'sin': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.sin(z); }
    },
    'cos': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.cos(z); }
    },
    'tan': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.tan(z); }
    },
    'asin': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.asin(z); }
    },
    'acos': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) { return Math.acos(z); }
    },
    'atan': {
        argtypes: 'number',
        proc: function() {
            /* Oddly, R5RS overloads atan for both one and two arguments,
             rather than having a separate atan2. */
            switch (arguments.length) {
                case 1:
                    return Math.atan(arguments[0]);
                case 2:
                    return Math.atan2(arguments[0], arguments[1]);
                default:
                    throw new TooManyArgs('atan', 2, arguments.length);
            }
        }
    },
    'sqrt': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            return Math.sqrt(x);
        }
    },
    'expt': {
        argc: 2,
        argtypes: 'number',
        proc: function(z1, z2) {
            return Math.pow(z1, z2);
        }
    },
    'make-rectangular': {
        argc: 2,
        argtypes: 'number',
        proc: function(r, theta) {
            throw new UnimplementedOptionError('make-rectangular');
        }
    },
    'make-polar': {
        argc: 2,
        argtypes: 'number',
        proc: function(x, y) {
            throw new UnimplementedOptionError('make-polar');
        }
    },
    'real-part': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new UnimplementedOptionError('real-part');
        }
    },
    'imag-part': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new UnimplementedOptionError('imag-part');
        }
    },
    'magnitude': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new UnimplementedOptionError('magnitude');
        }
    },
    'angle': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new UnimplementedOptionError('angle');
        }
    },
    'exact->inexact': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            return x; // In JavaScript every number is inexact
        }
    },
    'inexact->exact': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            return x;
        }
    },
    'number->string': {
        argc: 1,
        argtypes: 'number',
        proc: function(x) {
            return newIdOrLiteral(x + '', 'string');
        }
    },
    'string->number': {
        argc: 1,
        argtypes: 'string',
        proc: function(s) {
            return parseFloat(s);
        }
    }
};

R5JS_builtins['pair'] = {

    'cons': {
        argc: 2,
        proc: function(car, cdr) {
            // todo bl this is really expensive! can we cut down on the copying?
            var realCar = car.stripParent().clone(true);
            var realCdr = cdr.stripParent().clone(true);
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
             siblings, we either return that last element as "cdr"
             (if we're in at the end of an improper list) or wrap it
             in a list (if we're at the end of a proper list).
             If we're not at the end of the siblings, we package up the
             remaining elements and return that as "cdr". See also
             comments to Datum.prototype.siblingsToList. */
            var startOfCdr = p.firstChild.nextSibling;
            if (startOfCdr) {
                return (startOfCdr.nextSibling || p.isList())
                    ? startOfCdr.siblingsToList(p.isImproperList())
                    : startOfCdr;
            } else return newEmptyList();
        }
    },

    'set-car!': {
        argc: 2,
        proc: function setCar(p, car) {
            if (p.isList() || p.isImproperList()) {
                car.nextSibling = p.firstChild.nextSibling;
                p.firstChild = car;

                /* If this was a derivative copy, mutate the source as well.
                 (Although this is written recursively, there should be at most one
                 recursion. See Datum.prototype.getCloneSource.) */
                return p.src
                    ? setCar(p.src, car.clone(true))
                    : null; // unspecified return value
            } else throw new ArgumentTypeError(p, 0, 'set-car!', 'pair');
        }
    },

    'set-cdr!': {
        argc: 2,
        proc: function setCdr(p, cdr) {
            if (p.isList() || p.isImproperList()) {

                if (cdr.isList()) {
                    p.firstChild.nextSibling = cdr.firstChild;
                    p.type = '(';
                } else {
                    p.firstChild.nextSibling = cdr;
                    p.type = '.(';
                }

                /* If this was a derivative copy, mutate the source as well.
                 (Although this is written recursively, there should be at most one
                 recursion. See Datum.prototype.getCloneSource.) */
                return p.src
                    ? setCdr(p.src, cdr)
                    : null; // unspecified return value
            } else throw new ArgumentTypeError(p, 0, 'set-cdr!', 'pair');
        }
    }
};

R5JS_builtins['symbol'] = {

    'symbol->string': {
        argc: 1,
        argtypes: ['symbol'],
        proc: function(sym) {
            return newIdOrLiteral(sym, 'string');
        }
    },

    'string->symbol': {
        argc: 1,
        argtypes: ['string'],
        proc: function(s) {
            return newIdOrLiteral(s, 'identifier');
        }
    }
};

R5JS_builtins['char'] = {

    'char=?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(c1, c2) {
            return c1 === c2;
        }
    },
    'char<?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(c1, c2) {
            return c1 < c2;
        }
    },
    'char>?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(c1, c2) {
            return c1 > c2;
        }
    },
    'char<=?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(c1, c2) {
            return c1 <= c2;
        }
    },
    'char>=?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(c1, c2) {
            return c1 >= c2;
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
            return newIdOrLiteral(String.fromCharCode(i), 'character');
        }
    },
    'char-upcase': {
        argc: 1,
        argtypes: ['char'],
        proc: function(c) {
            return newIdOrLiteral(c.toUpperCase(), 'character');
        }
    },
    'char-downcase': {
        argc: 1,
        argtypes: ['char'],
        proc: function(c) {
            return newIdOrLiteral(c.toLowerCase(), 'character');
        }
    }
};

R5JS_builtins['string'] = {

    'make-string': {
        argc: {min: 1, max: 2},
        argtypes: ['number', 'char'],
        proc: function(n, c) {
            /* R5RS 6.3.5: "If char is given, then all elements of the
             string are initialized to char, otherwise the contents
             of the string are unspecified." */
            c = c || ' ';
            var s = '';
            for (var i = 0; i < n; ++i)
                s += c;
            return newIdOrLiteral(s, 'string');
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
            return newIdOrLiteral(s.charAt(i), 'character');
        }
    },
    'string-set!': {
        argc: 3,
        proc: function stringSet(str, k, c) {

            if (!str.isString())
                throw new ArgumentTypeError(str, 0, 'string-set!', 'string');
            if (!k.isNumber())
                throw new ArgumentTypeError(k, 1, 'string-set!', 'number');
            if (!c.isCharacter())
                throw new ArgumentTypeError(c, 2, 'string-set!', 'character');

            var s = str.payload;

            str.payload = s.substr(0, k.payload)
                + c.payload
                + s.substr(k.payload + 1);

            /* If this was a derivative copy, mutate the source as well.
             (Although this is written recursively, there should be at most one
             recursion. See Datum.prototype.getCloneSource.) */
            return str.src
                ? stringSet(str.src, k, c)
                : null; // unspecified return value
        }
    }
};

R5JS_builtins['vector'] = {

    /* todo bl: the current vector implementation uses Datums, in other
     words, linked lists! Replace this by something that's actually
     random access. */

    'make-vector': {
        argc: {min: 1, max: 2},
        proc: function(n, fill) {
            /* We want n to be a number (= unwrapped) and fill to be a
             Datum (= wrapped). Unfortunately, if we specify
             argtypes: ['number'] in order to get automatic type checking
             on the first argument, then all the arguments will be
             unwrapped; and if we omit argtypes, then none of the
             arguments will be unwrapped. So we manually unwrap the
             first argument and do the type checking ourselves.
             C'est la vie. */
            n = n.unwrap();
            if (typeof n !== 'number')
                throw new ArgumentTypeError(n, 0, 'make-vector', 'number');
            /* R5RS 6.3.6: "If a second argument is given, then each
             element is initialized to fill. Otherwise the initial
             contents of each element is unspecified."

             False seems like a good default. */
            fill = fill || newIdOrLiteral(false, 'boolean');
            var buf = [];
            for (var i = 0; i < n; ++i)
                buf.push(fill.clone());
            return newVectorDatum(buf);
        }
    },
    'vector-length': {
        argc: 1,
        argtypes: ['vector'],
        proc:function (v) {
            return v.isArrayBacked()
                ? v.payload.length
                : v.convertVectorToArrayBacked().payload.length;

        }
    },
    'vector-ref': {
        argc: 2,
        argtypes: ['vector', 'number'],
        proc: function(v, k) {
            return v.isArrayBacked()
                ? v.payload[k]
                : v.convertVectorToArrayBacked().payload[k];
        }
    },
    'vector-set!': {
        argc:3,
        proc:function (v, k, fill) {
            v = v.unwrap();
            k = k.unwrap();

            if (!v.isVector())
                throw new ArgumentTypeError(v, 0, 'vector-set!', 'vector');
            else if (typeof k !== 'number')
                throw new ArgumentTypeError(k, 1, 'vector-set!', 'number');

            if (v.isArrayBacked())
                v.payload[k] = fill;
            else
                v.convertVectorToArrayBacked().payload[k] = fill;

            return null;
        }
    }
};

R5JS_builtins['control'] = {

    'apply': {
        argc: {min: 2},
        hasSpecialEvalLogic: true,
        proc: function() {

            /* R5RS 6.4: (apply proc arg1 ... args)
             "Proc must be a procedure and args must be a list.
             Calls proc with the elements of the list
             (append (list arg1 ...) args) as the actual arguments.*/

            var mustBeProc = arguments[0];
            if (!mustBeProc.isProcedure())
                throw new ArgumentTypeError(mustBeProc, 0, 'apply', 'procedure');

            var curProcCall = arguments[arguments.length - 3];
            /* todo bl: very little idea what's going on here, but we seem to
             use both sources of procName. */
            var procName = newIdOrLiteral(curProcCall.firstOperand.payload || mustBeProc.name);
            var continuation = arguments[arguments.length - 2];
            var resultStruct = arguments[arguments.length - 1];

            var lastRealArgIndex = arguments.length - 4;
            var mustBeList = arguments[lastRealArgIndex];
            if (!mustBeList.isList())
                throw new ArgumentTypeError(mustBeList, lastRealArgIndex, 'apply', 'list');

            // (apply foo '(x y z))
            if (lastRealArgIndex === 1) {
                var newArgs = new SiblingBuffer();
                // todo bl document why we are quoting the arguments
                for (var arg = mustBeList.firstChild; arg; arg = arg.nextSibling)
                    newArgs.appendSibling(arg.quote());
                var actualProcCall = newProcCall(procName, newArgs.toSiblings(), continuation);
                actualProcCall.setStartingEnv(curProcCall.env);
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

    'call-with-current-continuation': {
        argc: 1,
        argtypes: ['procedure'],
        hasSpecialEvalLogic: true,
        proc: function(procedure, procCall, continuation, resultStruct) {
            /* Semantics of call/cc:

             (call-with-current-continuation foo)

             means create a new procedure call,

             (foo cc)

             where cc is the current continuation. Then inside the procedure
             body, if we see

             (cc x)

             (that is, if the trampoline determines that the identifier is
             bound to a Continuation object), this means bind x to cc's
             lastResultName and set the next continuable to cc's
             nextContinuable. */
            if (resultStruct.beforeThunk) {
                /* If this continuation is inside a call to dynamic-wind but
                 escapes and then is later re-called, we have to remember
                 to execute the associated before and after thunks. */
                continuation.installBeforeThunk(resultStruct.beforeThunk);
                resultStruct.beforeThunk = null;
            }
            var dummyProcCall = newProcCall(procCall.firstOperand, continuation, continuation);
            dummyProcCall.setStartingEnv(procCall.env);
            resultStruct.nextContinuable = dummyProcCall;
        }
    },
    'values': {
        argc: {min: 1},
        hasSpecialEvalLogic: true,
        proc: function() {
            // Varargs procedures that also have special eval logic are a pain.
            var resultStruct = arguments[arguments.length - 1];
            var continuation = arguments[arguments.length - 2];
            var procCall = arguments[arguments.length - 3];
            var numUserArgs = arguments.length - 3;

            /* If there's just one user-supplied argument, that works fine
             with the existing machinery. Example:

             (values 1 [_0 ...])

             should just bind 1 to _0 and continue. */
            if (numUserArgs === 1)
                procCall.env.addBinding(continuation.lastResultName, arguments[0]);

            /* If there's more than one argument, we bind the whole array
             to the continuation's lastResultName. This means later, when
             we're evaluating the arguments to a procedure call, we have
             to remember that a single name like _0 could specify a whole
             list of arguments. */
            else {

                var userArgs = [];

                for (var i = 0; i < numUserArgs; ++i)
                    userArgs.push(arguments[i]);

                procCall.env.addBinding(continuation.lastResultName, userArgs);
            }
            if (continuation.nextContinuable)
                continuation.nextContinuable.setStartingEnv(procCall.env);
            resultStruct.nextContinuable = continuation.nextContinuable;
        }
    }   ,
    'call-with-values': {
        argc: 2,
        argtypes: ['procedure', 'procedure'],
        hasSpecialEvalLogic: true,
        proc: function(producer, consumer, procCall, continuation, resultStruct) {
            /* R5RS 6.4: (call-with-values producer consumer)
             "Calls its producer argument with no values and a continuation
             that, when passed some values, calls the consumer procedure
             with those values as arguments. The continuation for the call
             to consumer is the continuation of the call to
             call-with-values." */

            var valuesName = newCpsName();
            var producerContinuation = new Continuation(valuesName);
            var producerCall = newProcCall(
                procCall.firstOperand,
                null, // no arguments
                producerContinuation);
            producerCall.setStartingEnv(procCall.env);
            var consumerCall = newProcCall(
                procCall.firstOperand.nextSibling,
                newIdOrLiteral(valuesName),
                continuation);
            consumerCall.setStartingEnv(procCall.env);
            producerContinuation.nextContinuable = consumerCall;
            resultStruct.nextContinuable = producerCall;
        }
    },
    'dynamic-wind': {
        argc: 3,
        argtypes: ['procedure', 'procedure', 'procedure'],
        hasSpecialEvalLogic: true,
        proc: function(before, thunk, after, procCall, continuation, resultStruct) {

            /* Semantics of dynamic-wind (as I understand it):
             (dynamic-wind foo bar baz) means execute bar with the
             following modifications:

             - Whenever I'm about to go into bar, do foo first
             - Whenever I'm about to go out of bar, do baz first

             In simple cases, this is the same as (begin foo bar baz)
             (except that the return value is that of bar, not baz).
             The situation is complicated by continuations captured inside
             a call/cc and later reentered; these must trigger the before
             and after thunks. For example:

             (define cont #f)
             (define (foo) (display 'foo))
             (define (bar) (display 'bar))
             (dynamic-wind
             foo
             (lambda ()
             (call-with-current-continuation
             (lambda (c)
             (set! cont c))))
             bar)
             (cont 42)

             This will print "foo", "bar", "foo", "bar", and return
             an unspecified value (because there's nothing in the body
             of the lambda after the call/cc, for the call/cc to deliver the
             42 to). */

            var before = newCpsName();

            // None of the three thunks have any arguments.

            // todo bl use a ContinuableBuffer for efficiency

            var procCallBefore = newProcCall(
                procCall.firstOperand,
                null, // no arguments
                new Continuation(before));


            var procCallAfter = newProcCall(
                procCall.firstOperand.nextSibling.nextSibling,
                null, // no arguments
                new Continuation(newCpsName())
            );

            var result = newCpsName();
            procCallAfter.appendContinuable(newIdShim(newIdOrLiteral(result), newCpsName()));
            procCallAfter.getLastContinuable().continuation = continuation;

            var procCallThunk = newProcCall(
                procCall.firstOperand.nextSibling,
                null, // no arguments
                new Continuation(result)
            );

            procCallThunk.appendContinuable(procCallAfter);
            procCallBefore.appendContinuable(procCallThunk);

            resultStruct.nextContinuable = procCallBefore;
            /* We use the TrampolineResultStruct to store the thunk.
             This should be okay because dynamic-wind is the only one
             who writes to it, and call/cc is the only one who reads it.

             todo bl document why we cannot reuse procCallBefore. */
            resultStruct.beforeThunk = newProcCall(
                procCall.firstOperand,
                null,
                new Continuation(before));
        }
    }
};

R5JS_builtins['eval'] = {
    'eval': {
        argc: 2,
        proc: function(expr, envSpec) {
            if (!(expr instanceof Datum))
                throw new ArgumentTypeError(expr, 0, 'eval', 'datum');
            else if (!(envSpec instanceof Datum) || !envSpec.isEnvironmentSpecifier())
                throw new ArgumentTypeError(envSpec, 1, 'eval', 'environment-specifier');
            else {
                // don't accidentally evaluate the next expr!
                expr.nextSibling = null;
                return R5JS.evalDatum(expr, envSpec.payload);
            }
        }
    },
    /* This is not part of any Scheme standard, but it should be useful to
     test Scheme expressions that should not evaluate. */
    'eval-or-else': {
        argc: 3,
        proc: function(expr, envSpec, orElse) {
            try {
                if (!(expr instanceof Datum))
                    throw new ArgumentTypeError(expr, 0, 'eval', 'datum');
                else if (!(envSpec instanceof Datum) || !envSpec.isEnvironmentSpecifier())
                    throw new ArgumentTypeError(envSpec, 1, 'eval', 'environment-specifier');
                else return R5JS.evalDatum(expr, envSpec.payload);
            } catch (e) {
                return orElse;
            }
        }
    },
    'scheme-report-environment': {
        argc: 1,
        argtypes: ['number'],
        proc: function(num) {
            if (num === 5)
                return newEnvironmentSpecifier(R5JS_R5RSEnv);
            else throw new InternalInterpreterError(
                'unsupported scheme report environment ' + num);
        }
    },
    'null-environment': {
        argc: 1,
        argtypes: ['number'],
        proc: function(num) {
            if (num === 5)
                return newEnvironmentSpecifier(R5JS_nullEnv);
            else throw new InternalInterpreterError(
                'unsupported null environment ' + num);
        }
    }
};

// todo bl
R5JS_builtins['io'] = {
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
    'write-char': {
    },
    'display': {
        /* According to R5RS 6.6.3, display is supposed to be a library
            procedure. Since the only non-library output routine is write-char,
            display would presumably have to be written in terms of write-char.
            That's not too efficient, so decided to write it in JavaScript.

            todo bl: all the I/O routines in Scheme take an optional second
            parameter to specify the port, but this doesn't make a lot of sense
            on a web page. Make a decision about whether to support these. */
        argc: 1,
        proc: function(x) {
            /* Don't show quotes when displaying strings, even though they
             are part of the external representation. */
            if (x instanceof Datum
                && (x.isString() || x.isCharacter()))
                console.log(x.payload);
            else if (x && x.toString)
                console.log(x.toString());
            else
                console.log(x);

            // The return value is unspecified.
            return null;
        }
    }
};

function registerBuiltin(name, definition, targetEnv) {

    var argc = definition.argc;
    var argtypes = definition.argtypes;
    var resultType = definition.resultType;
    var proc = definition.proc;

    if (!proc) {
        //console.log('warning, builtin ' + name + ' unspecified, skipping');
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
                var classifier = targetEnv.getProcedure(argtypes + '?');
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
                        && !targetEnv.getProcedure(argtypes[i] + '?')(arguments[i]).unwrap())
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