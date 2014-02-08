/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.PrimitiveProcedures');


goog.require('r5js.ArgumentTypeError');
goog.require('r5js.CdrHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.ImmutableError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.NodeBackedPort');
goog.require('r5js.OutputMode');
goog.require('r5js.ParseError');
goog.require('r5js.PrimitiveProcedureError');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooFewArgs');
goog.require('r5js.TooManyArgs');
goog.require('r5js.UnimplementedOptionError');
goog.require('r5js.data');
goog.require('r5js.globals');
goog.require('r5js.procs');
goog.require('r5js.trampoline');
goog.require('r5js.util.Logger');


/**
 * The names of the different categories of builtins are just
 * for readability; they all get loaded into the same namespace.
 */
r5js.builtins = {};



r5js.builtins['equiv'] = {

    'eqv?': {
        argc: 2,
        proc: function(p, q) {
            /* This implementation closely follows the description of eqv?
             in R5RS 6.1, which explicitly leaves some comparisons undefined. */

            if (p.sameTypeAs(q)) {

                if (p.isBoolean())
                    return p.payload === q.payload;
                else if (p.isIdentifier())
                    return p.payload === q.payload;
                else if (p.isNumber())
                    return p.payload === q.payload; // todo bl numerical precision...
                else if (p.isCharacter())
                    return p.payload === q.payload;
                else if (p.isList()) {
                    var ans;
                    if (p === q || p.isEmptyList() && q.isEmptyList())
                        ans = true;
                    else {
                        var pHelper = p.getCdrHelper();
                        var qHelper = q.getCdrHelper();
                        if (pHelper && qHelper) {
                            ans = pHelper.equals(qHelper);
                        } else if (pHelper) {
                            ans = pHelper.resolvesTo(q);
                        } else if (qHelper) {
                            ans = qHelper.resolvesTo(p);
                        } else ans = false;
                    }

                        return ans;
                }
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
                } else if (p.isUndefined()) {
                    /* There is only one undefined value Datum:
                     Environment.prototype.unspecifiedSentinel. */
                    return p === q;
                } else return false;

            } else return false;
        }
    },
    /* From the description of eq? at R5RS 6.1, it looks like it is
     permissible for it to have exactly the same semantics as eqv?. */
    'eq?':{
        argc:2,
        proc: function(p, q) {
            // todo bl is there a faster and cleaner way to alias this?
            return r5js.builtins['equiv']['eqv?'].proc(p, q);
        }
    }
};

r5js.builtins['type'] = {

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
            return (p instanceof r5js.Datum && p.isProcedure())
                || p instanceof r5js.Continuation;
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

    'port?': {
        argc: 1,
        proc: function(datum) {
            return datum.isPort();
        }
    },

    'input-port?': {
        argc: 1,
        proc: function(datum) {
            return datum.isInputPort();
        }
    },

    'output-port?': {
        argc: 1,
        proc: function(datum) {
            return datum.isOutputPort();
        }
    }
};

r5js.builtins['number'] = {

    'complex?': {
        argc: 1,
        proc: function(x) {
            return x.isNumber();
        }
    },

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
                throw new r5js.PrimitiveProcedureError('remainder: undefined for 0');
            // The JavaScript % semantics are precisely the Scheme remainder semantics.
            else return p % q;
        }
    },

    'quotient': {
        argc: 2,
        argtypes: 'number',
        proc: function(p, q) {
            if (q === 0)
                throw new r5js.PrimitiveProcedureError('quotient: undefined for 0');
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
                throw new r5js.PrimitiveProcedureError('modulo: undefined for 0');
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
                    throw new r5js.TooManyArgs('atan', 2, arguments.length);
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
            throw new r5js.UnimplementedOptionError('make-rectangular');
        }
    },
    'make-polar': {
        argc: 2,
        argtypes: 'number',
        proc: function(x, y) {
            throw new r5js.UnimplementedOptionError('make-polar');
        }
    },
    'real-part': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new r5js.UnimplementedOptionError('real-part');
        }
    },
    'imag-part': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new r5js.UnimplementedOptionError('imag-part');
        }
    },
    'magnitude': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new r5js.UnimplementedOptionError('magnitude');
        }
    },
    'angle': {
        argc: 1,
        argtypes: 'number',
        proc: function(z) {
            throw new r5js.UnimplementedOptionError('angle');
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
            return r5js.data.newIdOrLiteral(x + '', 'string');
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

r5js.builtins['pair'] = {

    'cons': {
        argc: 2,
        proc: function(car, cdr) {
            // todo bl this is really expensive! can we cut down on the copying?
            var realCar = car.clone();
            var realCdr = cdr.clone();
            // Since cdr already has a "head of list" node, reuse that. Convoluted eh?
            if (realCdr.isList() || realCdr.isImproperList()) {
                realCdr.prependChild(realCar);
                return realCdr;
            } else {
                var ans = new r5js.Datum();
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
            var ans;
            if (startOfCdr) {
                ans = (startOfCdr.nextSibling || p.isList())
                    ? startOfCdr.siblingsToList(p.isImproperList())
                    : startOfCdr;
                return ans.setCdrHelper(new r5js.CdrHelper(p, startOfCdr));
            } else return newEmptyList();
        }
    },

    'set-car!': {
        argc: 2,
        proc: function(p, car) {
            if (p.isList() || p.isImproperList()) {

                if (p.isImmutable())
                    throw new r5js.ImmutableError(p.toString());

                car.nextSibling = p.firstChild.nextSibling;
                p.firstChild = car;

                for (var helper = p.getCdrHelper();
                     helper;
                     helper = helper.getCdrHelper())
                    helper.setCar(car);

                return null; // unspecified return value
            } else throw new r5js.ArgumentTypeError(p, 0, 'set-car!', 'pair');
        }
    },

    'set-cdr!': {
        argc: 2,
        proc: function(p, cdr) {
            if (p.isList() || p.isImproperList()) {

                if (p.isImmutable())
                    throw new r5js.ImmutableError(p.toString());

                if (cdr.isList()) {
                    p.firstChild.nextSibling = cdr.firstChild;
                    p.type = '(';
                } else {
                    p.firstChild.nextSibling = cdr;
                    p.type = '.(';
                }

                for (var helper = p.getCdrHelper();
                    helper; helper = helper.getCdrHelper()) {
                    helper.setCdr(cdr);
                }

                return null; // unspecified return value
            } else throw new r5js.ArgumentTypeError(p, 0, 'set-cdr!', 'pair');
        }
    }
};

r5js.builtins['symbol'] = {

    'symbol->string': {
        argc: 1,
        argtypes: ['symbol'],
        proc: function(sym) {
            return r5js.data.newIdOrLiteral(sym, 'string').setImmutable();
        }
    },

    'string->symbol': {
        argc: 1,
        argtypes: ['string'],
        proc: function(node) {
            return r5js.data.newIdOrLiteral(node.payload, 'identifier');
        }
    }
};

r5js.builtins['char'] = {

    'char=?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(node1, node2) {
            return node1.payload === node2.payload;
        }
    },
    'char<?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(node1, node2) {
            return node1.payload < node2.payload;
        }
    },
    'char>?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(node1, node2) {
            return node1.payload > node2.payload;
        }
    },
    'char<=?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(node1, node2) {
            return node1.payload <= node2.payload;
        }
    },
    'char>=?': {
        argc: 2,
        argtypes: ['char', 'char'],
        proc: function(node1, node2) {
            return node1.payload >= node2.payload;
        }
    },
    'char->integer': {
        argc: 1,
        argtypes: ['char'],
        proc: function(node) {
            return node.payload.charCodeAt(0);
        }
    },
    'integer->char': {
        argc: 1,
        argtypes: ['number'],
        proc: function(i) {
            return r5js.data.newIdOrLiteral(String.fromCharCode(i), 'character');
        }
    },
    'char-upcase': {
        argc: 1,
        argtypes: ['char'],
        proc: function(node) {
            return r5js.data.newIdOrLiteral(node.payload.toUpperCase(), 'character');
        }
    },
    'char-downcase': {
        argc: 1,
        argtypes: ['char'],
        proc: function(node) {
            return r5js.data.newIdOrLiteral(node.payload.toLowerCase(), 'character');
        }
    }
};

r5js.builtins['string'] = {

    'make-string': {
        argc: {min: 1, max: 2},
        argtypes: ['number', 'char'],
        proc: function(n, charNode) {
            /* R5RS 6.3.5: "If char is given, then all elements of the
             string are initialized to char, otherwise the contents
             of the string are unspecified." */
            var c = charNode ? charNode.payload : ' ';
            var s = '';
            for (var i = 0; i < n; ++i)
                s += c;
            return r5js.data.newIdOrLiteral(s, 'string');
        }
    },
    'string-length': {
        argc: 1,
        argtypes: ['string'],
        proc: function(node) {
            return node.payload.length;
        }
    },
    'string-ref': {
        argc: 2,
        argtypes: ['string', 'number'],
        proc: function(node, i) {
            return r5js.data.newIdOrLiteral(node.payload.charAt(i), 'character');
        }
    },
    'string-set!': {
        argc: 3,
        proc: function stringSet(str, k, c) {

            if (!str.isString())
                throw new r5js.ArgumentTypeError(str, 0, 'string-set!', 'string');
            if (!k.isNumber())
                throw new r5js.ArgumentTypeError(k, 1, 'string-set!', 'number');
            if (!c.isCharacter())
                throw new r5js.ArgumentTypeError(c, 2, 'string-set!', 'character');

            if (str.isImmutable())
                throw new r5js.ImmutableError(str.payload);

            var s = str.payload;

            str.payload = s.substr(0, k.payload)
                + c.payload
                + s.substr(k.payload + 1);

            return null; // unspecified return value
        }
    }
};

r5js.builtins['vector'] = {

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
                throw new r5js.ArgumentTypeError(n, 0, 'make-vector', 'number');
            /* R5RS 6.3.6: "If a second argument is given, then each
             element is initialized to fill. Otherwise the initial
             contents of each element is unspecified."

             False seems like a good default. */
            fill = fill || r5js.data.newIdOrLiteral(false, 'boolean');
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
                throw new r5js.ArgumentTypeError(v, 0, 'vector-set!', 'vector');
            else if (typeof k !== 'number')
                throw new r5js.ArgumentTypeError(k, 1, 'vector-set!', 'number');

            if (v.isImmutable())
                throw new r5js.ImmutableError(v.toString());

            if (v.isArrayBacked())
                v.payload[k] = fill;
            else
                v.convertVectorToArrayBacked().payload[k] = fill;

            // todo bl requires a cycle-labeling procedure like set-car! and set-cdr!

            return null;
        }
    }
};

r5js.builtins['control'] = {

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
                throw new r5js.ArgumentTypeError(mustBeProc, 0, 'apply', 'procedure');

            var curProcCall = arguments[arguments.length - 3];
            /* todo bl: very little idea what's going on here, but we seem to
             use both sources of procName. */
            var procName = r5js.data.newIdOrLiteral(curProcCall.firstOperand.payload || mustBeProc.name);
            var continuation = arguments[arguments.length - 2];
            var resultStruct = arguments[arguments.length - 1];

            var lastRealArgIndex = arguments.length - 4;
            var mustBeList = arguments[lastRealArgIndex];
            if (!mustBeList.isList())
                throw new r5js.ArgumentTypeError(mustBeList, lastRealArgIndex, 'apply', 'list');

            // (apply foo '(x y z))
            if (lastRealArgIndex === 1) {
                var newArgs = new r5js.SiblingBuffer();
                // todo bl document why we are quoting the arguments
                for (var arg = mustBeList.firstChild; arg; arg = arg.nextSibling)
                    newArgs.appendSibling(arg.quote());
                var actualProcCall = r5js.procs.newProcCall(procName, newArgs.toSiblings(), continuation);
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
                var actualProcCall = r5js.procs.newProcCall(procName, newArgs.firstChild, continuation);
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
            var dummyProcCall = r5js.procs.newProcCall(procCall.firstOperand, continuation, continuation);
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
            var producerContinuation = new r5js.Continuation(valuesName);
            var producerCall = r5js.procs.newProcCall(
                procCall.firstOperand,
                null, // no arguments
                producerContinuation);
            producerCall.setStartingEnv(procCall.env);
            var consumerCall = r5js.procs.newProcCall(
                procCall.firstOperand.nextSibling,
                r5js.data.newIdOrLiteral(valuesName),
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

            // TODO bl: the compiler thinks there's already a variable named
	    // "before" in scope here. Figure out why.
	    var before2 = newCpsName();

            // None of the three thunks have any arguments.

            // todo bl use a ContinuableBuffer for efficiency

            var procCallBefore = r5js.procs.newProcCall(
                procCall.firstOperand,
                null, // no arguments
                new r5js.Continuation(before2)
            );


            var procCallAfter = r5js.procs.newProcCall(
                procCall.firstOperand.nextSibling.nextSibling,
                null, // no arguments
                new r5js.Continuation(newCpsName())
            );

            var result = newCpsName();
            procCallAfter.appendContinuable(newIdShim(r5js.data.newIdOrLiteral(result), newCpsName()));
            procCallAfter.getLastContinuable().continuation = continuation;

            var procCallThunk = r5js.procs.newProcCall(
                procCall.firstOperand.nextSibling,
                null, // no arguments
                new r5js.Continuation(result)
            );

            procCallThunk.appendContinuable(procCallAfter);
            procCallBefore.appendContinuable(procCallThunk);

            resultStruct.nextContinuable = procCallBefore;
            /* We use the TrampolineResultStruct to store the thunk.
             This should be okay because dynamic-wind is the only one
             who writes to it, and call/cc is the only one who reads it.

             todo bl document why we cannot reuse procCallBefore. */
            resultStruct.beforeThunk = r5js.procs.newProcCall(
                procCall.firstOperand,
                null,
                new r5js.Continuation(before2));
        }
    }
};

r5js.builtins['eval'] = {
    'eval': {
        argc: 2,
        proc: function(expr, envSpec) {
            if (!(expr instanceof r5js.Datum))
                throw new r5js.ArgumentTypeError(expr, 0, 'eval', 'datum');
            if (!(envSpec instanceof r5js.Datum) || !envSpec.isEnvironmentSpecifier())
                throw new r5js.ArgumentTypeError(envSpec, 1, 'eval', 'environment-specifier');

            /* An interesting special case. If we're about to evaluate a wrapped
             procedure (primitive JavaScript or SchemeProcedure), return its name
             (= external representation) instead. Example:

             (eval + (null-environment 5))

             The answer is (the external representation) +, even though the identifier
             + is not bound in the null environment. Why? eval, like every procedure,
             receives its arguments already evaluated, and the value of the identifier
             + in the regular environment is the primitive procedure for addition.
             But if we were to pass this Datum-wrapped procedure into the parser,
             it would not know what to do with it and parsing would fail.

             todo bl: are there any other cases where a procedure can
             escape into the parser? */

            if (expr && expr.isProcedure())
                return r5js.data.newIdOrLiteral(expr.name);

            else {
                /* Call the parse/desugar/eval portions of the interpreter
                 pipeline manually. It would be nice to reuse the code in
                 api.js, but it made for some awkward forward references.
                 Reasoning about this copy/pasted code is simpler than
                 reasoning about the build process. */

                var env = envSpec.payload;
                // don't accidentally evaluate the next expr!
                expr.nextSibling = null;

                var parsed = new r5js.Parser(expr).parse();
                if (!parsed)
                    throw new r5js.ParseError(expr);
                var continuable = parsed.desugar(env).setStartingEnv(env);
                return r5js.trampoline(
                    continuable,
                    null,
                    null,
                    r5js.util.Logger.getLogger('[embedded eval]'));
            }
        }
    },
    /* This is not part of any Scheme standard, but it should be useful to
     test Scheme expressions that should not evaluate. */
    'will-eval?': {
        argc: 2,
        proc: function(expr, envSpec) {
            try {
                r5js.builtins['eval']['eval'].proc(expr, envSpec);
                return true;
            } catch (e) {
                return false;
            }
        }
    },
    'scheme-report-environment': {
        argc: 1,
        argtypes: ['number'],
        proc: function(num) {
            if (num === 5)
                return newEnvironmentSpecifier(
                    /** @type {!r5js.IEnvironment} */(r5js.PrimitiveProcedures.r5RSEnv_));
            else throw new r5js.InternalInterpreterError(
                'unsupported scheme report environment ' + num);
        }
    },
    'null-environment': {
        argc: 1,
        argtypes: ['number'],
        proc: function(num) {
            if (num === 5)
                return newEnvironmentSpecifier(
                    /** @type {!r5js.IEnvironment} */ (r5js.PrimitiveProcedures.nullEnv_));
            else throw new r5js.InternalInterpreterError(
                'unsupported null environment ' + num);
        }
    }
};

r5js.builtins['io'] = {

    /* Important: several of the primitive IO procedures delegate actual
     work by calling methods on the payload of the input- or output-port
     Datum. (This was done to collect all the filesystem-aware code in one
     place, as it may not be relevant for browser environments.) Such a
     payload can be anything, but it must provide the functions defined
     in the Port "interface": close, read-char, write-char, and so on.

     Unfortunately, this polymorphism interacts poorly with the Google
     Closure Compiler, which renames the functions both at the call site
     and in the implementation class definition (and to different names,
     which is the problem). To defeat this, the call and definition sites must
     both access the functions via string literals, not properties:
     datum.payload['write'](), not datum.payload.write(). */

    'current-input-port': {
        argc: 0,
        needsCurrentPorts: true, // pushes current input, current output
        proc: function() {
            return arguments[0];
        }
    },
    'current-output-port': {
        argc: 0,
        needsCurrentPorts: true, // pushes current input, current output
        proc: function() {
            return arguments[1];
        }
    },
    'open-input-file': {
        argc: 1,
        argtypes: ['string'],
        proc: function(datum) {
            return r5js.data.newInputPortDatum(
                new r5js.NodeBackedPort(datum.payload, 'r'));
        }
    },
    'open-output-file': {
        argc: 1,
        argtypes: ['string'],
        proc: function(datum) {
            return r5js.data.newOutputPortDatum(
                new r5js.NodeBackedPort(datum.payload, 'w'));
        }
    },
    'close-input-port': {
        argc: 1,
        argtypes: ['input-port'],
        proc: function(datum) {
            datum.payload['close']();
            return null;
        }
    },
    'close-output-port': {
        argc: 1,
        argtypes: ['output-port'],
        proc: function(datum) {
            datum.payload['close']();
            return null;
        }
    },
    'read-char': {
        needsCurrentPorts: true,
        proc: function() {
            var numUserArgs = arguments.length - 2;
            if (numUserArgs === 0 || numUserArgs === 1) {
                var inputPort = (numUserArgs === 0)
                    ? arguments[arguments.length - 2]
                    : arguments[0];
                if (!inputPort.isInputPort()) {
                    throw new r5js.ArgumentTypeError(inputPort, 0, 'read-char', 'input-port');
                } else if (inputPort.payload['isEof']()) {
                    /* R5RS 6.6.2: "If no more characters are available,
                     an end of file object is returned." */
                    return inputPort;
                } else return r5js.data.newIdOrLiteral(inputPort.payload['readChar'](), 'character');
            } else throw new r5js.TooManyArgs('read-char', 1, numUserArgs);
        }
    },
    'peek-char': {
        needsCurrentPorts: true,
        proc: function() {
            var numUserArgs = arguments.length - 2;
            if (numUserArgs === 0 || numUserArgs === 1) {
                var inputPort = (numUserArgs === 0)
                    ? arguments[arguments.length - 2]
                    : arguments[0];
                if (!inputPort.isInputPort()) {
                    throw new r5js.ArgumentTypeError(inputPort, 0, 'read-char', 'input-port');
                } else if (inputPort.payload['isEof']()) {
                    /* R5RS 6.6.2: "If no more characters are available,
                     an end of file object is returned." */
                    return inputPort;
                } else return r5js.data.newIdOrLiteral(inputPort.payload['peekChar'](), 'character');
            } else throw new r5js.TooManyArgs('read-char', 1, numUserArgs);
        }
    },
    'eof-object?': {
        argc: 1,
        proc: function(port) {
            return port instanceof r5js.Datum
                && port.isPort()
                && port.payload['isEof']();
        }
    },
    'char-ready?': {
        needsCurrentPorts: true,
        proc: function() {
            var numUserArgs = arguments.length-2;
            if (numUserArgs === 0 || numUserArgs === 1) {
                var inputPort = (numUserArgs === 0)
                    ? arguments[arguments.length-2]
                    : arguments[0];
                if (!inputPort.isInputPort()) {
                    throw new r5js.ArgumentTypeError(inputPort, 0, 'char-ready?', 'input-port');
                } else if (inputPort.payload['isEof']()) {
                    /* R5RS 6.6.2: "If the port is at end of file then
                     char-ready? returns true." (Because the next call to
                     read-char is guaranteed not to block -- it'll return EOF.) */
                    return true;
                } else return inputPort.payload['isCharReady']();
            } else throw new r5js.TooManyArgs('char-ready?', 1, arguments.length);
        }
    },
    'write': {
        needsCurrentPorts: true,
        proc: function() {
            var numUserArgs = arguments.length - 2;
            if (numUserArgs === 0) {
                throw new r5js.TooFewArgs('write', 1, numUserArgs);
            } else if (numUserArgs === 1 || numUserArgs === 2) {
                var x = arguments[0];
                var outputPort = (numUserArgs === 1)
                    ? arguments[arguments.length - 1]
                    : arguments[1];
                if (!outputPort.isOutputPort())
                    throw new r5js.ArgumentTypeError(outputPort, 1, 'write', 'output-port');
                var toWrite = x instanceof r5js.Datum
                    ? x.stringForOutputMode(r5js.OutputMode.WRITE)
                    : String(x);
                /* Port implementations aren't required to implement
                 write. If they don't, we just call writeChar (which they
                 must implement) on every single character. */
                if (outputPort.payload['write']) {
                    outputPort.payload['write'](toWrite);
                } else {
                    for (var i = 0; i < toWrite.length; ++i)
                        outputPort.payload['writeChar'](toWrite[i]);
                }
                return null; // unspecified return value
            } else throw new r5js.TooManyArgs('write', 2, numUserArgs);
        }


    },
    'write-char': {
        needsCurrentPorts: true,
        proc: function() {
            var numUserArgs = arguments.length - 2;
            if (numUserArgs === 0) {
                throw new r5js.TooFewArgs('write-char', 1, 0);
            } else if (numUserArgs === 1 || numUserArgs === 2) {
                var c = arguments[0];
                if (!c.isCharacter())
                    throw new r5js.ArgumentTypeError(c, 0, 'write-char', 'character');
                var outputPort = (numUserArgs === 1)
                    ? arguments[arguments.length - 1]
                    : arguments[1];
                if (!outputPort.isOutputPort())
                    throw new r5js.ArgumentTypeError(outputPort, 1, 'write-char', 'output-port');

                outputPort.payload['writeChar'](c.payload);
            } else throw new r5js.TooManyArgs('write-char', 2, numUserArgs);
            return null;
        }
    },
    'display': {
        /* According to R5RS 6.6.3, display is supposed to be a library
            procedure. Since the only non-library output routine is write-char,
            display would presumably have to be written in terms of write-char.
            That's not too efficient, so I decided to write it in JavaScript. */
        needsCurrentPorts: true,
        proc: function() {
            var numUserArgs = arguments.length - 2;
            if (numUserArgs === 0) {
                throw new r5js.TooFewArgs('display', 1, numUserArgs);
            } else if (numUserArgs === 1 || numUserArgs === 2) {
                var x = arguments[0];
                /* If the programmer gave an output port, use that,
                 otherwise use the current output port. */
                var outputPort = (numUserArgs === 1)
                    ? arguments[arguments.length - 1]
                    : arguments[1];
                if (!outputPort.isOutputPort())
                    throw new r5js.ArgumentTypeError(outputPort, 1, 'display', 'output-port');
                var toWrite = x instanceof r5js.Datum
                    ? x.stringForOutputMode(r5js.OutputMode.DISPLAY)
                    : String(x);
                /* Port implementations aren't required to implement
                 write. If they don't, we just call writeChar (which they
                 must implement) on every single character. */
                if (outputPort.payload['write']) {
                    outputPort.payload['write'](toWrite);
                } else {
                    for (var i = 0; i < toWrite.length; ++i)
                        outputPort.payload['writeChar'](toWrite[i]);
                }
                return null; // unspecified return value
            } else throw new r5js.TooManyArgs('display', 2, numUserArgs);
        }
    }
};


/** @private {r5js.IEnvironment} */
r5js.PrimitiveProcedures.nullEnv_;

/** @private {r5js.IEnvironment} */
r5js.PrimitiveProcedures.r5RSEnv_;


/**
 * @param {!r5js.IEnvironment} nullEnv The null environment, needed by
 *     the eval primitive procedure.
 * @param {!r5js.IEnvironment} r5RSEnv The R5RS environment, needed by
 *     the eval primitive procedure.
 * @param {!r5js.util.Logger} logger Logger.
 */
r5js.PrimitiveProcedures.install = function(nullEnv, r5RSEnv, logger) {
    r5js.PrimitiveProcedures.nullEnv_ = nullEnv;
    r5js.PrimitiveProcedures.r5RSEnv_ = r5RSEnv;

  for (var category in r5js.builtins) {
    var procs = r5js.builtins[category];
    for (var name in procs)
      r5js.PrimitiveProcedures.install_(name, procs[name], r5RSEnv, logger);
  }

  /* Experimental Scheme->JS FFI is browser-only for now.
     I used to have if (this.window === this), which is cleverer but
     doesn't work for strict mode. (Thanks, Stack Overflow!) */
  if (Function('return this;')().window) {
    r5RSEnv.addBinding(
        'window',
        r5js.ffiutil.newFFIDatum(new r5js.JsObjOrMethod(window)));
    for (var name in r5js.ffi) {
      r5js.PrimitiveProcedures.install_(name, r5js.ffi[name], r5RSEnv, logger);
    }
  }
};


/**
 * @param {string} name
 * @param {?} definition
 * @param {!r5js.IEnvironment} targetEnv Environment the builtin should be
 *        registered in.
 * @param {!r5js.util.Logger} logger Logger, for informational messages.
 * @return {?}
 * @private
 * TODO bl this function is too long. Split apart.
 */
r5js.PrimitiveProcedures.install_ = function(name, definition, targetEnv, logger) {

  var argc = definition.argc;
  var argtypes = definition.argtypes;
  var resultType = definition.resultType;
  var proc = definition.proc;

  if (!proc) {
    logger.warning('builtin ' + name + ' unspecified, skipping');
    return targetEnv;
  }

  if (targetEnv.hasBinding(name)) {
    logger.warning('redefining ' + name);
  }

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
      else if (definition.needsCurrentPorts)
        numArgsFromUser -= 2;

      // If argc is a number, it means exactly that many args are required
      if (typeof argc === 'number' && numArgsFromUser !== argc)
        throw new r5js.IncorrectNumArgs(name, argc, numArgsFromUser);
      else if (argc.min && numArgsFromUser < argc.min)
        throw new r5js.TooFewArgs(name, argc.min, numArgsFromUser);
      else if (argc.max && numArgsFromUser > argc.max)
        throw new r5js.TooManyArgs(name, argc.max, numArgsFromUser);
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
            maybeUnwrappedArgs.push(arguments[i] instanceof r5js.Datum ? arguments[i].unwrap() : arguments[i]);
          else
            throw new r5js.ArgumentTypeError(arguments[i], i, name, argtypes);
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
            throw new r5js.ArgumentTypeError(arguments[i], i, name, argtypes[i]);
          maybeUnwrappedArgs.push(arguments[i] instanceof r5js.Datum ? arguments[i].unwrap() : arguments[i]);
        }
      }
    }

  // If no type checking was requested, don't unwrap the args
    else maybeUnwrappedArgs = arguments;

    var returnValue = proc.apply(null, maybeUnwrappedArgs);
    return definition.hasSpecialEvalLogic
            ? null /* A function with special eval logic will set the trampolineResultStruct directly. */
            : r5js.data.maybeWrapResult(returnValue, resultType);
  };
  /* We are setting a boolean flag on a JavaScript function object.
     Not sure this is good style, but it saves us having to wrap
     the function in some other object to signal to the trampoline
     that it has special evaluation logic. */
  if (definition.hasSpecialEvalLogic)
    binding.hasSpecialEvalLogic = true;
  else if (definition.needsCurrentPorts)
    binding.needsCurrentPorts = true;
  targetEnv.addBinding(name, binding);
};

function requirePresenceOf(name, argtypes, targetEnv) {
  if (typeof argtypes === 'string' && !targetEnv.hasBinding(argtypes + '?'))
    throw new r5js.InternalInterpreterError('builtin procedure '
            + name
            + ' requires an argument to have type '
            + argtypes
            + ", but the default environment doesn't know about that type yet");
  else if (argtypes instanceof Array) {
    for (var i = 0; i < argtypes.length; ++i)
      if (typeof argtypes[i] === 'string' && !targetEnv.hasBinding(argtypes[i] + '?'))
        throw new r5js.InternalInterpreterError('builtin procedure '
                    + name
                    + ' requires an argument to have type '
                    + argtypes[i]
                    + ", but the default environment doesn't know about that type yet");
  }
}
