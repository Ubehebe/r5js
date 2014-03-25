goog.provide('r5js.PrimitiveProcedures');


goog.require('goog.log');
goog.require('r5js.CdrHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.DottedList');
goog.require('r5js.Lambda');
goog.require('r5js.List');
goog.require('r5js.OutputMode');
goog.require('r5js.PrimitiveProcedureError');
goog.require('r5js.Quasiquote');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooManyArgs');
goog.require('r5js.UnimplementedOptionError');
goog.require('r5js.ast.Boolean');
goog.require('r5js.ast.Character');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.InputPort');
goog.require('r5js.ast.Number');
goog.require('r5js.ast.OutputPort');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.String');
goog.require('r5js.ast.Vector');
goog.require('r5js.parse.Terminals');
goog.require('r5js.procspec');


/** @private {r5js.IEnvironment} */
r5js.PrimitiveProcedures.nullEnv_;


/** @private {r5js.IEnvironment} */
r5js.PrimitiveProcedures.r5RSEnv_;


/** @const @private {!Object.<string, !r5js.PrimitiveProcedure>} */
r5js.PrimitiveProcedures.registry_ = {};


goog.scope(function() {
var _ = r5js.procspec;
var PrimitiveProcedures = r5js.PrimitiveProcedures.registry_;

// Equivalence-related procedures

/* From the description of eq? at R5RS 6.1, it looks like it is
     permissible for eq? to have exactly the same semantics as eqv?. */
PrimitiveProcedures['eqv?'] = PrimitiveProcedures['eq?'] =
    _.binary(function(p, q) {
  /* This implementation closely follows the description of eqv?
             in R5RS 6.1, which explicitly leaves some comparisons undefined. */

  if (!p.sameTypeAs(q)) {
    return false;
  }

  if (p instanceof r5js.ast.Boolean) {
    return p.getPayload() === q.getPayload();
  } else if (p instanceof r5js.ast.Identifier) {
    return p.getPayload() === q.getPayload();
  } else if (p instanceof r5js.ast.Number) {
    return p.getPayload() === q.getPayload(); // todo bl numerical precision...
  } else if (p instanceof r5js.ast.Character) {
    return p.getPayload() === q.getPayload();
  } else if (p instanceof r5js.List) {
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
  } else if (p.isImproperList()) {
    return p === q;
  } else if (p instanceof r5js.ast.Vector) {
    return (p.isArrayBacked() && q.isArrayBacked()) ?
        p.getPayload() === q.getPayload() :
        p === q;
  } else if (p instanceof r5js.ast.String) {
    return p === q;
  } else if (p instanceof r5js.Lambda) {
    return p.getPayload() === q.getPayload();
  } else if (p instanceof r5js.Quasiquote) {
    /* todo bl: not sure this is the right thing to do.
    We can't just "unescape" the quasiquotations. Example:

    (equal? '(a `(b ,(+ 1 2))) '(a `(b ,(+ 1 2))))

    This will eventually call

    (eqv? `(b ,(+ 1 2)) `(b ,(+ 1 2)))

    From this procedure call, it looks as if we should unescape
    the quasiquotes, but that's incorrect; we've lost the surrounding
    quotation level.

    It may be possible to figure out what to do based on the qqLevels,
    but it's been a while since I've looked at that subsystem. */
    return p.isEqual(q);
  } else return false;
});

// Type-related procedures

PrimitiveProcedures['boolean?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Boolean;
});

PrimitiveProcedures['char?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Character;
});

PrimitiveProcedures['input-port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.InputPort;
});

PrimitiveProcedures['null?'] = _.unary(function(node) {
  return node.isEmptyList();
});

PrimitiveProcedures['number?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Number;
});

PrimitiveProcedures['output-port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.OutputPort;
});

PrimitiveProcedures['pair?'] = _.unary(function(node) {
  return (node instanceof r5js.List ||
      node.isImproperList() ||
      node instanceof r5js.ast.Quote) &&
      !!node.getFirstChild(); // 3.2: (pair? '()) => #f
});

PrimitiveProcedures['port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.InputPort ||
      node instanceof r5js.ast.OutputPort;
});

PrimitiveProcedures['procedure?'] = _.unary(function(node) {
  /* R5RS 6.4: "The procedure call-with-current-continuation
         packages up the current continuation as an "escape procedure"
         and passes it as an argument to proc." Thus a Continuation
         must count as a procedure. */
  return node instanceof r5js.Lambda ||
      node instanceof r5js.Continuation;
});

PrimitiveProcedures['string?'] = _.unary(function(node) {
  return node instanceof r5js.ast.String;
});

PrimitiveProcedures['symbol?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Identifier;
});

PrimitiveProcedures['vector?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Vector;
});

// Number-related procedures

PrimitiveProcedures['='] = _.varargsAtLeast0(function() {
  for (var i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] !== arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['/'] = _.varargsAtLeast1(function() {
  if (arguments.length === 1) { // unary
    return 1 / arguments[0];
  } else { // varargs: (x1 / x2) / x3 etc
    var ans = arguments[0];
    for (var i = 1; i < arguments.length; ++i)
      ans /= arguments[i];
    return ans;
  }
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['-'] = _.varargsAtLeast1(function() {
  if (arguments.length === 1) { // unary
    return -1 * arguments[0];
  } else { // varargs: (x1 - x2) - x3 etc
    var ans = arguments[0];
    for (var i = 1; i < arguments.length; ++i) {
      ans -= arguments[i];
    }
    return ans;
  }
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['*'] = _.varargsAtLeast0(function() {
  var product = 1;
  for (var i = 0; i < arguments.length; ++i) {
    product *= arguments[i];
  }
  return product;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['+'] = _.varargsAtLeast0(function() {
  var sum = 0;
  for (var i = 0; i < arguments.length; ++i) {
    sum += arguments[i];
  }
  return sum;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['>='] = _.varargsAtLeast0(function() {
  for (var i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] < arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['>'] = _.varargsAtLeast0(function() {
  for (var i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] <= arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['<='] = _.varargsAtLeast0(function() {
  for (var i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] > arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['<'] = _.varargsAtLeast0(function() {
  for (var i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] >= arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['angle'] = _.unary(function(z) {
  throw new r5js.UnimplementedOptionError('angle');
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['acos'] = _.unary(Math.acos, r5js.DatumType.NUMBER);

PrimitiveProcedures['asin'] = _.unary(Math.asin, r5js.DatumType.NUMBER);

PrimitiveProcedures['atan'] = _.varargsAtLeast1(function() {
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
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['ceiling'] = _.unary(Math.ceil, r5js.DatumType.NUMBER);

PrimitiveProcedures['complex?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Number;
});

PrimitiveProcedures['cos'] = _.unary(Math.cos, r5js.DatumType.NUMBER);

PrimitiveProcedures['exact?'] = _.unary(function(x) {
  return false; // In JavaScript every number is a double.
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['exact->inexact'] = _.unary(function(x) {
  return x; // In JavaScript every number is inexact
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['exp'] = _.unary(Math.exp, r5js.DatumType.NUMBER);

PrimitiveProcedures['expt'] = _.binary(
    Math.pow, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);

PrimitiveProcedures['floor'] = _.unary(Math.floor, r5js.DatumType.NUMBER);

PrimitiveProcedures['imag-part'] = _.unary(function(z) {
  throw new r5js.UnimplementedOptionError('imag-part');
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['inexact?'] = _.unary(function(x) {
  return true;
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['inexact->exact'] = _.unary(function(x) {
  return x; // TODO bl
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['magnitude'] = _.unary(function(z) {
  throw new r5js.UnimplementedOptionError('magnitude');
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['make-polar'] = _.binary(function(x, y) {
  throw new r5js.UnimplementedOptionError('make-polar');
}, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);

PrimitiveProcedures['make-rectangular'] = _.binary(function(r, theta) {
  throw new r5js.UnimplementedOptionError('make-rectangular');
}, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);

PrimitiveProcedures['number->string'] = _.unary(function(x) {
  return new r5js.ast.String(x + '');
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['integer?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Number &&
      Math.round(node.getPayload()) === node.getPayload();
});

PrimitiveProcedures['log'] = _.unary(Math.log, r5js.DatumType.NUMBER);

PrimitiveProcedures['modulo'] = _.binary(function(p, q) {
  if (q === 0) {
    throw new r5js.PrimitiveProcedureError('modulo: undefined for 0');
  }
  var remainder = p % q;
  var sign = p * q;
  var ans = remainder;
  if (sign > 0) {
    // Both positive or both negative: remainder and modulo are the same
    return remainder;
  } else if (p > 0) {
    /* If p is positive and q is negative,
                 remainder will be positive and modulo will be negative */
    while (ans > 0)
      ans += q;
    return ans;
  } else {
    /* If p is negative and q is positive,
                 remainder will be negative and modulo will be positive */
    while (ans < 0) {
      ans += q;
    }
    return ans;
  }
}, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);

PrimitiveProcedures['quotient'] = _.binary(function(p, q) {
  if (q === 0) {
    throw new r5js.PrimitiveProcedureError('quotient: undefined for 0');
  }
  /* In Scheme, quotient rounds towards zero, which is unfortunately
                 not what JavaScript's Math.round() does. */
  var unrounded = p / q;
  return unrounded > 0 ? Math.floor(unrounded) : Math.ceil(unrounded);
}, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);

PrimitiveProcedures['rational?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Number;
});

PrimitiveProcedures['real?'] = _.unary(function(node) {
  return node instanceof r5js.ast.Number;
});

PrimitiveProcedures['real-part'] = _.unary(function(z) {
  throw new r5js.UnimplementedOptionError('real-part');
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['remainder'] = _.binary(function(p, q) {
  if (q === 0) {
    throw new r5js.PrimitiveProcedureError('remainder: undefined for 0');
  }
  // The JavaScript % semantics are precisely the Scheme remainder semantics.
  return p % q;
}, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);

PrimitiveProcedures['round'] = _.unary(function(x) {
  /* R5RS 6.2.5: "Round returns the closest integer to x,
             rounding to even when x is halfway between two integers." */
  var down = Math.floor(x);
  var downDiff = Math.abs(x - down);
  var up = Math.ceil(x);
  var upDiff = Math.abs(up - x);

  if (upDiff < downDiff) {
    return up;
  } else if (downDiff < upDiff) {
    return down;
  } else {
    return up % 2 ? down : up;
  }
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['sin'] = _.unary(Math.sin, r5js.DatumType.NUMBER);

PrimitiveProcedures['sqrt'] = _.unary(Math.sqrt, r5js.DatumType.NUMBER);

PrimitiveProcedures['string->number'] = _.unary(
    parseFloat, r5js.DatumType.STRING);

PrimitiveProcedures['tan'] = _.unary(Math.tan, r5js.DatumType.NUMBER);

PrimitiveProcedures['truncate'] = _.unary(function(x) {
  /* R5RS 6.2.5: "Truncate returns the integer closest to x
   whose absolute value is not larger than the absolute value of x." */
  return x > 0 ? Math.floor(x) : Math.ceil(x);
}, r5js.DatumType.NUMBER);

// Pair-related procedures

PrimitiveProcedures['car'] = _.unary(function(p) {
  return p.getFirstChild();
}, r5js.DatumType.PAIR);

PrimitiveProcedures['cdr'] = _.unary(function(p) {
  var startOfCdr = p.getFirstChild().getNextSibling();
  var ans;
  if (startOfCdr) {
    ans = (startOfCdr.getNextSibling() ||
        (p instanceof r5js.List && !p.isDirty())) ?
        startOfCdr.siblingsToList(p.isImproperList()) :
        startOfCdr;
    return ans.setCdrHelper(new r5js.CdrHelper(p, startOfCdr));
  } else {
    return new r5js.SiblingBuffer().toList(r5js.List);
  }
}, r5js.DatumType.PAIR);

PrimitiveProcedures['cons'] = _.binary(function(car, cdr) {
  // todo bl this is really expensive! can we cut down on the copying?
  var realCar = car.clone();
  var realCdr = cdr.clone();
  // Since cdr already has a "head of list" node, reuse that. Convoluted eh?
  if (realCdr instanceof r5js.List || realCdr.isImproperList()) {
    var oldFirstChild = realCdr.getFirstChild();
    realCdr.setFirstChild(realCar);
    realCar.setNextSibling(oldFirstChild);
    return realCdr;
  } else {
    var ans = new r5js.DottedList(realCar);
    ans.appendChild(realCdr);
    // todo bl hmm the parent field isn't getting set...is that ok?
    return ans;
  }
});

PrimitiveProcedures['set-car!'] = _.binary(function(p, car) {
  if (!(p instanceof r5js.List || p.isImproperList())) {
    throw new r5js.ArgumentTypeError(
        p, 0, 'set-car!', r5js.parse.Terminals.LPAREN);
  }
  if (p.isImmutable()) {
    throw new r5js.ImmutableError(p.toString());
  }

  car.setNextSibling(p.getFirstChild().getNextSibling());
  p.setFirstChild(car);

  var helper = p.getCdrHelper();
  if (helper) {
    helper.setCar(car);
  }

  return null; // unspecified return value
});

PrimitiveProcedures['set-cdr!'] = _.binary(function(p, cdr) {
  if (!(p instanceof r5js.List || p.isImproperList())) {
    throw new r5js.ArgumentTypeError(
        p, 0, 'set-cdr!', r5js.parse.Terminals.LPAREN);
  }

  if (p.isImmutable()) {
    throw new r5js.ImmutableError(p.toString());
  }

  if (cdr instanceof r5js.List) {
    p.getFirstChild().setNextSibling(cdr.getFirstChild());
    p.setType(r5js.parse.Terminals.LPAREN);
  } else {
    p.getFirstChild().setNextSibling(cdr);
    p.setType(r5js.parse.Terminals.LPAREN_DOT);
  }

  var helper = p.getCdrHelper();
  if (helper) {
    helper.setCdr(cdr);
  }

  if (p instanceof r5js.List) {
    p.markDirty();
  }

  return null; // unspecified return value
});

// Vector-related procedures

PrimitiveProcedures['make-vector'] = _.varargsRange(
    function(numberNode, fillNode) {
      var n = numberNode.unwrap();
      if (typeof n !== 'number') {
        throw new r5js.ArgumentTypeError(
            numberNode, 0, 'make-vector', r5js.DatumType.NUMBER);
      }
      /* R5RS 6.3.6: "If a second argument is given, then each element
         is initialized to fill. Otherwise the initial contents of each element
         is unspecified." False seems like a good default. */
      fillNode = fillNode || new r5js.ast.Boolean(false);
      var buf = [];
      for (var i = 0; i < n; ++i) {
        buf.push(fillNode.clone());
      }
      return new r5js.ast.Vector(buf);
    }, 1, 2);

PrimitiveProcedures['vector-length'] = _.unary(function(v) {
  return v.isArrayBacked() ?
      v.getPayload().length :
      v.convertVectorToArrayBacked().getPayload().length;
}, 'vector' /* TODO bl */);

PrimitiveProcedures['vector-ref'] = _.binary(function(v, k) {
  return v.isArrayBacked() ?
      v.getPayload()[k] :
      v.convertVectorToArrayBacked().getPayload()[k];
}, 'vector' /* TODO bl */, r5js.DatumType.NUMBER);

PrimitiveProcedures['vector-set!'] = _.ternary(function(v, k, fill) {
  v = v.unwrap();
  k = k.unwrap();

  if (!(v instanceof r5js.ast.Vector)) {
    throw new r5js.ArgumentTypeError(
        v, 0, 'vector-set!', r5js.DatumType.VECTOR);
  }
  if (typeof k !== 'number') {
    throw new r5js.ArgumentTypeError(
        k, 1, 'vector-set!', r5js.DatumType.NUMBER);
  }
  if (v.isImmutable()) {
    throw new r5js.ImmutableError(v.toString());
  }

  if (v.isArrayBacked()) {
    v.getPayload()[k] = fill;
  } else {
    v.convertVectorToArrayBacked().getPayload()[k] = fill;
  }

  // todo bl requires a cycle-labeling procedure like set-car! and set-cdr!

  return null;
});

// Symbol-related procedures

PrimitiveProcedures['symbol->string'] = _.unary(function(sym) {
  return new r5js.ast.String(sym).setImmutable();
}, r5js.DatumType.SYMBOL);

PrimitiveProcedures['string->symbol'] = _.unary(function(node) {
  // TODO bl it doesn't seem right to be creating Identifiers instead of Symbols
  return new r5js.ast.Identifier(node.getPayload());
});

// Character-related procedures

PrimitiveProcedures['char=?'] = _.binary(function(node1, node2) {
  return node1.getPayload() === node2.getPayload();
}, r5js.DatumType.CHARACTER, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char<?'] = _.binary(function(node1, node2) {
  return node1.getPayload() < node2.getPayload();
}, r5js.DatumType.CHARACTER, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char>?'] = _.binary(function(node1, node2) {
  return node1.getPayload() > node2.getPayload();
}, r5js.DatumType.CHARACTER, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char<=?'] = _.binary(function(node1, node2) {
  return node1.getPayload() <= node2.getPayload();
}, r5js.DatumType.CHARACTER, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char>=?'] = _.binary(function(node1, node2) {
  return node1.getPayload() >= node2.getPayload();
}, r5js.DatumType.CHARACTER, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char->integer'] = _.unary(function(node) {
  return node.getPayload().charCodeAt(0);
}, r5js.DatumType.CHARACTER);

PrimitiveProcedures['integer->char'] = _.unary(function(i) {
  return new r5js.ast.Character(String.fromCharCode(i));
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['char-upcase'] = _.unary(function(node) {
  return new r5js.ast.Character(node.getPayload().toUpperCase());
}, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char-downcase'] = _.unary(function(node) {
  return new r5js.ast.Character(node.getPayload().toLowerCase());
}, r5js.DatumType.CHARACTER);

// String-related procedures

PrimitiveProcedures['make-string'] = _.varargsRange(
    function(numberNode, charNode) {
      /* R5RS 6.3.5: "If char is given, then all elements of the
             string are initialized to char, otherwise the contents
             of the string are unspecified." */
      var c = goog.isDef(charNode) ? charNode.getPayload() : ' ';
      var n = numberNode.getPayload();
      var s = '';
      for (var i = 0; i < n; ++i) {
        s += c;
      }
      return new r5js.ast.String(s);
    }, 1, 2);

PrimitiveProcedures['string-length'] = _.unary(function(node) {
  return node.getPayload().length;
}, r5js.DatumType.STRING);

PrimitiveProcedures['string-ref'] = _.binary(function(node, i) {
  return new r5js.ast.Character(node.getPayload().charAt(i));
}, r5js.DatumType.STRING, r5js.DatumType.NUMBER);

PrimitiveProcedures['string-set!'] = _.ternary(function(str, k, c) {
  if (!(str instanceof r5js.ast.String)) {
    throw new r5js.ArgumentTypeError(
        str, 0, 'string-set!', r5js.DatumType.STRING);
  }
  if (!(k instanceof r5js.ast.Number)) {
    throw new r5js.ArgumentTypeError(
        k, 1, 'string-set!', r5js.DatumType.NUMBER);
  }
  if (!(c instanceof r5js.ast.Character)) {
    throw new r5js.ArgumentTypeError(
        c, 2, 'string-set!', r5js.DatumType.CHARACTER);
  }

  if (str.isImmutable()) {
    throw new r5js.ImmutableError(/** @type {string} */ (str.getPayload()));
  }

  var s = str.getPayload();

  var kNum = /** @type {number} */ (k.getPayload());

  str.setPayload(s.substr(0, kNum) + c.getPayload() + s.substr(kNum + 1));

  return null; // unspecified return value
});

// Vector-related procedures

// Evaluation-related procedures

PrimitiveProcedures['eval'] = _.binary(
    /** @suppress {checkTypes} */ function(expr, envSpec) {
      if (!(expr instanceof r5js.Datum))
        throw new r5js.ArgumentTypeError(
            expr, 0, 'eval', 'ref' /* TODO bl is this right? */);
      var isEnvNode = r5js.ast.Node.isImplementedBy(envSpec) &&
          envSpec instanceof r5js.ast.EnvironmentSpecifier;
      if (!isEnvNode) {
        throw new r5js.ArgumentTypeError(
            envSpec, 1, 'eval', r5js.DatumType.ENVIRONMENT_SPECIFIER);
      }
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

      if (expr instanceof r5js.Lambda)
        return new r5js.ast.Identifier(/** @type {string} */ (expr.getName()));

      else {
        /* Call the parse/desugar/eval portions of the interpreter pipeline
    manually. It would be nice to reuse the code in api.js, but it made
    for some awkward forward references. Reasoning about this copy/pasted
    code is simpler than reasoning about the build process. */

        var env = /** @type {!r5js.IEnvironment} */ (envSpec.getPayload());
        // don't accidentally evaluate the next expr!
        expr.setNextSibling(null);

        var parsed = new r5js.Parser(expr).parse();
        if (!parsed)
          throw new r5js.ParseError(expr);
        var continuable = parsed.desugar(env).setStartingEnv(env);
        return r5js.trampoline(
            continuable,
            null,
            null,
            goog.log.getLogger('[embedded eval]'));
      }
    });

/* This is not part of any Scheme standard, but it should be useful to
     test Scheme expressions that should not evaluate. */
PrimitiveProcedures['will-eval?'] = _.binary(function(expr, envSpec) {
  try {
    PrimitiveProcedures['eval'].fn_.call(null, expr, envSpec);
    return true;
  } catch (e) {
    return false;
  }
});

// I/O related procedures

PrimitiveProcedures['char-ready?'] = _.nullaryOrUnaryWithCurrentPorts(
    function(maybeUserSuppliedInputPort, inputPort) {
      var inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!(inputPortToUse instanceof r5js.ast.InputPort)) {
        throw new r5js.ArgumentTypeError(
            inputPortToUse, 0, 'char-ready?', r5js.DatumType.INPUT_PORT);
      }
      return inputPortToUse.getPayload().isCharReady();
    });

PrimitiveProcedures['close-input-port'] = _.unary(function(datum) {
  datum.getPayload().close();
  return null;
}, r5js.DatumType.INPUT_PORT);

PrimitiveProcedures['close-output-port'] = _.unary(function(datum) {
  datum.getPayload().close();
  return null;
}, r5js.DatumType.OUTPUT_PORT);

PrimitiveProcedures['current-input-port'] = _.nullaryWithCurrentPorts(
    function(inputPort) { return inputPort; });

PrimitiveProcedures['current-output-port'] = _.nullaryWithCurrentPorts(
    function(inputPort, outputPort) { return outputPort; });

/* According to R5RS 6.6.3, display is supposed to be a library
     procedure. Since the only non-library output routine is write-char,
     display would presumably have to be written in terms of write-char.
     That's not too efficient, so I decided to write it in JavaScript. */
PrimitiveProcedures['display'] = _.unaryOrBinaryWithCurrentPorts(
    function(datum, inputPort, outputPort) {
      if (!(outputPort instanceof r5js.ast.OutputPort)) {
        throw new r5js.ArgumentTypeError(
            outputPort, 1, 'display', r5js.DatumType.OUTPUT_PORT);
      }
      var toWrite = datum instanceof r5js.Datum ?
          datum.stringForOutputMode(r5js.OutputMode.DISPLAY) :
          String(datum);
      /* Port implementations aren't required to implement
             write. If they don't, we just call writeChar (which they
             must implement) on every single character. */
      if (outputPort.getPayload().write) {
        outputPort.getPayload().write(toWrite);
      } else {
        for (var i = 0; i < toWrite.length; ++i) {
          outputPort.getPayload().writeChar(toWrite[i]);
        }
      }
      return null; // unspecified return value
    });

PrimitiveProcedures['eof-object?'] = _.unary(function(port) {
  return false; // TODO bl add port tests
});

PrimitiveProcedures['open-input-file'] = _.unary(function(datum) {
  return new r5js.ast.InputPort(
      new r5js.NodeBackedPort(datum.getPayload(), 'r'));
}, r5js.DatumType.STRING);

PrimitiveProcedures['open-output-file'] = _.unary(function(datum) {
  return new r5js.ast.OutputPort(
      new r5js.NodeBackedPort(datum.getPayload(), 'w'));
}, r5js.DatumType.STRING);

PrimitiveProcedures['peek-char'] = _.nullaryOrUnaryWithCurrentPorts(
    function(maybeUserSuppliedInputPort, inputPort) {
      var inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!(inputPortToUse instanceof r5js.ast.InputPort)) {
        throw new r5js.ArgumentTypeError(
            inputPortToUse, 0, 'peek-char', r5js.DatumType.INPUT_PORT);
      }
      return new r5js.ast.Character(inputPortToUse.getPayload().peekChar());
    });

PrimitiveProcedures['read-char'] = _.nullaryOrUnaryWithCurrentPorts(
    function(maybeUserSuppliedInputPort, inputPort) {
      var inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!(inputPortToUse instanceof r5js.ast.InputPort)) {
        throw new r5js.ArgumentTypeError(
            inputPortToUse, 0, 'read-char', r5js.DatumType.INPUT_PORT);
      }
      return new r5js.ast.Character(inputPortToUse.getPayload().readChar());
    });

PrimitiveProcedures['write'] = _.unaryOrBinaryWithCurrentPorts(
    function(datum, maybeUserSuppliedOutputPort, inputPort, outputPort) {
      var outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!(outputPortToUse instanceof r5js.ast.OutputPort)) {
        throw new r5js.ArgumentTypeError(
            outputPortToUse, 1, 'write', r5js.DatumType.OUTPUT_PORT);
      }
      var toWrite = datum instanceof r5js.Datum ?
          datum.stringForOutputMode(r5js.OutputMode.WRITE) :
          String(datum);
      /* Port implementations aren't required to implement
            write. If they don't, we just call writeChar (which they
            must implement) on every single character. */
      if (outputPortToUse.getPayload().write) {
        outputPortToUse.getPayload().write(toWrite);
      } else {
        for (var i = 0; i < toWrite.length; ++i) {
          outputPortToUse.getPayload().writeChar(toWrite[i]);
        }
      }
      return null; // unspecified return value
    });

PrimitiveProcedures['write-char'] = _.unaryOrBinaryWithCurrentPorts(
    function(charNode, maybeUserSuppliedOutputPort, inputPort, outputPort) {
      if (!(charNode instanceof r5js.ast.Character)) {
        throw new r5js.ArgumentTypeError(
            charNode, 0, 'write-char', r5js.DatumType.CHARACTER);
      }
      var outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!(outputPortToUse instanceof r5js.ast.OutputPort)) {
        throw new r5js.ArgumentTypeError(
            outputPortToUse, 1, 'write-char', r5js.DatumType.OUTPUT_PORT);
      }
      outputPort.getPayload().writeChar(charNode.getPayload());
      return null; // unspecified return value
    });

// Control flow related procedures


/**
 * R5RS 6.4: (apply proc arg1 ... args)
 * "Proc must be a procedure and args must be a list.
 * Calls proc with the elements of the list
 * (append (list arg1 ...) args) as the actual arguments.
 */
PrimitiveProcedures['apply'] = _.atLeastNWithSpecialEvalLogic(2, function() {
  var mustBeProc = arguments[0];
  if (!(mustBeProc instanceof r5js.Lambda)) {
    throw new r5js.ArgumentTypeError(
        mustBeProc, 0, 'apply', r5js.parse.Terminals.LAMBDA);
  }

  var curProcCall = arguments[arguments.length - 3];
  /* todo bl: very little idea what's going on here, but we seem to
     use both sources of procName. */
  var procName = new r5js.ast.Identifier(
      curProcCall.firstOperand.getPayload() || mustBeProc.getName());
  var continuation = arguments[arguments.length - 2];
  var resultStruct = arguments[arguments.length - 1];

  var lastRealArgIndex = arguments.length - 4;
  var mustBeList = arguments[lastRealArgIndex];
  if (!(mustBeList instanceof r5js.List)) {
    throw new r5js.ArgumentTypeError(
        mustBeList, lastRealArgIndex, 'apply', r5js.parse.Terminals.LPAREN);
  }

  // (apply foo '(x y z))
  if (lastRealArgIndex === 1) {
    var newArgs = new r5js.SiblingBuffer();
    // todo bl document why we are quoting the arguments
    for (var arg = mustBeList.getFirstChild(); arg; arg = arg.getNextSibling())
      newArgs.appendSibling(new r5js.ast.Quote(arg));
    var actualProcCall = r5js.procs.newProcCall(
        procName, newArgs.toSiblings(), continuation);
    actualProcCall.setStartingEnv(curProcCall.env);
    resultStruct.nextContinuable = actualProcCall;
  } else {
    // (apply foo a b c '(1 2 3))
    for (var i = 1; i < lastRealArgIndex - 1; ++i) {
      arguments[i].setNextSibling(arguments[i + 1]);
    }
    arguments[lastRealArgIndex - 1].setNextSibling(mustBeList.getFirstChild());

    var newArgs = new r5js.SiblingBuffer().
        appendSibling(arguments[1]).
        toSiblings();
    var actualProcCall = r5js.procs.newProcCall(
        procName, newArgs, continuation);
    resultStruct.nextContinuable = actualProcCall;
  }

  return null;
});


/**
 * Semantics of dynamic-wind (as I understand it):
 * (dynamic-wind foo bar baz) means execute bar with the
 * following modifications:
 *
 * - Whenever I'm about to go into bar, do foo first
 * - Whenever I'm about to go out of bar, do baz first
 *
 * In simple cases, this is the same as (begin foo bar baz)
 * (except that the return value is that of bar, not baz).
 * The situation is complicated by continuations captured inside
 * a call/cc and later reentered; these must trigger the before
 * and after thunks. For example:
 *
 * (define cont #f)
 * (define (foo) (display 'foo))
 * (define (bar) (display 'bar))
 * (dynamic-wind
 * foo
 * (lambda ()
 * (call-with-current-continuation
 * (lambda (c)
 * (set! cont c))))
 * bar)
 * (cont 42)
 *
 * This will print "foo", "bar", "foo", "bar", and return
 * an unspecified value (because there's nothing in the body
 * of the lambda after the call/cc, for the call/cc to deliver the
 * 42 to).
 */
PrimitiveProcedures['dynamic-wind'] = _.ternaryWithSpecialEvalLogic(
    function(before, thunk, after, procCall, continuation, resultStruct) {
      // TODO bl: the compiler thinks there's already a variable named
      // "before" in scope here. Figure out why.
      var before2 = newCpsName();

      // None of the three thunks have any arguments.

      // todo bl use a ContinuableBuffer for efficiency

      var procCallBefore = r5js.procs.newProcCall(
          procCall.firstOperand,
          null, // no arguments
          new r5js.Continuation(before2));

      var procCallAfter = r5js.procs.newProcCall(
          procCall.firstOperand.getNextSibling().getNextSibling(),
          null, // no arguments
          new r5js.Continuation());

      var result = newCpsName();
      procCallAfter.appendContinuable(
          newIdShim(new r5js.ast.Identifier(result)));
      procCallAfter.getLastContinuable().continuation = continuation;

      var procCallThunk = r5js.procs.newProcCall(
          procCall.firstOperand.getNextSibling(),
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
      return null;
    });


/**
 * R5RS 6.4: (call-with-values producer consumer)
 * "Calls its producer argument with no values and a continuation that,
 * when passed some values, calls the consumer procedure with those values
 * as arguments. The continuation for the call to consumer is the continuation
 * of the call to call-with-values."
 */
PrimitiveProcedures['call-with-values'] = _.binaryWithSpecialEvalLogic(
    function(producer, consumer, procCall, continuation, resultStruct) {
      var valuesName = newCpsName();
      var producerContinuation = new r5js.Continuation(valuesName);
      var producerCall = r5js.procs.newProcCall(
          procCall.firstOperand,
          null, // no arguments
          producerContinuation);
      producerCall.setStartingEnv(
          /** @type {!r5js.IEnvironment} */ (procCall.env));
      var consumerCall = r5js.procs.newProcCall(
          procCall.firstOperand.getNextSibling(),
          new r5js.ast.Identifier(valuesName),
          continuation);
      consumerCall.setStartingEnv(
          /** @type {!r5js.IEnvironment} */ (procCall.env));
      producerContinuation.nextContinuable = consumerCall;
      resultStruct.nextContinuable = producerCall;
      return null;
    });


/**
 * Semantics of call/cc:
 *
 * (call-with-current-continuation foo)
 *
 * means create a new procedure call,
 *
 * (foo cc)
 *
 * where cc is the current continuation. Then inside the procedure body,
 * if we see
 *
 * (cc x)
 *
 * (that is, if the trampoline determines that the identifier is bound to a
 * Continuation object), this means bind x to cc's lastResultName and set
 * the next continuable to cc's nextContinuable.
 */
PrimitiveProcedures['call-with-current-continuation'] =
    _.unaryWithSpecialEvalLogic(function(
        procedure, procCall, continuation, resultStruct) {
      if (resultStruct.beforeThunk) {
        /* If this continuation is inside a call to dynamic-wind but
           escapes and then is later re-called, we have to remember
           to execute the associated before and after thunks. */
        continuation.installBeforeThunk(resultStruct.beforeThunk);
        resultStruct.beforeThunk = null;
      }
      var dummyProcCall = r5js.procs.newProcCall(
          procCall.firstOperand, continuation, continuation);
      dummyProcCall.setStartingEnv(
          /** @type {!r5js.IEnvironment} */ (procCall.env));
      resultStruct.nextContinuable = dummyProcCall;
      return null;
    });

PrimitiveProcedures['values'] = _.atLeastNWithSpecialEvalLogic(1, function() {
  // Varargs procedures that also have special eval logic are a pain.
  var resultStruct = arguments[arguments.length - 1];
  var continuation = arguments[arguments.length - 2];
  var procCall = arguments[arguments.length - 3];
  var numUserArgs = arguments.length - 3;

  /* If there's just one user-supplied argument, that works fine
     with the existing machinery. Example:

     (values 1 [_0 ...])

     should just bind 1 to _0 and continue. */
  if (numUserArgs === 1) {
    procCall.env.addBinding(continuation.lastResultName, arguments[0]);
  } else {
    /* If there's more than one argument, we bind the whole array
       to the continuation's lastResultName. This means later, when we're
       evaluating the arguments to a procedure call, we have to remember
       that a single name like _0 could specify a whole list of arguments. */

    var userArgs = [];

    for (var i = 0; i < numUserArgs; ++i) {
      userArgs.push(arguments[i]);
    }

    procCall.env.addBinding(continuation.lastResultName, userArgs);
  }
  if (continuation.nextContinuable) {
    continuation.nextContinuable.setStartingEnv(procCall.env);
  }
  resultStruct.nextContinuable = continuation.nextContinuable;
  return null;
});


// Environment-related procedures

PrimitiveProcedures['null-environment'] = _.unary(function(num) {
  if (num !== 5) {
    throw new r5js.InternalInterpreterError(
        'unsupported null environment ' + num);
  }
  return new r5js.ast.EnvironmentSpecifier(
      /** @type {!r5js.IEnvironment} */ (r5js.PrimitiveProcedures.nullEnv_));
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['scheme-report-environment'] = _.unary(function(num) {
  if (num !== 5) {
    throw new r5js.InternalInterpreterError(
        'unsupported scheme report environment ' + num);
  }
  return new r5js.ast.EnvironmentSpecifier(
      /** @type {!r5js.IEnvironment} */(r5js.PrimitiveProcedures.r5RSEnv_));
}, r5js.DatumType.NUMBER);


});  // goog.scope


/**
 * @param {!r5js.IEnvironment} nullEnv
 * @param {!r5js.IEnvironment} r5RSEnv
 */
r5js.PrimitiveProcedures.install = function(nullEnv, r5RSEnv) {
  r5js.PrimitiveProcedures.nullEnv_ = nullEnv;
  r5js.PrimitiveProcedures.r5RSEnv_ = r5RSEnv;
  for (var name in r5js.PrimitiveProcedures.registry_) {
    var proc = r5js.PrimitiveProcedures.registry_[name];
    proc.setDebugName(name);
    r5RSEnv.addBinding(name, proc);
  }
};

