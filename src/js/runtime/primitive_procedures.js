goog.provide('r5js.runtime');


goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.PrimitiveProcedureError');
goog.require('r5js.TooManyArgs');
goog.require('r5js.UnimplementedOptionError');
goog.require('r5js.ast.InputPort');
goog.require('r5js.ast.OutputPort');
goog.require('r5js.runtime.unary');


/** @const @private {!Object.<string, !r5js.runtime.PrimitiveProcedure>} */
r5js.runtime.PrimitiveProcedures_ = {};


goog.scope(function() {
var _ = r5js.runtime;
var PrimitiveProcedures = r5js.runtime.PrimitiveProcedures_;

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

  if (p.isBoolean()) {
    return p.getPayload() === q.getPayload();
  } else if (p.isIdentifier()) {
    return p.getPayload() === q.getPayload();
  } else if (p.isNumber()) {
    return p.getPayload() === q.getPayload(); // todo bl numerical precision...
  } else if (p.isCharacter()) {
    return p.getPayload() === q.getPayload();
  } else if (p.isList()) {
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
  } else if (p.isVector()) {
    return (p.isArrayBacked() && q.isArrayBacked()) ?
        p.getPayload() === q.getPayload() :
        p === q;
  } else if (p.isString()) {
    return p === q;
  } else if (p.isProcedure()) {
    return p.getPayload() === q.getPayload();
  } else if (p.isQuasiquote()) {
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
  return node.isBoolean();
});

PrimitiveProcedures['char?'] = _.unary(function(node) {
  return node.isCharacter();
});

PrimitiveProcedures['input-port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.InputPort;
});

PrimitiveProcedures['null?'] = _.unary(function(node) {
  return node.isEmptyList();
});

PrimitiveProcedures['number?'] = _.unary(function(node) {
  return node.isNumber();
});

PrimitiveProcedures['output-port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.OutputPort;
});

PrimitiveProcedures['pair?'] = _.unary(function(node) {
  return (node.isList() || node.isImproperList() || node.isQuote()) &&
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
  return (node instanceof r5js.Datum && node.isProcedure()) ||
      node instanceof r5js.Continuation;
});

PrimitiveProcedures['string?'] = _.unary(function(node) {
  return node.isString();
});

PrimitiveProcedures['symbol?'] = _.unary(function(node) {
  return node.isIdentifier();
});

PrimitiveProcedures['vector?'] = _.unary(function(node) {
  return node.isVector();
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
  return node.isNumber();
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
  return r5js.data.newIdOrLiteral(x + '', r5js.DatumType.STRING);
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['integer?'] = _.unary(function(node) {
  return node.isNumber() &&
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
  return node.isNumber();
});

PrimitiveProcedures['real?'] = _.unary(function(node) {
  return node.isNumber();
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
    ans = (startOfCdr.getNextSibling() || p.isList()) ?
        startOfCdr.siblingsToList(p.isImproperList()) :
        startOfCdr;
    return ans.setCdrHelper(new r5js.CdrHelper(p, startOfCdr));
  } else return newEmptyList();
}, r5js.DatumType.PAIR);

PrimitiveProcedures['cons'] = _.binary(function(car, cdr) {
  // todo bl this is really expensive! can we cut down on the copying?
  var realCar = car.clone();
  var realCdr = cdr.clone();
  // Since cdr already has a "head of list" node, reuse that. Convoluted eh?
  if (realCdr.isList() || realCdr.isImproperList()) {
    realCdr.prependChild(realCar);
    return realCdr;
  } else {
    var ans = new r5js.Datum();
    ans.setType(r5js.DatumType.DOTTED_LIST);
    ans.appendChild(realCar);
    ans.appendChild(realCdr);
    // todo bl hmm the parent field isn't getting set...is that ok?
    return ans;
  }
});

PrimitiveProcedures['set-car!'] = _.binary(function(p, car) {
  if (!(p.isList() || p.isImproperList())) {
    throw new r5js.ArgumentTypeError(p, 0, 'set-car!', r5js.DatumType.LIST);
  }
  if (p.isImmutable()) {
    throw new r5js.ImmutableError(p.toString());
  }

  car.setNextSibling(p.getFirstChild().getNextSibling());
  p.setFirstChild(car);

  for (var helper = p.getCdrHelper();
      helper;
      helper = helper.getCdrHelper())
    helper.setCar(car);

  return null; // unspecified return value
});

PrimitiveProcedures['set-cdr!'] = _.binary(function(p, cdr) {
  if (!(p.isList() || p.isImproperList())) {
    throw new r5js.ArgumentTypeError(p, 0, 'set-cdr!', r5js.DatumType.LIST);
  }

  if (p.isImmutable()) {
    throw new r5js.ImmutableError(p.toString());
  }

  if (cdr.isList()) {
    p.getFirstChild().setNextSibling(cdr.getFirstChild());
    p.setType(r5js.parse.Terminals.LPAREN);
  } else {
    p.getFirstChild().setNextSibling(cdr);
    p.setType(r5js.parse.Terminals.LPAREN_DOT);
  }

  for (var helper = p.getCdrHelper();
      helper; helper = helper.getCdrHelper()) {
    helper.setCdr(cdr);
  }

  return null; // unspecified return value
});

// Symbol-related procedures

PrimitiveProcedures['symbol->string'] = _.unary(function(sym) {
  return r5js.data.newIdOrLiteral(sym, r5js.DatumType.STRING).setImmutable();
}, r5js.DatumType.SYMBOL);

PrimitiveProcedures['string->symbol'] = _.unary(function(node) {
  return r5js.data.newIdOrLiteral(node.getPayload(), r5js.DatumType.IDENTIFIER);
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
  return r5js.data.newIdOrLiteral(
      String.fromCharCode(i), r5js.DatumType.CHARACTER);
}, r5js.DatumType.NUMBER);

PrimitiveProcedures['char-upcase'] = _.unary(function(node) {
  return r5js.data.newIdOrLiteral(
      node.getPayload().toUpperCase(), r5js.DatumType.CHARACTER);
}, r5js.DatumType.CHARACTER);

PrimitiveProcedures['char-downcase'] = _.unary(function(node) {
  return r5js.data.newIdOrLiteral(
      node.getPayload().toLowerCase(), r5js.DatumType.CHARACTER);
}, r5js.DatumType.CHARACTER);

// String-related procedures

PrimitiveProcedures['string-length'] = _.unary(function(node) {
  return node.getPayload().length;
}, r5js.DatumType.STRING);

PrimitiveProcedures['string-ref'] = _.binary(function(node, i) {
  return r5js.data.newIdOrLiteral(
      node.getPayload().charAt(i), r5js.DatumType.CHARACTER);
}, r5js.DatumType.STRING, r5js.DatumType.NUMBER);

// Vector-related procedures

});  // goog.scope


/** @param {!r5js.IEnvironment} env */
r5js.runtime.install = function(env) {
  for (var name in r5js.runtime.PrimitiveProcedures_) {
    var proc = r5js.runtime.PrimitiveProcedures_[name];
    env.addBinding(name, goog.bind(proc.javascript, proc));
  }
};

