goog.module('r5js.PrimitiveProcedures');

const _ = goog.require('r5js.procspec');
const Boolean = goog.require('r5js.ast.Boolean');
const CallWithCurrentContinuation = goog.require('r5js.CallWithCurrentContinuation');
const Character = goog.require('r5js.ast.Character');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Continuation = goog.require('r5js.Continuation');
const Datum = goog.require('r5js.Datum');
const DynamicWindContinuation = goog.require('r5js.DynamicWindContinuation');
const EOF = goog.require('r5js.runtime.EOF');
const Error = goog.require('r5js.Error');
const IEnvironment = goog.require('r5js.IEnvironment');
const InputPort = goog.require('r5js.InputPort');
const Identifier = goog.require('r5js.ast.Identifier');
const Lambda = goog.require('r5js.ast.Lambda');
const List = goog.require('r5js.ast.List');
const Number = goog.require('r5js.ast.Number');
const OutputPort = goog.require('r5js.OutputPort');
const ParserImpl = goog.require('r5js.ParserImpl');
const PortManager = goog.require('r5js.PortManager');
const ProcCall = goog.require('r5js.ProcCall');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const Quote = goog.require('r5js.ast.Quote');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const StringNode = goog.require('r5js.ast.String');
const Terminals = goog.require('r5js.parse.Terminals');
const trampoline = goog.require('r5js.trampoline');
const TrampolineHelper = goog.require('r5js.TrampolineHelper');
const Type = goog.require('r5js.Type');
const Types = Type.Types;
const UNSPECIFIED_VALUE = goog.require('r5js.runtime.UNSPECIFIED_VALUE');
const valutil = goog.require('r5js.valutil');
const Vector = goog.require('r5js.ast.Vector');

/** @type {IEnvironment} */ let nullEnv_ = null;
/** @type {IEnvironment} */ let r5RSEnv_ = null;
/** @type {PortManager} */ let portManager_ = null;

/** @const {!Object<string, !_.PrimitiveProcedure>} */ const PrimitiveProcedures = {};

// Type-related procedures
_.installPredicates(PrimitiveProcedures);

// Equivalence-related procedures

/* From the description of eq? at R5RS 6.1, it looks like it is
     permissible for eq? to have exactly the same semantics as eqv?. */
PrimitiveProcedures['eqv?']
    = PrimitiveProcedures['eq?']
    = _.binary((p, q) => p.eqv(q));

// Number-related procedures

PrimitiveProcedures['='] = _.varargsAtLeast0(function() {
  for (let i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] !== arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['/'] = _.varargsAtLeast1(function() {
  if (arguments.length === 1) { // unary
    return 1 / arguments[0];
  } else { // varargs: (x1 / x2) / x3 etc
    let ans = arguments[0];
    for (let i = 1; i < arguments.length; ++i)
      ans /= arguments[i];
    return ans;
  }
}, Types.NUMBER);

PrimitiveProcedures['-'] = _.varargsAtLeast1(function() {
  if (arguments.length === 1) { // unary
    return -1 * arguments[0];
  } else { // varargs: (x1 - x2) - x3 etc
    let ans = arguments[0];
    for (let i = 1; i < arguments.length; ++i) {
      ans -= arguments[i];
    }
    return ans;
  }
}, Types.NUMBER);

PrimitiveProcedures['*'] = _.varargsAtLeast0(function() {
  let product = 1;
  for (let i = 0; i < arguments.length; ++i) {
    product *= arguments[i];
  }
  return product;
}, Types.NUMBER);

PrimitiveProcedures['+'] = _.varargsAtLeast0(function() {
  let sum = 0;
  for (let i = 0; i < arguments.length; ++i) {
    sum += arguments[i];
  }
  return sum;
}, Types.NUMBER);

PrimitiveProcedures['>='] = _.varargsAtLeast0(function() {
  for (let i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] < arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['>'] = _.varargsAtLeast0(function() {
  for (let i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] <= arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['<='] = _.varargsAtLeast0(function() {
  for (let i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] > arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['<'] = _.varargsAtLeast0(function() {
  for (let i = 0; i < arguments.length - 1; ++i) {
    if (arguments[i] >= arguments[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['angle'] = _.unary(z => {
  throw Error.unimplementedOption('angle');
}, Types.NUMBER);

PrimitiveProcedures['acos'] = _.unary(Math.acos, Types.NUMBER);

PrimitiveProcedures['asin'] = _.unary(Math.asin, Types.NUMBER);

PrimitiveProcedures['atan'] = _.varargsAtLeast1(function() {
  /* Oddly, R5RS overloads atan for both one and two arguments,
             rather than having a separate atan2. */
  switch (arguments.length) {
    case 1:
      return Math.atan(arguments[0]);
    case 2:
      return Math.atan2(arguments[0], arguments[1]);
    default:
      throw Error.tooManyVarargs('atan', 2, arguments.length);
  }
}, Types.NUMBER);

PrimitiveProcedures['ceiling'] = _.unary(Math.ceil, Types.NUMBER);

PrimitiveProcedures['complex?'] = _.unary(node => node instanceof Number);

PrimitiveProcedures['cos'] = _.unary(Math.cos, Types.NUMBER);

// In JavaScript every number is a double.
PrimitiveProcedures['exact?'] = _.unary(x => false, Types.NUMBER);

// In JavaScript every number is inexact
PrimitiveProcedures['exact->inexact'] = _.unary(function(x) { return x; }, Types.NUMBER);

PrimitiveProcedures['exp'] = _.unary(Math.exp, Types.NUMBER);

PrimitiveProcedures['expt'] = _.binary(Math.pow, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['floor'] = _.unary(Math.floor, Types.NUMBER);

PrimitiveProcedures['imag-part'] = _.unary(function(z) {
  throw Error.unimplementedOption('imag-part');
}, Types.NUMBER);

PrimitiveProcedures['inexact?'] = _.unary(x => true, Types.NUMBER);

PrimitiveProcedures['inexact->exact'] = _.unary(function(x) { return x; } /* TODO bl */, Types.NUMBER);

PrimitiveProcedures['magnitude'] = _.unary(z => {
    throw Error.unimplementedOption('magnitude')
}, Types.NUMBER);

PrimitiveProcedures['make-polar'] = _.binary((x, y) => {
  throw Error.unimplementedOption('make-polar');
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['make-rectangular'] = _.binary((r, theta) => {
  throw Error.unimplementedOption('make-rectangular');
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['number->string'] = _.unary(x => new StringNode(x + ''), Types.NUMBER);

PrimitiveProcedures['integer?'] = _.unary(node =>
  node instanceof Number && Math.round(node.getPayload()) === node.getPayload());

PrimitiveProcedures['log'] = _.unary(Math.log, Types.NUMBER);

PrimitiveProcedures['modulo'] = _.binary((p, q) => {
  const remainder = p % q;
  const sign = p * q;
  let ans = remainder;
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
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['quotient'] = _.binary((p, q) => {
  /* In Scheme, quotient rounds towards zero, which is unfortunately
                 not what JavaScript's Math.round() does. */
  const unrounded = p / q;
  return unrounded > 0 ? Math.floor(unrounded) : Math.ceil(unrounded);
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['rational?'] = _.unary(node => node instanceof Number);

PrimitiveProcedures['real?'] = _.unary(node => node instanceof Number);

PrimitiveProcedures['real-part'] = _.unary(z => {
  throw Error.unimplementedOption('real-part');
}, Types.NUMBER);

// The JavaScript % semantics are precisely the Scheme remainder semantics.
PrimitiveProcedures['remainder'] = _.binary((p, q) => p % q, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['round'] = _.unary(x => {
  /* R5RS 6.2.5: "Round returns the closest integer to x,
             rounding to even when x is halfway between two integers." */
  const down = Math.floor(x);
  const downDiff = Math.abs(x - down);
  const up = Math.ceil(x);
  const upDiff = Math.abs(up - x);

  if (upDiff < downDiff) {
    return up;
  } else if (downDiff < upDiff) {
    return down;
  } else {
    return up % 2 ? down : up;
  }
}, Types.NUMBER);

PrimitiveProcedures['sin'] = _.unary(Math.sin, Types.NUMBER);

PrimitiveProcedures['sqrt'] = _.unary(Math.sqrt, Types.NUMBER);

PrimitiveProcedures['string->number'] = _.unary(parseFloat, Types.STRING);

PrimitiveProcedures['tan'] = _.unary(Math.tan, Types.NUMBER);

/* R5RS 6.2.5: "Truncate returns the integer closest to x
whose absolute value is not larger than the absolute value of x." */
PrimitiveProcedures['truncate'] = _.unary(x => x > 0 ? Math.floor(x) : Math.ceil(x), Types.NUMBER);

// Pair-related procedures

PrimitiveProcedures['car'] = _.unary(p => p.car(), Types.PAIR);

PrimitiveProcedures['cdr'] = _.unary(p => {
      const cdr = p.cdr();
      cdr.setCdrHelper(new List.CdrHelperImpl(p, cdr.getFirstChild()));
      return cdr;
    }, Types.PAIR);

PrimitiveProcedures['cons'] = _.binary((car, cdr) => {
  // todo bl this is really expensive! can we cut down on the copying?
  const realCar = car.clone();
  const realCdr = cdr.clone();
  // Since cdr already has a "head of list" node, reuse that. Convoluted eh?
  if (realCdr instanceof List || realCdr.isImproperList()) {
    const oldFirstChild = realCdr.getFirstChild();
    realCdr.setFirstChild(realCar);
    realCar.setNextSibling(oldFirstChild);
    return realCdr;
  } else {
    return new SiblingBuffer()
        .appendSibling(realCar)
        .appendSibling(realCdr)
        .toList(List.Dotted);
  }
});

PrimitiveProcedures['set-car!'] = _.binary((p, car) => {
  if (!(p instanceof List || p.isImproperList())) {
    throw Error.argumentTypeError(
        p, 0, 'set-car!', Types.PAIR, _.runtimeType(p));
  }
  if (p.isImmutable()) {
    throw Error.immutable(p.toString());
  }

  car.setNextSibling(p.getFirstChild().getNextSibling());
  p.setFirstChild(car);

  const helper = (/** @type {!CompoundDatum} */ (p)).getCdrHelper();
  if (helper) {
    helper.setCar(car);
  }

  return UNSPECIFIED_VALUE;
});

PrimitiveProcedures['set-cdr!'] = _.binary((p, cdr) => {
  if (!(p instanceof List || p.isImproperList())) {
    throw Error.argumentTypeError(
        p, 0, 'set-cdr!', Types.PAIR,
        _.runtimeType(p));
  }

  if (p.isImmutable()) {
    throw Error.immutable(p.toString());
  }

  if (cdr instanceof List) {
    p.getFirstChild().setNextSibling(cdr.getFirstChild());
  } else {
    p.getFirstChild().setNextSibling(cdr);
  }

  const helper = (/** @type {!CompoundDatum} */ (p)).getCdrHelper();
  if (helper) {
    helper.setCdr(cdr);
  }

  if (p instanceof List) {
    p.markDirty();
  }

  return UNSPECIFIED_VALUE;
});

// Vector-related procedures

PrimitiveProcedures['make-vector'] = _.varargsRange((numberNode, fillNode) => {
      if (!(numberNode instanceof Number)) {
        throw Error.argumentTypeError(
            numberNode, 0, 'make-vector', Types.NUMBER,
            _.runtimeType(numberNode));
      }
      const n = numberNode.getPayload();
      /* R5RS 6.3.6: "If a second argument is given, then each element
         is initialized to fill. Otherwise the initial contents of each element
         is unspecified." False seems like a good default. */
      fillNode = fillNode || new Boolean(false);
      const buf = [];
      for (let i = 0; i < n; ++i) {
        buf.push(fillNode.clone());
      }
      return new Vector(buf);
    }, 1, 2);

PrimitiveProcedures['vector-length'] = _.unary(v => v.vectorLength(), Types.VECTOR);

PrimitiveProcedures['vector-ref'] = _.binary((v, k) =>v.vectorRef(k), Types.VECTOR, Types.NUMBER);

PrimitiveProcedures['vector-set!'] = _.ternary((v, k, fill) => {
  if (!(v instanceof Vector)) {
    throw Error.argumentTypeError(
        v, 0, 'vector-set!', Types.VECTOR,
        _.runtimeType(v));
  }
  if (!(k instanceof Number)) {
    throw Error.argumentTypeError(
        k, 1, 'vector-set!', Types.NUMBER,
        _.runtimeType(k));
  }
  if (v.isImmutable()) {
    throw Error.immutable(v.toString());
  }
  v.vectorSet(k.getPayload(), fill);
  // todo bl requires a cycle-labeling procedure like set-car! and set-cdr!
  return UNSPECIFIED_VALUE;
});

// Symbol-related procedures

PrimitiveProcedures['symbol->string'] = _.unary(sym =>
    new StringNode(sym).setImmutable(), Types.SYMBOL);

  // TODO bl it doesn't seem right to be creating Identifiers instead of Symbols
PrimitiveProcedures['string->symbol'] = _.unary(node =>
    new Identifier(node.getPayload()), Types.STRING);

// Character-related procedures

PrimitiveProcedures['char=?'] = _.binary((node1, node2) =>
node1.getPayload() === node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char<?'] = _.binary((node1, node2) =>
    node1.getPayload() < node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char>?'] = _.binary((node1, node2) =>
  node1.getPayload() > node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char<=?'] = _.binary((node1, node2) =>
node1.getPayload() <= node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char>=?'] = _.binary((node1, node2) =>
  node1.getPayload() >= node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char->integer'] = _.unary(node => node.getPayload().charCodeAt(0),
    Types.CHARACTER);

PrimitiveProcedures['integer->char'] = _.unary(i => new Character(String.fromCharCode(i)),
    Types.NUMBER);

PrimitiveProcedures['char-upcase'] = _.unary((node) =>
    new Character(node.getPayload().toUpperCase()), Types.CHARACTER);

PrimitiveProcedures['char-downcase'] = _.unary((node) =>
    new Character(node.getPayload().toLowerCase()), Types.CHARACTER);

// String-related procedures

PrimitiveProcedures['make-string'] = _.varargsRange((numberNode, charNode) => {
      /* R5RS 6.3.5: "If char is given, then all elements of the
             string are initialized to char, otherwise the contents
             of the string are unspecified." */
      const c = goog.isDef(charNode) ? charNode.getPayload() : ' ';
      const n = numberNode.getPayload();
      let s = '';
      for (let i = 0; i < n; ++i) {
        s += c;
      }
      return new StringNode(s);
    }, 1, 2);

PrimitiveProcedures['string-length'] = _.unary(node => node.getPayload().length, Types.STRING);

PrimitiveProcedures['string-ref'] = _.binary((node, i) =>
    new Character(node.getPayload().charAt(i)), Types.STRING, Types.NUMBER);

PrimitiveProcedures['string-set!'] = _.ternary((str, k, c) => {
  if (str.isImmutable()) {
    throw Error.immutable(/** @type {string} */ (str.getPayload()));
  }
  const s = str.getPayload();
  str.setPayload(s.substr(0, k) + c.getPayload() + s.substr(k + 1));
  return UNSPECIFIED_VALUE;
}, Types.STRING, Types.NUMBER, Types.CHARACTER);

// Vector-related procedures

// Evaluation-related procedures

PrimitiveProcedures['eval'] = _.binaryWithCurrentPorts(
    /** @suppress {accessControls} */function(inputPort, outputPort, expr, envSpec) {
      if (!(expr instanceof Datum))
        // TODO bl how could this not be a datum? The type signature of binaryWithCurrentPorts
        // is not helpful. Also, Types.SYMBOL is not right.
        throw Error.argumentTypeError(
            expr, 0, 'eval', Types.SYMBOL, _.runtimeType(expr));
      if (!IEnvironment.isImplementedBy(envSpec)) {
        throw Error.argumentTypeError(
            envSpec, 1, 'eval', Types.ENVIRONMENT_SPECIFIER,
            _.runtimeType(envSpec));
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

      if (expr instanceof Lambda)
        return new Identifier(/** @type {string} */ (expr.getName()));

      else {
        /* Call the parse/desugar/eval portions of the interpreter pipeline
    manually. It would be nice to reuse the code in api.js, but it made
    for some awkward forward references. Reasoning about this copy/pasted
    code is simpler than reasoning about the build process. */

        const env = /** @type {!IEnvironment} */ (envSpec);
        // don't accidentally evaluate the next expr!
        expr.nextSibling_ = null;

        const parsed = new ParserImpl.ParserImpl(expr).parse();
        if (!parsed) {
          throw Error.parse(expr);
        }
        const continuable = /** @type {!ProcCallLike} */ (
            parsed.desugar(env));
        return trampoline(continuable, env, inputPort, outputPort);
      }
    });

/* This is not part of any Scheme standard, but it should be useful to
     test Scheme expressions that should not evaluate. */
PrimitiveProcedures['will-eval?'] = _.binary(
    /** @suppress {accessControls} */function(expr) {
      try {
        PrimitiveProcedures['eval'].fn_.call(null, expr);
        return true;
      } catch (e) {
        return false;
      }
    });

// I/O related procedures

PrimitiveProcedures['char-ready?'] = _.nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
        const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
        if (!InputPort.isImplementedBy(inputPortToUse)) {
            throw Error.argumentTypeError(
                inputPortToUse, 0, 'char-ready?', Types.INPUT_PORT,
                _.runtimeType(inputPortToUse));
        }
        return inputPortToUse.isCharReady();
    });

PrimitiveProcedures['close-input-port'] = _.unary(datum => {
  datum.close();
  return UNSPECIFIED_VALUE;
}, Types.INPUT_PORT);

PrimitiveProcedures['close-output-port'] = _.unary(datum => {
  datum.close();
  return UNSPECIFIED_VALUE;
}, Types.OUTPUT_PORT);

PrimitiveProcedures['current-input-port'] = _.nullaryWithCurrentPorts(
    function(inputPort, outputPort) { return inputPort; });

PrimitiveProcedures['current-output-port'] = _.nullaryWithCurrentPorts(
    function(inputPort, outputPort) { return outputPort; });

/* According to R5RS 6.6.3, display is supposed to be a library
     procedure. Since the only non-library output routine is write-char,
     display would presumably have to be written in terms of write-char.
     That's not too efficient, so I decided to write it in JavaScript. */
PrimitiveProcedures['display'] = _.unaryOrBinaryWithCurrentPorts(
    (inputPort, outputPort, datum, maybeUserSuppliedOutputPort) => {
      const outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!OutputPort.isImplementedBy(outputPortToUse)) {
        throw Error.argumentTypeError(
            outputPortToUse, 1, 'display', Types.OUTPUT_PORT,
            _.runtimeType(outputPortToUse));
      }
      (/** @type {!OutputPort} */ (outputPortToUse)).
          write(valutil.toDisplayString(datum));
      return UNSPECIFIED_VALUE;
    });

PrimitiveProcedures['eof-object?'] = _.unary(port => port === EOF);

PrimitiveProcedures['open-input-file'] = _.unary(datum =>
    portManager_.newInputPort(datum.getPayload()), Types.STRING);

PrimitiveProcedures['open-output-file'] = _.unary(datum =>
  portManager_.newOutputPort(datum.getPayload()), Types.STRING);

PrimitiveProcedures['peek-char'] = _.nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
      const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!InputPort.isImplementedBy(inputPortToUse)) {
        throw Error.argumentTypeError(
            inputPortToUse, 0, 'peek-char', Types.INPUT_PORT,
            _.runtimeType(inputPortToUse));
      }
      return inputPortToUse.peekChar() || EOF;
    });

PrimitiveProcedures['read'] = _.nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
      const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!InputPort.isImplementedBy(inputPortToUse)) {
        throw Error.argumentTypeError(
            inputPortToUse, 0, 'read', Types.INPUT_PORT,
            _.runtimeType(inputPortToUse));
      }
      return inputPortToUse.read() || EOF;
    });

PrimitiveProcedures['read-char'] = _.nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
      const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!InputPort.isImplementedBy(inputPortToUse)) {
        throw Error.argumentTypeError(
            inputPortToUse, 0, 'read-char', Types.INPUT_PORT,
            _.runtimeType(inputPortToUse));
      }
      return inputPortToUse.readChar() || EOF;
    });

PrimitiveProcedures['write'] = _.unaryOrBinaryWithCurrentPorts(
    (inputPort, outputPort, datum, maybeUserSuppliedOutputPort) => {
      const outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!OutputPort.isImplementedBy(outputPortToUse)) {
        throw Error.argumentTypeError(
            outputPortToUse, 1, 'write', Types.OUTPUT_PORT,
            _.runtimeType(outputPortToUse));
      }
      outputPortToUse.write(valutil.toWriteString(datum));
      return UNSPECIFIED_VALUE;
    });

PrimitiveProcedures['write-char'] = _.unaryOrBinaryWithCurrentPorts(
    (inputPort, outputPort, charNode, maybeUserSuppliedOutputPort) => {
      if (!(charNode instanceof Character)) {
        throw Error.argumentTypeError(
            charNode, 0, 'write-char', Types.CHARACTER,
            _.runtimeType(charNode));
      }
      const outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!OutputPort.isImplementedBy(outputPortToUse)) {
        throw Error.argumentTypeError(
            outputPortToUse, 1, 'write-char', Types.OUTPUT_PORT,
            _.runtimeType(outputPortToUse));
      }
      outputPortToUse.write(valutil.toWriteString(charNode));
      return UNSPECIFIED_VALUE;
    });

// Control flow related procedures


/**
 * R5RS 6.4: (apply proc arg1 ... args)
 * "Proc must be a procedure and args must be a list.
 * Calls proc with the elements of the list
 * (append (list arg1 ...) args) as the actual arguments.
 */
PrimitiveProcedures['apply'] = _.atLeastNWithSpecialEvalLogic(2, function() {
  const mustBeProc = arguments[0];
  if (!(mustBeProc instanceof Lambda)) {
    throw Error.argumentTypeError(
        mustBeProc, 0, 'apply', Types.PROCEDURE,
        _.runtimeType(mustBeProc));
  }

  const procName = new Identifier(mustBeProc.getName());
  const procCallLike = arguments[arguments.length - 2];
  const resultStruct = /** @type {!TrampolineHelper} */ (
      arguments[arguments.length - 1]);

  const lastRealArgIndex = arguments.length - 3;
  const mustBeList = arguments[lastRealArgIndex];
  if (!(mustBeList instanceof List)) {
    throw Error.argumentTypeError(
        mustBeList, lastRealArgIndex, 'apply', Types.PAIR,
        _.runtimeType(mustBeList));
  }

  // (apply foo '(x y z))
  if (lastRealArgIndex === 1) {
    const newArgs = new SiblingBuffer();
    // todo bl document why we are quoting the arguments
    for (let arg = mustBeList.getFirstChild(); arg; arg = arg.getNextSibling())
      newArgs.appendSibling(new Quote(arg));
    const actualProcCall = new ProcCall(
        procName, newArgs.toSiblings());
    actualProcCall.setNext(procCallLike.getNext());
    actualProcCall.setResultName(procCallLike.getResultName());
    resultStruct.setNext(actualProcCall);
  } else {
    // (apply foo a b c '(1 2 3))
    for (let i = 1; i < lastRealArgIndex - 1; ++i) {
      arguments[i].setNextSibling(arguments[i + 1]);
    }
    arguments[lastRealArgIndex - 1].setNextSibling(mustBeList.getFirstChild());

    const newArgs = new SiblingBuffer()
        .appendSibling(arguments[1])
        .toSiblings();
    const actualProcCall = new ProcCall(procName, newArgs);
    actualProcCall.setNext(procCallLike.getNext());
    actualProcCall.setResultName(procCallLike.getResultName());
    resultStruct.setNext(actualProcCall);
  }

  return UNSPECIFIED_VALUE;
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
    (before, thunk, after, procCallLike, resultStruct) => {

      const procCall = /** @type {!ProcCall} */ (procCallLike);
      // None of the three thunks have any arguments.

      // todo bl use a ContinuableBuffer for efficiency

      const procCallBefore = new ProcCall(procCall.getFirstOperand(), null /* no arguments */);

      const procCallAfter = new ProcCall(
          procCall.getFirstOperand().getNextSibling().getNextSibling(),
          null /* no arguments */);

      const procCallThunk = new ProcCall(
          procCall.getFirstOperand().getNextSibling(),
          null /* no arguments */);

      ProcCallLike.appendProcCallLike(
          procCallAfter,
          new Identifier(procCallThunk.getResultName()).toProcCallLike());
      ProcCallLike.getLast(procCallAfter).setNext(
          /** @type {!ProcCallLike} */ (procCallLike.getNext()));


      ProcCallLike.appendProcCallLike(
          procCallThunk, procCallAfter);
      ProcCallLike.appendProcCallLike(
          procCallBefore, procCallThunk);

      resultStruct.setNext(procCallBefore);
      /* We use the TrampolineResultStruct to store the thunk.
         This should be okay because dynamic-wind is the only one
         who writes to it, and call/cc is the only one who reads it.
         todo bl document why we cannot reuse procCallBefore. */
      resultStruct.setBeforeThunk(new ProcCall(
          procCall.getFirstOperand(),
          null /* no arguments */, procCallBefore.getResultName()));
      return UNSPECIFIED_VALUE;
    });


/**
 * R5RS 6.4: (call-with-values producer consumer)
 * "Calls its producer argument with no values and a continuation that,
 * when passed some values, calls the consumer procedure with those values
 * as arguments. The continuation for the call to consumer is the continuation
 * of the call to call-with-values."
 */
PrimitiveProcedures['call-with-values'] = _.binaryWithSpecialEvalLogic(
    (producer, consumer, procCallLike, resultStruct) => {
      const procCall = /** @type {!ProcCall} */ (procCallLike);
      const producerCall = new ProcCall(
          procCall.getFirstOperand(), null /* no arguments */);
      const consumerCall = new ProcCall(
          procCall.getFirstOperand().getNextSibling(),
          new Identifier(producerCall.getResultName()));
      consumerCall.setNext(/** @type {!ProcCallLike} */ (
          procCallLike.getNext()));
      producerCall.setNext(consumerCall);
      resultStruct.setNext(producerCall);
      return UNSPECIFIED_VALUE;
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
 *
 * TODO bl: type checking is turned off because of the continuation argument
 * to new ProcCall. Subclass and correct.
 */
PrimitiveProcedures['call-with-current-continuation'] =
    _.unaryWithSpecialEvalLogic(/** @suppress {checkTypes} */ function(
        procedure, procCallLike, resultStruct) {
      const procCall = /** @type {!ProcCall} */ (procCallLike);
      const next = procCallLike.getNext();
      const resultName = procCallLike.getResultName();
      const beforeThunk = resultStruct.getBeforeThunk();
      /* If this continuation is inside a call to dynamic-wind but
         escapes and then is later re-called, we have to remember
         to execute the associated before and after thunks. */
      const continuation = beforeThunk
          ? new DynamicWindContinuation(beforeThunk, next, resultName)
          : new Continuation(resultName, next);
      const dummyProcCall = new CallWithCurrentContinuation(
          procCall.getFirstOperand(), continuation);
      if (next) {
            dummyProcCall.setNext(next);
      }
      dummyProcCall.setResultName(procCallLike.getResultName());
      resultStruct.setNext(dummyProcCall);
      return UNSPECIFIED_VALUE;
    });


// TODO bl: This can be implemented as a macro. See R5RS p. 34.
PrimitiveProcedures['values'] = _.atLeastNWithSpecialEvalLogic(1, function() {
  // Varargs procedures that also have special eval logic are a pain.
  const resultStruct = arguments[arguments.length - 1];
  const procCallLike = arguments[arguments.length - 2];
  const procCall = arguments[arguments.length - 2];
  const numUserArgs = arguments.length - 2;

  /* If there's just one user-supplied argument, that works fine
     with the existing machinery. Example:

     (values 1 [_0 ...])

     should just bind 1 to _0 and continue. */
  if (numUserArgs === 1) {
    procCall.getEnv().addBinding(procCallLike.getResultName(), arguments[0]);
  } else {
    /* If there's more than one argument, we bind the whole array
       to the continuation's lastResultName. This means later, when we're
       evaluating the arguments to a procedure call, we have to remember
       that a single name like _0 could specify a whole list of arguments. */

    const userArgs = [];

    for (let i = 0; i < numUserArgs; ++i) {
      userArgs.push(arguments[i]);
    }

    procCall.getEnv().addBinding(procCallLike.getResultName(), userArgs);
  }
  const nextContinuable = procCallLike.getNext();
  if (nextContinuable) {
    nextContinuable.setStartingEnv(procCall.getEnv());
  }
  resultStruct.setNext(nextContinuable);
  return UNSPECIFIED_VALUE;
});


// Environment-related procedures

PrimitiveProcedures['null-environment'] = _.unary(num => {
  if (num !== 5) {
    throw Error.unimplementedOption(
        '(null-environment ' + num + ')');
  }
  return nullEnv_.child();
}, Types.NUMBER);

PrimitiveProcedures['scheme-report-environment'] = _.unary(num => {
  if (num !== 5) {
    throw Error.unimplementedOption(
        '(scheme-report-environment ' + num + ')');
  }
  return r5RSEnv_.child();
}, Types.NUMBER);

/**
 * @param {!IEnvironment} nullEnv
 * @param {!IEnvironment} r5RSEnv
 */
function install(nullEnv, r5RSEnv) {
  nullEnv_ = nullEnv;
  r5RSEnv_ = r5RSEnv;
  portManager_ = new PortManager();
  for (const name in PrimitiveProcedures) {
    const proc = PrimitiveProcedures[name];
    proc.setDebugName(name);
    r5RSEnv.addBinding(name, proc);
  }
}

exports.install = install;

