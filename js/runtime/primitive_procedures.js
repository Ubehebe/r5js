goog.module('r5js.PrimitiveProcedures');

const {Boolean} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/boolean');
const CallWithCurrentContinuation = goog.require('r5js.CallWithCurrentContinuation');
const {Character} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/character');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Continuation = goog.require('r5js.Continuation');
const {Datum, ProcCallLike, appendProcCallLike, getLastProcCallLike} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const DynamicWindContinuation = goog.require('r5js.DynamicWindContinuation');
const EOF = goog.require('r5js.runtime.EOF');
const Environment = goog.require('r5js.Environment');
const InputPort = goog.require('r5js.InputPort');
const Lambda = goog.require('r5js.Lambda');
const Number = goog.require('r5js.ast.Number');
const ParserImpl = goog.require('r5js.ParserImpl');
const PortManager = goog.require('r5js.PortManager');
const ProcCall = goog.require('r5js.ProcCall');
const Quote = goog.require('r5js.ast.Quote');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const StringNode = goog.require('r5js.ast.String');
const TrampolineHelper = goog.require('r5js.TrampolineHelper');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const Vector = goog.require('r5js.ast.Vector');
const trampoline = goog.require('r5js.trampoline');
const valutil = goog.require('r5js.valutil');
const {CdrHelperImpl, DottedList, List} = goog.require('r5js.ast.List');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {OutputPort, isOutputPortImpl} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');
const {Types} = require('/js/ast/type_collect_es6_sources.es6/node_modules/__main__/js/ast/type');
const {argumentTypeError} = goog.require('r5js.runtime.errors');
const {
    atLeastNWithSpecialEvalLogic,
    binary,
    binaryWithCurrentPorts,
    binaryWithSpecialEvalLogic,
    installPredicates,
    nullaryOrUnaryWithCurrentPorts,
    nullaryWithCurrentPorts,
    PrimitiveProcedure,
    runtimeType,
    ternary,
    ternaryWithSpecialEvalLogic,
    unary,
    unaryOrBinaryWithCurrentPorts,
    unaryWithSpecialEvalLogic,
    varargsAtLeast0,
    varargsAtLeast1,
    varargsRange} = goog.require('r5js.procspec');

/** @type {?IEnvironment} */ let nullEnv_ = null;
/** @type {?IEnvironment} */ let r5RSEnv_ = null;
/** @type {?PortManager} */ let portManager_ = null;

/** @const {!Object<string, !PrimitiveProcedure>} */ const PrimitiveProcedures = {};

// Type-related procedures
installPredicates(PrimitiveProcedures);

// Equivalence-related procedures

/* From the description of eq? at R5RS 6.1, it looks like it is
     permissible for eq? to have exactly the same semantics as eqv?. */
PrimitiveProcedures['eqv?'] = PrimitiveProcedures['eq?'] = binary((p, q) => p.eqv(q));

// Number-related procedures

PrimitiveProcedures['='] = varargsAtLeast0((...args) => {
  for (let i = 0; i < args.length - 1; ++i) {
    if (args[i] !== args[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['/'] = varargsAtLeast1((...args) => {
  if (args.length === 1) { // unary
    return 1 / args[0];
  } else { // varargs: (x1 / x2) / x3 etc
    let ans = args[0];
    for (let i = 1; i < args.length; ++i)
      ans /= args[i];
    return ans;
  }
}, Types.NUMBER);

PrimitiveProcedures['-'] = varargsAtLeast1((...args) => {
  if (args.length === 1) { // unary
    return -1 * args[0];
  } else { // varargs: (x1 - x2) - x3 etc
    let ans = args[0];
    for (let i = 1; i < args.length; ++i) {
      ans -= args[i];
    }
    return ans;
  }
}, Types.NUMBER);

PrimitiveProcedures['*'] = varargsAtLeast0((...args) => {
  let product = 1;
  for (let i = 0; i < args.length; ++i) {
    product *= args[i];
  }
  return product;
}, Types.NUMBER);

PrimitiveProcedures['+'] = varargsAtLeast0((...args) => {
  let sum = 0;
  for (let i = 0; i < args.length; ++i) {
    sum += args[i];
  }
  return sum;
}, Types.NUMBER);

PrimitiveProcedures['>='] = varargsAtLeast0((...args) => {
  for (let i = 0; i < args.length - 1; ++i) {
    if (args[i] < args[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['>'] = varargsAtLeast0((...args) => {
  for (let i = 0; i < args.length - 1; ++i) {
    if (args[i] <= args[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['<='] = varargsAtLeast0((...args) => {
  for (let i = 0; i < args.length - 1; ++i) {
    if (args[i] > args[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['<'] = varargsAtLeast0((...args) => {
  for (let i = 0; i < args.length - 1; ++i) {
    if (args[i] >= args[i + 1]) {
      return false;
    }
  }
  return true;
}, Types.NUMBER);

PrimitiveProcedures['angle'] = unary(z => {
  throw Error.unimplementedOption('angle');
}, Types.NUMBER);

PrimitiveProcedures['acos'] = unary(Math.acos, Types.NUMBER);

PrimitiveProcedures['asin'] = unary(Math.asin, Types.NUMBER);

PrimitiveProcedures['atan'] = varargsAtLeast1((...args) => {
  /* Oddly, R5RS overloads atan for both one and two arguments,
             rather than having a separate atan2. */
  switch (args.length) {
    case 1:
      return Math.atan(args[0]);
    case 2:
      return Math.atan2(args[0], args[1]);
    default:
      throw Error.tooManyVarargs('atan', 2, args.length);
  }
}, Types.NUMBER);

PrimitiveProcedures['ceiling'] = unary(Math.ceil, Types.NUMBER);

PrimitiveProcedures['complex?'] = unary(node => node instanceof Number);

PrimitiveProcedures['cos'] = unary(Math.cos, Types.NUMBER);

// In JavaScript every number is a double.
PrimitiveProcedures['exact?'] = unary(x => false, Types.NUMBER);

// In JavaScript every number is inexact
PrimitiveProcedures['exact->inexact'] = unary(x => x, Types.NUMBER);

PrimitiveProcedures['exp'] = unary(Math.exp, Types.NUMBER);

PrimitiveProcedures['expt'] = binary(Math.pow, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['floor'] = unary(Math.floor, Types.NUMBER);

PrimitiveProcedures['imag-part'] = unary((z) => {
  throw Error.unimplementedOption('imag-part');
}, Types.NUMBER);

PrimitiveProcedures['inexact?'] = unary(x => true, Types.NUMBER);

PrimitiveProcedures['inexact->exact'] = unary(x => x /* TODO bl */, Types.NUMBER);

PrimitiveProcedures['magnitude'] = unary(z => {
    throw Error.unimplementedOption('magnitude');
}, Types.NUMBER);

PrimitiveProcedures['make-polar'] = binary((x, y) => {
  throw Error.unimplementedOption('make-polar');
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['make-rectangular'] = binary((r, theta) => {
  throw Error.unimplementedOption('make-rectangular');
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['number->string'] = unary(x => new StringNode(x + ''), Types.NUMBER);

PrimitiveProcedures['integer?'] = unary(node =>
  node instanceof Number && Math.round(node.getPayload()) === node.getPayload());

PrimitiveProcedures['log'] = unary(Math.log, Types.NUMBER);

PrimitiveProcedures['modulo'] = binary((p, q) => {
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

PrimitiveProcedures['quotient'] = binary((p, q) => {
  /* In Scheme, quotient rounds towards zero, which is unfortunately
                 not what JavaScript's Math.round() does. */
  const unrounded = p / q;
  return unrounded > 0 ? Math.floor(unrounded) : Math.ceil(unrounded);
}, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['rational?'] = unary(node => node instanceof Number);

PrimitiveProcedures['real?'] = unary(node => node instanceof Number);

PrimitiveProcedures['real-part'] = unary(z => {
  throw Error.unimplementedOption('real-part');
}, Types.NUMBER);

// The JavaScript % semantics are precisely the Scheme remainder semantics.
PrimitiveProcedures['remainder'] = binary((p, q) => p % q, Types.NUMBER, Types.NUMBER);

PrimitiveProcedures['round'] = unary(x => {
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

PrimitiveProcedures['sin'] = unary(Math.sin, Types.NUMBER);

PrimitiveProcedures['sqrt'] = unary(Math.sqrt, Types.NUMBER);

PrimitiveProcedures['string->number'] = unary(parseFloat, Types.STRING);

PrimitiveProcedures['tan'] = unary(Math.tan, Types.NUMBER);

/* R5RS 6.2.5: "Truncate returns the integer closest to x
whose absolute value is not larger than the absolute value of x." */
PrimitiveProcedures['truncate'] = unary(x => x > 0 ? Math.floor(x) : Math.ceil(x), Types.NUMBER);

// Pair-related procedures

PrimitiveProcedures['car'] = unary(p => p.car(), Types.PAIR);

PrimitiveProcedures['cdr'] = unary(p => {
    const cdr = p.cdr();
    // TODO bl bug city. I have no idea what this code path does, and it only seems to be triggered
    // intermittently, when I'm working on unrelated stuff.
    if (cdr instanceof CompoundDatum) {
        cdr.setCdrHelper(new CdrHelperImpl(p, /** @type {!Datum} */ (cdr.getFirstChild())));
    }
    return cdr;
}, Types.PAIR);

PrimitiveProcedures['cons'] = binary((car, cdr) => {
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
        .toList(DottedList);
  }
});

PrimitiveProcedures['set-car!'] = binary((p, car) => {
  if (!(p instanceof List || p.isImproperList())) {
    throw argumentTypeError(
        p, 0, 'set-car!', Types.PAIR, runtimeType(p));
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

PrimitiveProcedures['set-cdr!'] = binary((p, cdr) => {
  if (!(p instanceof List || p.isImproperList())) {
    throw argumentTypeError(
        p, 0, 'set-cdr!', Types.PAIR,
        runtimeType(p));
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

PrimitiveProcedures['make-vector'] = varargsRange((numberNode, fillNode) => {
      if (!(numberNode instanceof Number)) {
        throw argumentTypeError(
            numberNode, 0, 'make-vector', Types.NUMBER,
            runtimeType(numberNode));
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

PrimitiveProcedures['vector-length'] = unary(v => v.vectorLength(), Types.VECTOR);

PrimitiveProcedures['vector-ref'] = binary((v, k) =>v.vectorRef(k), Types.VECTOR, Types.NUMBER);

PrimitiveProcedures['vector-set!'] = ternary((v, k, fill) => {
  if (!(v instanceof Vector)) {
    throw argumentTypeError(
        v, 0, 'vector-set!', Types.VECTOR,
        runtimeType(v));
  }
  if (!(k instanceof Number)) {
    throw argumentTypeError(
        k, 1, 'vector-set!', Types.NUMBER,
        runtimeType(k));
  }
  if (v.isImmutable()) {
    throw Error.immutable(v.toString());
  }
  v.vectorSet(k.getPayload(), fill);
  // todo bl requires a cycle-labeling procedure like set-car! and set-cdr!
  return UNSPECIFIED_VALUE;
});

// Symbol-related procedures

PrimitiveProcedures['symbol->string'] = unary(sym =>
    new StringNode(sym).setImmutable(), Types.SYMBOL);

  // TODO bl it doesn't seem right to be creating Identifiers instead of Symbols
PrimitiveProcedures['string->symbol'] = unary(node =>
    new Identifier(node.getPayload()), Types.STRING);

// Character-related procedures

PrimitiveProcedures['char=?'] = binary((node1, node2) =>
node1.getPayload() === node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char<?'] = binary((node1, node2) =>
    node1.getPayload() < node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char>?'] = binary((node1, node2) =>
  node1.getPayload() > node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char<=?'] = binary((node1, node2) =>
node1.getPayload() <= node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char>=?'] = binary((node1, node2) =>
  node1.getPayload() >= node2.getPayload(), Types.CHARACTER, Types.CHARACTER);

PrimitiveProcedures['char->integer'] = unary(node => node.getPayload().charCodeAt(0),
    Types.CHARACTER);

PrimitiveProcedures['integer->char'] = unary(i => new Character(String.fromCharCode(i)),
    Types.NUMBER);

PrimitiveProcedures['char-upcase'] = unary((node) =>
    new Character(node.getPayload().toUpperCase()), Types.CHARACTER);

PrimitiveProcedures['char-downcase'] = unary((node) =>
    new Character(node.getPayload().toLowerCase()), Types.CHARACTER);

// String-related procedures

PrimitiveProcedures['make-string'] = varargsRange((numberNode, charNode) => {
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

PrimitiveProcedures['string-length'] = unary(node => node.getPayload().length, Types.STRING);

PrimitiveProcedures['string-ref'] = binary((node, i) =>
    new Character(node.getPayload().charAt(i)), Types.STRING, Types.NUMBER);

PrimitiveProcedures['string-set!'] = ternary((str, k, c) => {
  if (str.isImmutable()) {
    throw Error.immutable(/** @type {string} */ (str.getPayload()));
  }
  const s = str.getPayload();
  str.setPayload(s.substr(0, k) + c.getPayload() + s.substr(k + 1));
  return UNSPECIFIED_VALUE;
}, Types.STRING, Types.NUMBER, Types.CHARACTER);

// Evaluation-related procedures

PrimitiveProcedures['eval'] = binaryWithCurrentPorts(
    function(inputPort, outputPort, expr, envSpec) {
      if (!(expr instanceof Datum))
        // TODO bl how could this not be a datum? The type signature of binaryWithCurrentPorts
        // is not helpful. Also, Types.SYMBOL is not right.
        throw argumentTypeError(
            expr, 0, 'eval', Types.SYMBOL, runtimeType(expr));
      if (!(envSpec instanceof Environment)) {
        throw argumentTypeError(
            envSpec, 1, 'eval', Types.ENVIRONMENT_SPECIFIER,
            runtimeType(envSpec));
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
        expr.setNextSibling(null);

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
PrimitiveProcedures['will-eval?'] = binary(
    /** @suppress {accessControls} */function(expr) {
      try {
        PrimitiveProcedures['eval'].fn_.call(null, expr);
        return true;
      } catch (e) {
        return false;
      }
    });

// I/O related procedures

PrimitiveProcedures['char-ready?'] = nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
        const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
        if (!InputPort.isImplementedBy(inputPortToUse)) {
            throw argumentTypeError(
                inputPortToUse, 0, 'char-ready?', Types.INPUT_PORT,
                runtimeType(inputPortToUse));
        }
        return inputPortToUse.isCharReady();
    });

PrimitiveProcedures['close-input-port'] = unary(datum => {
  datum.close();
  return UNSPECIFIED_VALUE;
}, Types.INPUT_PORT);

PrimitiveProcedures['close-output-port'] = unary(datum => {
  datum.close();
  return UNSPECIFIED_VALUE;
}, Types.OUTPUT_PORT);

PrimitiveProcedures['current-input-port'] =
  nullaryWithCurrentPorts((inputPort, outputPort) => inputPort);

PrimitiveProcedures['current-output-port'] =
  nullaryWithCurrentPorts((inputPort, outputPort) => outputPort);

/* According to R5RS 6.6.3, display is supposed to be a library
     procedure. Since the only non-library output routine is write-char,
     display would presumably have to be written in terms of write-char.
     That's not too efficient, so I decided to write it in JavaScript. */
PrimitiveProcedures['display'] = unaryOrBinaryWithCurrentPorts(
    (inputPort, outputPort, datum, maybeUserSuppliedOutputPort) => {
      const outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!isOutputPortImpl(outputPortToUse)) {
        throw argumentTypeError(
            outputPortToUse, 1, 'display', Types.OUTPUT_PORT,
            runtimeType(outputPortToUse));
      }
      (/** @type {!OutputPort} */ (outputPortToUse)).
          write(valutil.toDisplayString(datum));
      return UNSPECIFIED_VALUE;
    });

PrimitiveProcedures['eof-object?'] = unary(port => port === EOF);

PrimitiveProcedures['open-input-file'] = unary(datum =>
    portManager_.newInputPort(datum.getPayload()), Types.STRING);

PrimitiveProcedures['open-output-file'] = unary(datum =>
  portManager_.newOutputPort(datum.getPayload()), Types.STRING);

PrimitiveProcedures['peek-char'] = nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
      const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!InputPort.isImplementedBy(inputPortToUse)) {
        throw argumentTypeError(
            inputPortToUse, 0, 'peek-char', Types.INPUT_PORT,
            runtimeType(inputPortToUse));
      }
      return inputPortToUse.peekChar() || EOF;
    });

PrimitiveProcedures['read'] = nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
      const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!InputPort.isImplementedBy(inputPortToUse)) {
        throw argumentTypeError(
            inputPortToUse, 0, 'read', Types.INPUT_PORT,
            runtimeType(inputPortToUse));
      }
      return inputPortToUse.read() || EOF;
    });

PrimitiveProcedures['read-char'] = nullaryOrUnaryWithCurrentPorts(
    (inputPort, outputPort, maybeUserSuppliedInputPort) => {
      const inputPortToUse = maybeUserSuppliedInputPort || inputPort;
      if (!InputPort.isImplementedBy(inputPortToUse)) {
        throw argumentTypeError(
            inputPortToUse, 0, 'read-char', Types.INPUT_PORT,
            runtimeType(inputPortToUse));
      }
      return inputPortToUse.readChar() || EOF;
    });

PrimitiveProcedures['write'] = unaryOrBinaryWithCurrentPorts(
    (inputPort, outputPort, datum, maybeUserSuppliedOutputPort) => {
      const outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!isOutputPortImpl(outputPortToUse)) {
        throw argumentTypeError(
            outputPortToUse, 1, 'write', Types.OUTPUT_PORT,
            runtimeType(outputPortToUse));
      }
      outputPortToUse.write(valutil.toWriteString(datum));
      return UNSPECIFIED_VALUE;
    });

PrimitiveProcedures['write-char'] = unaryOrBinaryWithCurrentPorts(
    (inputPort, outputPort, charNode, maybeUserSuppliedOutputPort) => {
      if (!(charNode instanceof Character)) {
        throw argumentTypeError(
            charNode, 0, 'write-char', Types.CHARACTER,
            runtimeType(charNode));
      }
      const outputPortToUse = maybeUserSuppliedOutputPort || outputPort;
      if (!isOutputPortImpl(outputPortToUse)) {
        throw argumentTypeError(
            outputPortToUse, 1, 'write-char', Types.OUTPUT_PORT,
            runtimeType(outputPortToUse));
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
PrimitiveProcedures['apply'] = atLeastNWithSpecialEvalLogic(2, (...args) => {
  const mustBeProc = args[0];
  if (!(mustBeProc instanceof Lambda)) {
    throw argumentTypeError(
        mustBeProc, 0, 'apply', Types.PROCEDURE,
        runtimeType(mustBeProc));
  }

  const procName = new Identifier(mustBeProc.getName());
  const procCallLike = args[args.length - 2];
  const resultStruct = /** @type {!TrampolineHelper} */ (
      args[args.length - 1]);

  const lastRealArgIndex = args.length - 3;
  const mustBeList = args[lastRealArgIndex];
  if (!(mustBeList instanceof List)) {
    throw argumentTypeError(
        mustBeList, lastRealArgIndex, 'apply', Types.PAIR,
        runtimeType(mustBeList));
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
      args[i].setNextSibling(args[i + 1]);
    }
    args[lastRealArgIndex - 1].setNextSibling(mustBeList.getFirstChild());

    const newArgs = new SiblingBuffer()
        .appendSibling(args[1])
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
PrimitiveProcedures['dynamic-wind'] = ternaryWithSpecialEvalLogic(
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

      appendProcCallLike(
          procCallAfter,
          new Identifier(procCallThunk.getResultName()).toProcCallLike());
      getLastProcCallLike(procCallAfter).setNext(
          /** @type {!ProcCallLike} */ (procCallLike.getNext()));


      appendProcCallLike(procCallThunk, procCallAfter);
      appendProcCallLike(procCallBefore, procCallThunk);

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
PrimitiveProcedures['call-with-values'] = binaryWithSpecialEvalLogic(
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
    unaryWithSpecialEvalLogic(/** @suppress {checkTypes} */ function(
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
PrimitiveProcedures['values'] = atLeastNWithSpecialEvalLogic(1, (...args) => {
  // Varargs procedures that also have special eval logic are a pain.
  const resultStruct = args[args.length - 1];
  const procCallLike = args[args.length - 2];
  const procCall = args[args.length - 2];
  const numUserArgs = args.length - 2;

  /* If there's just one user-supplied argument, that works fine
     with the existing machinery. Example:

     (values 1 [_0 ...])

     should just bind 1 to _0 and continue. */
  if (numUserArgs === 1) {
    procCall.getEnv().addBinding(procCallLike.getResultName(), args[0]);
  } else {
    /* If there's more than one argument, we bind the whole array
       to the continuation's lastResultName. This means later, when we're
       evaluating the arguments to a procedure call, we have to remember
       that a single name like _0 could specify a whole list of arguments. */

    const userArgs = [];

    for (let i = 0; i < numUserArgs; ++i) {
      userArgs.push(args[i]);
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

PrimitiveProcedures['null-environment'] = unary(num => {
  if (num !== 5) {
    throw Error.unimplementedOption(
        '(null-environment ' + num + ')');
  }
  return nullEnv_.child();
}, Types.NUMBER);

PrimitiveProcedures['scheme-report-environment'] = unary(num => {
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

exports = {install};