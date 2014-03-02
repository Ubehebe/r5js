goog.provide('r5js.runtime');


goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.PrimitiveProcedureError');
goog.require('r5js.ast.InputPort');
goog.require('r5js.ast.OutputPort');
goog.require('r5js.runtime.unary');


/** @const @private {!Object.<string, !r5js.runtime.PrimitiveProcedure>} */
r5js.runtime.PrimitiveProcedures_ = {};


goog.scope(function() {
var _ = r5js.runtime;

// Type-related procedures

_.PrimitiveProcedures_['boolean?'] = _.unary(function(node) {
  return node.isBoolean();
});

_.PrimitiveProcedures_['char?'] = _.unary(function(node) {
  return node.isCharacter();
});

_.PrimitiveProcedures_['input-port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.InputPort;
});

_.PrimitiveProcedures_['null?'] = _.unary(function(node) {
  return node.isEmptyList();
});

_.PrimitiveProcedures_['number?'] = _.unary(function(node) {
  return node.isNumber();
});

_.PrimitiveProcedures_['output-port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.OutputPort;
});

_.PrimitiveProcedures_['pair?'] = _.unary(function(node) {
  return (node.isList() || node.isImproperList() || node.isQuote()) &&
      !!node.getFirstChild(); // 3.2: (pair? '()) => #f
});

_.PrimitiveProcedures_['port?'] = _.unary(function(node) {
  return node instanceof r5js.ast.InputPort ||
      node instanceof r5js.ast.OutputPort;
});

_.PrimitiveProcedures_['procedure?'] = _.unary(function(node) {
  /* R5RS 6.4: "The procedure call-with-current-continuation
         packages up the current continuation as an "escape procedure"
         and passes it as an argument to proc." Thus a Continuation
         must count as a procedure. */
  return (node instanceof r5js.Datum && node.isProcedure()) ||
      node instanceof r5js.Continuation;
});

_.PrimitiveProcedures_['string?'] = _.unary(function(node) {
  return node.isString();
});

_.PrimitiveProcedures_['symbol?'] = _.unary(
    function(node) {
      return node.isIdentifier();
    });

_.PrimitiveProcedures_['vector?'] = _.unary(function(node) {
  return node.isVector();
});

// Number-related procedures

_.PrimitiveProcedures_['complex?'] = _.unary(function(node) {
  return node.isNumber();
});

_.PrimitiveProcedures_['real?'] = _.unary(function(node) {
  return node.isNumber();
});

_.PrimitiveProcedures_['rational?'] = _.unary(function(node) {
  return node.isNumber();
});

_.PrimitiveProcedures_['integer?'] = _.unary(function(node) {
  return node.isNumber() &&
      Math.round(node.getPayload()) === node.getPayload();
});

_.PrimitiveProcedures_['exact?'] = _.unary(function(x) {
  return false; // In JavaScript every number is a double.
}, r5js.DatumType.NUMBER);

_.PrimitiveProcedures_['inexact?'] = _.unary(function(x) {
  return true;
}, r5js.DatumType.NUMBER);

_.PrimitiveProcedures_['remainder'] = _.binary(function(p, q) {
  if (q === 0) {
    throw new r5js.PrimitiveProcedureError('remainder: undefined for 0');
  }
  // The JavaScript % semantics are precisely the Scheme remainder semantics.
  return p % q;
}, r5js.DatumType.NUMBER, r5js.DatumType.NUMBER);


});  // goog.scope


/** @param {!r5js.IEnvironment} env */
r5js.runtime.install = function(env) {
  for (var name in r5js.runtime.PrimitiveProcedures_) {
    var proc = r5js.runtime.PrimitiveProcedures_[name];
    env.addBinding(name, goog.bind(proc.javascript, proc));
  }
};

