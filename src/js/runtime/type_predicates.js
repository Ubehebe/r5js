goog.module('r5js.runtime.TypePredicates');

const _ = goog.require('r5js.procspec');
const Boolean = goog.require('r5js.ast.Boolean');
const Character = goog.require('r5js.ast.Character');
const Continuation = goog.require('r5js.Continuation');
const Identifier = goog.require('r5js.ast.Identifier');
const InputPort = goog.require('r5js.InputPort');
const IPair = goog.require('r5js.IPair');
const Lambda = goog.require('r5js.ast.Lambda');
const List = goog.require('r5js.ast.List');
const Number = goog.require('r5js.ast.Number');
const OutputPort = goog.require('r5js.OutputPort');
const PrimitiveProcedure = goog.require('r5js.procspec').PrimitiveProcedure_;
const String = goog.require('r5js.ast.String');
const Vector = goog.require('r5js.ast.Vector');

/** @const {!Object<string, !PrimitiveProcedure>} */
const TypePredicates = {};

TypePredicates['boolean?'] = _.unary(node => node instanceof Boolean);

TypePredicates['char?'] = _.unary(node => node instanceof Character);

TypePredicates['input-port?'] = _.unary(port => InputPort.isImplementedBy(port));

TypePredicates['null?'] = _.unary(node => node instanceof List && !node.getFirstChild());

TypePredicates['number?'] = _.unary(node => node instanceof Number);

TypePredicates['output-port?'] = _.unary(port => OutputPort.isImplementedBy(port));

// 3.2: (pair? '()) => #f
TypePredicates['pair?'] = _.unary(node => IPair.isImplementedBy(node) && !!node.getFirstChild());

TypePredicates['port?'] = _.unary(port =>
InputPort.isImplementedBy(port) || OutputPort.isImplementedBy(port));

TypePredicates['procedure?'] = _.unary(node =>
    /* R5RS 6.4: "The procedure call-with-current-continuation
     packages up the current continuation as an "escape procedure"
     and passes it as an argument to proc." Thus a Continuation
     must count as a procedure. */
node instanceof Lambda || node instanceof Continuation);

TypePredicates['string?'] = _.unary(node => node instanceof String);

TypePredicates['symbol?'] = _.unary(node => node instanceof Identifier);

TypePredicates['vector?'] = _.unary(node => node instanceof Vector);

exports = TypePredicates;