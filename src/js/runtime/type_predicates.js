goog.module('r5js.runtime.Type');

const _ = goog.require('r5js.procspec');
const Boolean = goog.require('r5js.ast.Boolean');
const Character = goog.require('r5js.ast.Character');
const Continuation = goog.require('r5js.Continuation');
const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const Identifier = goog.require('r5js.ast.Identifier');
const InputPort = goog.require('r5js.InputPort');
const IPair = goog.require('r5js.IPair');
const Lambda = goog.require('r5js.ast.Lambda');
const List = goog.require('r5js.ast.List');
const Number = goog.require('r5js.ast.Number');
const OutputPort = goog.require('r5js.OutputPort');
const PrimitiveProcedure = goog.require('r5js.procspec').PrimitiveProcedure_;
const String = goog.require('r5js.ast.String');
const Type = goog.require('r5js.Type');
const Vector = goog.require('r5js.ast.Vector');

/** @const {!Object<string, !PrimitiveProcedure>} */
const Predicates = {};

Predicates['boolean?'] = _.unary(node => node instanceof Boolean);

Predicates['char?'] = _.unary(node => node instanceof Character);

Predicates['input-port?'] = _.unary(port => InputPort.isImplementedBy(port));

Predicates['null?'] = _.unary(node => node instanceof List && !node.getFirstChild());

Predicates['number?'] = _.unary(node => node instanceof Number);

Predicates['output-port?'] = _.unary(port => OutputPort.isImplementedBy(port));

// 3.2: (pair? '()) => #f
Predicates['pair?'] = _.unary(node => IPair.isImplementedBy(node) && !!node.getFirstChild());

Predicates['port?'] = _.unary(port =>
InputPort.isImplementedBy(port) || OutputPort.isImplementedBy(port));

Predicates['procedure?'] = _.unary(node =>
    /* R5RS 6.4: "The procedure call-with-current-continuation
     packages up the current continuation as an "escape procedure"
     and passes it as an argument to proc." Thus a Continuation
     must count as a procedure. */
node instanceof Lambda || node instanceof Continuation);

Predicates['string?'] = _.unary(node => node instanceof String);

Predicates['symbol?'] = _.unary(node => node instanceof Identifier);

Predicates['vector?'] = _.unary(node => node instanceof Vector);

/**
 * @param {!Datum} arg
 * @return {!Type}
 * @suppress {accessControls}
 */
function of(arg) {
    for (const key in Type.Types) {
        const type = Type.Types[key];
        const predicateName = type.getName() + '?';
        if (predicateName in Predicates
            && !!Predicates[predicateName].fn_.call(null, arg)) {
            return type;
        }
    }
    throw Error.internalInterpreterError("unknown type: " + arg);
}

exports.Predicates = Predicates;
exports.of = of;