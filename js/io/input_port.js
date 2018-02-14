goog.module('r5js.InputPort');

const Character = goog.require('r5js.ast.Character');

/** @interface */
class InputPort {
 /** @return {boolean} */
 isCharReady() {}

 /** @return {?Character} The next character, or null if there are no more characters. */
 peekChar() {}

 /**
  * @return {?Value} The next value, or null if there are
  * no more values.
  */
 read() {}

 /** @return {?Character} The next character, or null if there are no more characters. */
 readChar() {}

 /** @see R5RS 6.6.1 */
 close() {}

 /**
  * @param {*} obj
  * @return {boolean}
  * @suppress {reportUnknownTypes}
  * TODO bl temporary shim. Remove.
  */
 static isImplementedBy(obj) {
  return !!(obj && obj[InputPort.IMPLEMENTED_BY_PROP_]);
 }

 /**
  * @param {function(new: InputPort, ...)} ctor
  * @suppress {reportUnknownTypes}
  */
 static addImplementation(ctor) {
  ctor.prototype[InputPort.IMPLEMENTED_BY_PROP_] = true;
 }
}

/** @const @private */ InputPort.IMPLEMENTED_BY_PROP_ = '$r5js.InputPort';


/**
 * An input port that has no available input.
 * @implements {InputPort}
 */
class NullInputPort {
 /** @override */
 isCharReady() {
  return false;
 }

 /** @override */
 peekChar() {
  return null;
 }

 /** @override */
 read() {
  return null;
 }

 /** @override */
 readChar() {
  return null;
 }

 /** @override */
 close() {}
}

InputPort.addImplementation(NullInputPort);

/** @const {!r5js.InputPort} */
InputPort.NULL = new NullInputPort();

exports = InputPort;