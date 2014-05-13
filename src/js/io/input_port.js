goog.provide('r5js.InputPort');


goog.require('goog.functions');
goog.require('r5js.runtime.EOF');



/** @interface */
r5js.InputPort = function() {};


/** @return {boolean} */
r5js.InputPort.prototype.isCharReady = function() {};


/** @return {string|!r5js.runtime.Eof} */
r5js.InputPort.prototype.peekChar = function() {};


/** @return {!r5js.runtime.Value} */
r5js.InputPort.prototype.read = function() {};


/** @return {string|!r5js.runtime.Eof} */
r5js.InputPort.prototype.readChar = function() {};


/** @see R5RS 6.6.1 */
r5js.InputPort.prototype.close = function() {};


/** @const @private */ r5js.InputPort.IMPLEMENTED_BY_PROP_ = '$r5js.InputPort';


/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim. Remove.
 */
r5js.InputPort.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.InputPort.IMPLEMENTED_BY_PROP_]);
};


/** @param {function(new: r5js.InputPort, ...)} ctor */
r5js.InputPort.addImplementation = function(ctor) {
  ctor.prototype[r5js.InputPort.IMPLEMENTED_BY_PROP_] = true;
};



/**
 * An input port that has no available input.
 * @implements {r5js.InputPort}
 * @struct
 * @constructor
 * @private
 */
r5js.InputPort.Null_ = function() {};
r5js.InputPort.addImplementation(r5js.InputPort.Null_);


/** @override */
r5js.InputPort.Null_.prototype.isCharReady = goog.functions.FALSE;


/** @override */
r5js.InputPort.Null_.prototype.peekChar = goog.functions.constant(
    r5js.runtime.EOF);


/** @override */
r5js.InputPort.Null_.prototype.read = goog.functions.constant(
    r5js.runtime.EOF);


/** @override */
r5js.InputPort.Null_.prototype.readChar = goog.functions.constant(
    r5js.runtime.EOF);


/** @override */
r5js.InputPort.Null_.prototype.close = goog.nullFunction;


/** @const {!r5js.InputPort} */
r5js.InputPort.NULL = new r5js.InputPort.Null_();
