goog.provide('r5js.runtime.unary');


goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.data');



/** @interface */
r5js.runtime.PrimitiveProcedure = function() {};


/**
 * @type {!Function}
 * TODO bl improve name.
 */
r5js.runtime.PrimitiveProcedure.prototype.javascript;



/**
 * @param {function(!r5js.Datum):?} fn
 * @implements {r5js.runtime.PrimitiveProcedure}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.Unary_ = function(fn) {
  /** @const @private {function(!r5js.Datum):?} */
  this.fn_ = fn;
};


/** @override */
r5js.runtime.Unary_.prototype.javascript = function() {
  var numArgsFromUser = arguments.length;
  if (numArgsFromUser !== 1) {
    throw new r5js.IncorrectNumArgs('blah' /* TODO bl */, 1, numArgsFromUser);
  }
  var retval = this.fn_.apply(null, arguments);
  return r5js.data.maybeWrapResult(retval);
};


/**
 * @param {function(!r5js.Datum):?} fn
 * @return {!r5js.runtime.PrimitiveProcedure}
 */
r5js.runtime.unary = function(fn) {
    return new r5js.runtime.Unary_(fn);
};
