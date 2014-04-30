goog.provide('r5js.Pair');



/** @interface */
r5js.Pair = function() {};


/** @return {!r5js.runtime.Value} */
r5js.Pair.prototype.car = function() {};


/** @return {!r5js.runtime.Value} */
r5js.Pair.prototype.cdr = function() {};


/** @const @private */
r5js.Pair.IMPLEMENTED_BY_PROP_ = '$r5js.Pair';


/**
 * @param {*} obj
 * @return {boolean}
 */
r5js.Pair.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.Pair.IMPLEMENTED_BY_PROP_]);
};


/**
 * @param {function(new: r5js.Pair, ...)} ctor
 */
r5js.Pair.addImplementation = function(ctor) {
  ctor.prototype[r5js.Pair.IMPLEMENTED_BY_PROP_] = true;
};
