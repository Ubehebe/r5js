goog.provide('r5js.IPair');



/** @interface */
r5js.IPair = /** @interface */ class {
 /** @return {!r5js.runtime.Value} */
 car() {}

 /** @return {!r5js.runtime.Value} */
 cdr() {}

};


/** @const @private */
r5js.IPair.IMPLEMENTED_BY_PROP_ = '$r5js.IPair';


/**
 * @param {*} obj
 * @return {boolean}
 */
r5js.IPair.isImplementedBy = function(obj) {
  return !!(obj && obj[r5js.IPair.IMPLEMENTED_BY_PROP_]);
};


/**
 * @param {function(new: r5js.IPair, ...)} ctor
 */
r5js.IPair.addImplementation = function(ctor) {
  ctor.prototype[r5js.IPair.IMPLEMENTED_BY_PROP_] = true;
};
