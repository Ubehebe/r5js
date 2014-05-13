goog.provide('r5js.js.Html5Environment');


goog.require('goog.labs.net.xhr');
goog.require('r5js.Pipe');



/**
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.Html5Environment = function() {
  /** @const @private {!Object.<string, !r5js.Pipe>} */ this.ports_ = {};
};


/** @override */
r5js.js.Html5Environment.prototype.exit = goog.nullFunction;


/** @override */
r5js.js.Html5Environment.prototype.fetchUrl = goog.labs.net.xhr.get;


/** @override */
r5js.js.Html5Environment.prototype.newInputPort = function(name) {
  if (!(name in this.ports_)) {
    this.ports_[name] = new r5js.Pipe();
  }
  return this.ports_[name];
};


/** @override */
r5js.js.Html5Environment.prototype.newOutputPort =
    r5js.js.Html5Environment.prototype.newInputPort;
