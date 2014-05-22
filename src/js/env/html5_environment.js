goog.provide('r5js.js.Html5Environment');


goog.require('goog.labs.net.xhr');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');
goog.require('r5js.repl.jqconsole.Console_');



/**
 * @param {?} jqConsole
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.Html5Environment = function(jqConsole) {
  /** @const @private */ this.jqConsole_ = jqConsole;
  /** @const @private {!Object.<string, !r5js.InMemoryPortBuffer>} */
  this.buffers_ = {};
};


/** @override */
r5js.js.Html5Environment.prototype.exit = goog.nullFunction;


/** @override */
r5js.js.Html5Environment.prototype.fetchUrl = goog.labs.net.xhr.get;


/** @override */
r5js.js.Html5Environment.prototype.newInputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = [];
  }
  return new r5js.InMemoryInputPort(this.buffers_[name]);
};


/** @override */
r5js.js.Html5Environment.prototype.newOutputPort = function(name) {
  if (!(name in this.buffers_)) {
    this.buffers_[name] = [];
  }
  return new r5js.InMemoryOutputPort(this.buffers_[name]);
};


/**
 * @override
 * @suppress {accessControls} TODO bl
 */
r5js.js.Html5Environment.prototype.getTerminal = function() {
  return new r5js.repl.jqconsole.Console_(this.jqConsole_);
};
