goog.provide('r5js.js.Html5Environment');


goog.require('goog.Promise');
goog.require('goog.labs.net.xhr');
goog.require('r5js.InMemoryInputPort');
goog.require('r5js.InMemoryOutputPort');



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


/** @override */
r5js.js.Html5Environment.prototype.getTerminal = function() {
  return new r5js.js.Html5Environment.Terminal_(this.jqConsole_);
};



/**
 * @param {?} jqconsole
 * @implements {r5js.Terminal}
 * @struct
 * @constructor
 * @private
 */
r5js.js.Html5Environment.Terminal_ = function(jqconsole) {
  /** @const @private */ this.jqconsole_ = jqconsole;
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.js.Html5Environment.Terminal_.prototype.getNextLineOfInput = function() {
  return new goog.Promise(function(resolve) {
    this.jqconsole_.Prompt(true /* history_enabled */, resolve);
  }, this);
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.js.Html5Environment.Terminal_.prototype.print = function(msg) {
  this.jqconsole_.Write(msg + '\n', 'jqconsole-output');
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.js.Html5Environment.Terminal_.prototype.error = function(msg) {
  this.jqconsole_.Write(msg + '\n', 'jqconsole-error');
};

