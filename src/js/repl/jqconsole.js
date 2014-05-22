goog.provide('r5js.repl.jqconsole.Console_');


goog.require('goog.Promise');



/**
 * @param {?} jqconsole
 * @implements {r5js.Terminal}
 * @struct
 * @constructor
 * @private
 */
r5js.repl.jqconsole.Console_ = function(jqconsole) {
  /** @const @private */ this.jqconsole_ = jqconsole;
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.repl.jqconsole.Console_.prototype.getNextLineOfInput = function() {
  return new goog.Promise(function(resolve) {
    this.jqconsole_.Prompt(true /* history_enabled */, resolve);
  }, this);
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.repl.jqconsole.Console_.prototype.print = function(msg) {
  this.jqconsole_.Write(msg, 'jqconsole-output');
};


/**
 * @override
 * @suppress {checkTypes} for the jqconsole integration
 */
r5js.repl.jqconsole.Console_.prototype.error = function(msg) {
  this.jqconsole_.Write(msg, 'jqconsole-error');
};
