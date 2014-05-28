goog.provide('r5js.js.Environment');


goog.require('r5js.js.Html5Environment');
goog.require('r5js.js.NodeEnvironment');



/**
 * Abstraction of the (JavaScript) environment that the Scheme implementation
 * is running in.
 * @interface
 */
r5js.js.Environment = function() {};


/**
 * @param {string} url
 * @return {!goog.Promise.<string>}
 */
r5js.js.Environment.prototype.fetchUrl = function(url) {};


/** @param {number} statusCode */
r5js.js.Environment.prototype.exit = function(statusCode) {};


/**
 * @param {string} name
 * @return {!r5js.InputPort}
 */
r5js.js.Environment.prototype.newInputPort = function(name) {};


/**
 * @param {string} name
 * @return {!r5js.OutputPort}
 */
r5js.js.Environment.prototype.newOutputPort = function(name) {};


/**
 * @param {!r5js.Evaluator} evaluator TODO bl: ideally, {@link r5js.Terminal}
 * implementations would be "dumb", knowing nothing about Scheme.
 * One complication is multiline input, where terminals often show a different
 * prompt if the current line is a continuation of the last line. One of the
 * Terminal implementations, {@link r5js.js.Html5Environment.Terminal_},
 * has a quirky API that requires the implementation to know whether
 * the current line will complete. This parameter is passed in order to
 * communicate that knowledge.
 * @return {!r5js.Terminal}
 */
r5js.js.Environment.prototype.getTerminal = function(evaluator) {};


/** @return {!r5js.js.Environment} */
r5js.js.Environment.get = function() {
  var isNode = typeof XMLHttpRequest === 'undefined';
  return isNode ?
      new r5js.js.NodeEnvironment() :
      new r5js.js.Html5Environment(arguments[0] /* TODO bl improve */);
};
