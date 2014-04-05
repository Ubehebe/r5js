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


/** @return {!r5js.js.Environment} */
r5js.js.Environment.get = function() {
  var isNode = typeof XMLHttpRequest === 'undefined';
  return isNode ?
      new r5js.js.NodeEnvironment() :
      new r5js.js.Html5Environment();
};
