goog.provide('r5js.Token');



/**
 * @interface
 * @template T
 */
r5js.Token = function() {};


/** @return {T} */
r5js.Token.prototype.getPayload = function() {};


/**
 * @param {!r5js.scan.TokenType} type
 * @return {boolean}
 * TODO bl: temporary shim. Remove.
 */
r5js.Token.prototype.matchesType = function(type) {};



