goog.provide('r5js.js.Html5Environment');


goog.require('goog.labs.net.xhr');



/**
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.Html5Environment = function() {};


/** @override */
r5js.js.Html5Environment.prototype.exit = goog.nullFunction;


/** @override */
r5js.js.Html5Environment.prototype.fetchUrl = goog.labs.net.xhr.get;


/**
 * @override
 * @suppress {checkTypes} TODO bl implement
 */
r5js.js.Html5Environment.prototype.newInputPort = function(name) {};


/**
 * @override
 * @suppress {checkTypes} TODO bl implement
 */
r5js.js.Html5Environment.prototype.newOutputPort = function(name) {};