goog.provide('r5js.platform.android.Terminal');


goog.require('r5js.Terminal');
goog.require('r5js.error');



/**
 * @implements {r5js.Terminal}
 * @struct
 * @constructor
 */
r5js.platform.android.Terminal = function() {};


/** @override */
r5js.platform.android.Terminal.prototype.print = function(str) {
  AndroidSchemePlatform.print(str);
};


/** @override */
r5js.platform.android.Terminal.prototype.error = function(str) {
  AndroidSchemePlatform.error(str);
};


/** @override */
r5js.platform.android.Terminal.prototype.getNextLineOfInput = function() {
  throw r5js.error.unimplementedOption('getNextLineOfInput');
};
