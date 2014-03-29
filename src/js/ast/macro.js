goog.provide('r5js.ast.Macro');


goog.require('r5js.ast.SimpleDatum');



/**
 * @param {!r5js.Macro} macro
 * @extends {r5js.ast.SimpleDatum.<!r5js.Macro>}
 * @struct
 * @constructor
 */
r5js.ast.Macro = function(macro) {
  goog.base(this, macro);
};
goog.inherits(r5js.ast.Macro, r5js.ast.SimpleDatum);


/** @override */
r5js.ast.Macro.prototype.stringForOutputMode = function(outputMode) {
  return '[macro]';
};


/** @return {!r5js.Macro} */
r5js.ast.Macro.prototype.getMacro = function() {
  return this.getPayload().setIsLetOrLetrecSyntax();
};

