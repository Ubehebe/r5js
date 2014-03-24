goog.provide('r5js.ast.Macro');


goog.require('r5js.Datum');



/**
 * @param {!r5js.Macro} macro
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Macro = function(macro) {
  goog.base(this);
  this.setPayload(macro);
};
goog.inherits(r5js.ast.Macro, r5js.Datum);


/** @override */
r5js.ast.Macro.prototype.stringForOutputMode = function(outputMode) {
  return '[macro]';
};


/** @return {!r5js.Macro} */
r5js.ast.Macro.prototype.getMacro = function() {
  return this.getPayload().setIsLetOrLetrecSyntax();
};

