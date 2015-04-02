goog.provide('r5js.ast.Macro');


goog.require('r5js.ast.SimpleDatum');



r5js.ast.Macro = class extends r5js.ast.SimpleDatum {
 /**
  * @param {!r5js.Macro} macro
  */
 constructor(macro) {
  super(macro);
 }

 /** @return {!r5js.Macro} */
 getMacro() {
  return this.payload.setIsLetOrLetrecSyntax();
 }
};

