goog.provide('r5js.Ref');


goog.require('r5js.ast.SimpleDatum');


/**
 * TODO bl this class should not exist. It's used only as a shim in
 * {@link r5js.Environment#get}.
 */
r5js.Ref = /** @extends {r5js.ast.SimpleDatum<!r5js.Datum>} */
 class extends r5js.ast.SimpleDatum {
 /** @param {!r5js.Datum} deref Datum to dereference. */
 constructor(deref) {
  super(deref);
 }

 /** @return {!r5js.Datum} */
 deref() {
  return this.payload;
 }
};
