goog.module('r5js.ast.Boolean');

const SimpleDatum = goog.require('r5js.ast.SimpleDatum');

/** @extends {SimpleDatum<boolean>} */
class Boolean extends SimpleDatum {
 /** @param {boolean} val */
 constructor(val) {
  super(val);
 }
}

exports = Boolean;