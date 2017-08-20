goog.module('r5js.runtime.UNSPECIFIED_VALUE');

const Datum = goog.require('r5js.Datum');
const {Value} = goog.require('r5js.Value');

/** @type {!Value} */ const UNSPECIFIED_VALUE = new Datum();

exports = UNSPECIFIED_VALUE;