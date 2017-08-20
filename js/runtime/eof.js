goog.module('r5js.runtime.EOF');

const Datum = goog.require('r5js.Datum');
const {Value} = goog.require('r5js.Value');

/** @type {!Value} */ const EOF = new Datum();

exports = EOF;