goog.module('r5js.DatumStream');

const Datum = goog.require('r5js.Datum');

/** @interface */
class DatumStream {
 /** @return {Datum} The next datum, or null if there is none. */
 getNextDatum() {}

 /** @param {!Datum} next */
 advanceTo(next) {}

 /** Advances to child. */
 advanceToChild() {}

 /** Advances to sibling. */
 advanceToNextSibling() {}

 /** @return {boolean} True iff the advance worked. */
 maybeAdvanceToNextSiblingOfParent() {}
}

exports = DatumStream;
