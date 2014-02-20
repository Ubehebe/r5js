goog.provide('r5js.DatumStream');



/** @interface */
r5js.DatumStream = function() {};


/** @return {r5js.Datum} The next datum, or null if there is none. */
r5js.DatumStream.prototype.getNextDatum = function() {};


/** @param {!r5js.Datum} next */
r5js.DatumStream.prototype.advanceTo = function(next) {};


/** Advances to child. */
r5js.DatumStream.prototype.advanceToChild = function() {};


/** Advances to sibling. */
r5js.DatumStream.prototype.advanceToNextSibling = function() {};


/** @return {boolean} True iff the advance worked. */
r5js.DatumStream.prototype.maybeAdvanceToNextSiblingOfParent = function() {};


/** TODO bl remove. */
r5js.DatumStream.prototype.maybeRecoverAfterDeeplyNestedList = function() {};
