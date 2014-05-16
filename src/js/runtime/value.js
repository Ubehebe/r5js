goog.provide('r5js.runtime.EOF');
goog.provide('r5js.runtime.ObjectValue');
goog.provide('r5js.runtime.PrimitiveValue');
goog.provide('r5js.runtime.UNSPECIFIED_VALUE');
goog.provide('r5js.runtime.Value');


goog.require('r5js.Datum');


/**
 * Typedef for Scheme values that this implementation represents with
 * primitive JavaScript values.
 * @typedef {boolean|number|string}
 */
r5js.runtime.PrimitiveValue;



/**
 * Interface for Scheme values that this implementation represents with
 * JavaScript objects.
 * @interface
 */
r5js.runtime.ObjectValue = function() {};


/**
 * Top-level typedef for all Scheme values.
 * @typedef {!r5js.runtime.PrimitiveValue|!r5js.runtime.ObjectValue}
 */
r5js.runtime.Value;



/**
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 * @private
 */
r5js.runtime.UnspecifiedValue_ = function() {
  goog.base(this);
};
goog.inherits(r5js.runtime.UnspecifiedValue_, r5js.Datum);


/**
 * @const {!r5js.runtime.ObjectValue}
 * TODO bl: there is no reason to represent unspecified runtime values
 * as a Datum object. This is especially concerning because Datums have mutable
 * state. It appears unspecified values are leaking into contexts that assume
 * they are datums (for example the implementation of car).
 */
r5js.runtime.UNSPECIFIED_VALUE = new r5js.runtime.UnspecifiedValue_();



/** @const */ r5js.runtime.EOF = new r5js.Datum();

