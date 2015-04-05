goog.module('r5js.Reader');

const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');

/** @interface */
class Reader {
 /**
  * @return {!Datum} The root of the datum tree.
  * If reading the tokens into datums was unsuccessful, a {@link r5js.ReadError}
  * is thrown.
  * @throws {Error}
  */
 read() {}
}

exports = Reader;
