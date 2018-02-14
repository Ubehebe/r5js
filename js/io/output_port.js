goog.module('r5js.OutputPort');

/**
 * In Scheme, the main way of doing output is by passing a value to
 * the display and write procedures. (Less important output procedures include
 * write-char and newline.)
 *
 * R5RS 6.6.3 goes into detail about the differences between display and write;
 * it involves quote and backslash escaping, among other things.
 *
 * I think it makes more sense to let the output port implementations decide
 * what display and write do (and whether they do the same thing).
 * An implementation could, for example, convert the Scheme value
 * to some suitable value in the target environment, which need not be
 * a string. This is what {@link r5js.OutputSavingPort} does, for example.
 * @interface
 */
class OutputPort {
 /** @param {string} str String to write. */
 write(str) {}

 /** @see R5RS 6.6.1 */
 close() {}
}

/**
 * @param {*} obj
 * @return {boolean}
 * @suppress {reportUnknownTypes}
 * TODO bl temporary shim. Remove.
 */
function isOutputPortImpl(obj) {
    return !!(obj && obj[IMPLEMENTED_BY_PROP_]);
}

/**
 * @param {function(new: OutputPort, ...)} ctor
 * @suppress {reportUnknownTypes}
 */
function addOutputPortImpl(ctor) {
    ctor.prototype[IMPLEMENTED_BY_PROP_] = true;
}

/** @const @private */
const IMPLEMENTED_BY_PROP_ = '$r5js.OutputPort';

/**
 * An output port that discards its output.
 * @implements {OutputPort}
 */
class NullOutputPort {
 /** @override */ close() {}
 /** @override */ write() {}
}
addOutputPortImpl(NullOutputPort);

/** @const {!OutputPort} */
const NULL_OUTPUT_PORT = new NullOutputPort();

exports = {
 OutputPort,
 isOutputPortImpl,
 addOutputPortImpl,
 NULL_OUTPUT_PORT,
};
