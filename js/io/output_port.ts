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
 */
// TODO: this is logically an interface. Declaring as a class for now to work around
// tsickle/closure compiler issues.
export class OutputPort {
 write(str: string) {}

 /** @see R5RS 6.6.1 */
 close() {}
}

/** TODO bl temporary shim. Remove. */
export function isOutputPortImpl(obj: any): boolean {
    return !!(obj && obj[IMPLEMENTED_BY_PROP_]);
}

export function addOutputPortImpl(ctor: any) {
    ctor.prototype[IMPLEMENTED_BY_PROP_] = true;
}

/** @const @private */
const IMPLEMENTED_BY_PROP_ = '$r5js.OutputPort';

/** An output port that discards its output. */
class NullOutputPort extends OutputPort {
    constructor() {
        super();
    }
 /** @override */ close() {}
 /** @override */ write() {}
}
addOutputPortImpl(NullOutputPort);

/** @const {!OutputPort} */
export const NULL_OUTPUT_PORT: OutputPort = new NullOutputPort();
