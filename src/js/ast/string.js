goog.provide('r5js.ast.String');


goog.require('r5js.ast.SimpleDatum');



r5js.ast.String = /** @extends {r5js.ast.SimpleDatum<string>} */
 class extends r5js.ast.SimpleDatum {

    /** @param {string} s */
    constructor(s) {
        super(s);
    }

    /**
     * Unlike other simple datums, strings have reference equality semantics.
     * @see R5RS 6.1
     * @override
     */
    eqv(other) {
        return this === other;
    }

    /**
     * Datums representing strings have payloads of type string.
     * If they all unwrapped as JavaScript strings, it would be impossible
     * to re-wrap them correctly (noninjective mapping). We choose to store
     * identifiers unwrapped because they're expected to be more common than
     * strings.
     *
     * @override
     */
    unwrap() {
        return this;
    }
};