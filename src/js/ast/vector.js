goog.module('r5js.ast.Vector');

const Datum = goog.require('r5js.Datum');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');

/**
 * TODO bl: this class illustrates the interpreter's confusion of abstract
 * syntax, intermediate representation, and runtime representation.
 * Having Scheme vectors be backed by JavaScript arrays is a reasonable
 * runtime decision, but is independent of AST and IR decisions.
 * This constructor cannot call {@link #convertVectorToArrayBacked_}
 * on its argument; that changes the structure of the syntax tree,
 * which affects parsing. Only the runtime primitive procedures can do this.
 */
class Vector extends CompoundDatum {
    /** @param {!Datum|!Array<!Datum>} firstChildOrArray */
    constructor(firstChildOrArray) {
        super();

        /** @private */ this.arrayBacked_ = goog.isArray(firstChildOrArray);

        /** @const @private {!Array<!Datum>} */ this.array_ = this.arrayBacked_
            ? /** @type {!Array<!Datum>} */ (firstChildOrArray)
            : [];

        if (firstChildOrArray instanceof Datum) {
            this.setFirstChild(firstChildOrArray);
        }
    }

    /** @return {number} */
    vectorLength() {
        if (!this.arrayBacked_) {
            this.convertVectorToArrayBacked_();
        }
        return this.array_.length;
    }

    /**
     * @param {number} index
     * @return {!Datum}
     */
    vectorRef(index) {
        if (!this.arrayBacked_) {
            this.convertVectorToArrayBacked_();
        }
        return this.array_[index];
    }

    /**
     * @param {number} index
     * @param {!Datum} val
     */
    vectorSet(index, val) {
        if (!this.arrayBacked_) {
            this.convertVectorToArrayBacked_();
        }
        this.array_[index] = val;
    }

    /**
     * Vector literals are constructed by the reader as linked lists
     * with no random access, while vectors created programmatically
     * via make-vector can just use JavaScript arrays. Instead of building
     * logic into the reader to convert its inefficient vectors to array-backed
     * ones, we check in every primitive vector procedure if the vector
     * is array-backed, and mutate it in place if it isn't. There may
     * be bugs involving the lost child/sibling pointers.
     * @private
     */
    convertVectorToArrayBacked_() {
        this.forEachChild(function (child) {
            this.array_.push(child);
        }, this);
        this.clearFirstChild();
        this.arrayBacked_ = true;
    }
}

exports = Vector;
