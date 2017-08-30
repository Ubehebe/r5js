goog.module('r5js.ListLikeTransformer');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const EllipsisTransformer = goog.require('r5js.EllipsisTransformer');
const MacroIdTransformer = goog.require('r5js.MacroIdTransformer');
const Quote = goog.require('r5js.ast.Quote');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const Subtransformer = goog.require('r5js.Subtransformer');
const TemplateBindings = goog.require('r5js.TemplateBindings');
const Vector = goog.require('r5js.ast.Vector');
const asserts = goog.require('goog.asserts');
const {DottedList, List} = goog.require('r5js.ast.List');

/**
 * @interface
 * @extends {Subtransformer}
 */
class ListLikeTransformer {
    /**
     * @param {!Subtransformer} subtransformer
     * @return {!ListLikeTransformer} This object, for chaining.
     */
    addSubtransformer(subtransformer) {}

    /** @return {string} */
    getName() {}

    /** @return {!ListLikeTransformer} */
    static dottedList() {
        return new DottedListTransformer();
    }

    /** @return {!ListLikeTransformer} */
    static list() {
        return new ListTransformer();
    }

    /** @return {!ListLikeTransformer} */
    static quote() {
        return new QuoteTransformer();
    }

    /** @return {!ListLikeTransformer} */
    static vector() {
        return new VectorTransformer();
    }
}

/** @implements {ListLikeTransformer} */
class Base {
    /** @param {function(new: Datum, !Datum)} ctor */
    constructor(ctor) {
        /** @const @private */ this.ctor_ = ctor;
        /** @const @private {!Array<!Subtransformer>} */ this.subtransformers_ = [];
    }

    /** @override */
    addSubtransformer(subtransformer) {
        this.subtransformers_.push(subtransformer);
        return this;
    }

    /** @override */
    getName() {
        return asserts.assertInstanceof(this.subtransformers_[0], MacroIdTransformer)
            .getDatum()
            .getPayload();
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, renameHelper) {
        for (let i = 0; i < this.subtransformers_.length; ++i) {
            this.subtransformers_[i].collectNestingLevels(ellipsisLevel, renameHelper);
        }
    }

    /**
     * @param {!Datum} inputDatum
     * @return {boolean}
     */
    couldMatch(inputDatum) {
        return false;
    }

    /** @override */
    toDatum(bindings) {
        const siblingBuffer = this.toSiblingBuffer_(bindings);
        return siblingBuffer && siblingBuffer.toList(this.ctor_);
    }

    /** @override */
    matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
        const len = this.subtransformers_.length;
        const maybeEllipsis = this.subtransformers_[len - 1] instanceof EllipsisTransformer
            && this.subtransformers_[len - 1];

        if (!this.couldMatch(inputDatum)) {
            return false;
        }

        inputDatum = /** @type {!CompoundDatum} */ (inputDatum);

        /* R5RS 4.3.2: "an input form F matches a pattern P if and only if [...]
         - P is a list (P1 ... Pn) and F is a list of n forms match P1 through Pn,
         respectively; or
         - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or
         improper list of n or more forms that match P1 through Pn, respectively,
         and whose nth "cdr" matches Pn+1; or
         - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
         the identifier ... and F is a proper list of at least n forms,
         the first n of which match P1 through Pn, respectively,
         and each remaining element of F matches Pn+1; or
         - P is a vector of the form #(P1 ...Pn) and F is a vector of n forms
         that match P1 through Pn; or
         - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
         the identifier ... and F is a vector of n or more forms the first n
         of which match P1 through Pn, respectively, and each remaining element
         of F matches Pn+1" */
        for (var subinput = inputDatum.getFirstChild(), i = 0;
             subinput;
             subinput = subinput.getNextSibling(), ++i) {

            if (i === len - 1 && maybeEllipsis) {
                // If there's an ellipsis in the pattern, break out to deal with it.
                break;
            } else if (i >= len) {
                /* If there's no ellipsis in the pattern and the input is longer
                 than the pattern, this is a failure. */
                return false;
            } else if (!this.subtransformers_[i].matchInput(
                    subinput, literalIds, definitionEnv, useEnv, bindings)) {
                /* If pattern matching on the subinput and subpattern fails, this is
                 a failure. */
                return false;
            }
        }

        if (maybeEllipsis) {
            /* Corner case:
             an empty input like () cannot match a pattern like (x y ...) */
            return (!inputDatum.getFirstChild() && len > 1) ?
                false :
                maybeEllipsis.matchInput(
                    /** @type {!Datum} */(subinput),
                    literalIds, definitionEnv, useEnv, bindings);
        } else {
            /* If we matched all of the input without getting through all of
             the pattern, this is a failure. */
            return i === len;
        }
    }

    /**
     * @param {!TemplateBindings} bindings
     * @return {?SiblingBuffer}
     * @private
     */
    toSiblingBuffer_(bindings) {
        const buf = new SiblingBuffer();
        const len = this.subtransformers_.length;

        for (let i = 0; i < len; ++i) {
            const success = /** @type {!Datum|boolean} */ (
                this.subtransformers_[i].toDatum(bindings));
            if (success === false) {
                return null;
            } else if (success) {
                buf.appendSibling(/** @type {!Datum} */ (success));
            }
        }
        return buf;
    }
}

/** @implements {ListLikeTransformer} */
class QuoteTransformer extends Base {
    constructor() {
        super(Quote);
    }

    /**
     * This is a no-op mainly so we don't accidentally rename identifiers inside
     * quotes in {@link r5js.Transformer#setupIds_}.
     * @override
     */
    collectNestingLevels() {}
}

/** @implements {ListLikeTransformer} */
class VectorTransformer extends Base {
    constructor() {
        super(Vector);
    }

    /** @override */
    couldMatch(inputDatum) {
        // Vector patterns match only vector inputs
        return inputDatum instanceof Vector;
    }
}

/** @implements {ListLikeTransformer} */
class ListTransformer extends Base {
    constructor() {
        super(List);
    }

    /** @override */
    couldMatch(inputDatum) {
        // Proper list patterns can match only proper list inputs
        return inputDatum instanceof List;
    }
}

/** @implements {ListLikeTransformer} */
class DottedListTransformer extends Base {
    constructor() {
        super(DottedList);
    }

    /** @override */
    couldMatch(inputDatum) {
        // Dotted list patterns can match proper or dotted list inputs
        return inputDatum instanceof List || inputDatum.isImproperList();
    }

    /** @override */
    matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
        inputDatum = /** @type {!CompoundDatum} */ (inputDatum);
        const len = this.subtransformers_.length;
        let maybeEllipsis =
            this.subtransformers_[len - 1] instanceof EllipsisTransformer
            && this.subtransformers_[len - 1];

        if (!this.couldMatch(inputDatum)) {
            return false;
        }

        /* R5RS 4.3.2: "an input form F matches a pattern P if and only if [...]
         - P is a list (P1 ... Pn) and F is a list of n forms match P1 through Pn,
         respectively; or
         - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or
         improper list of n or more forms that match P1 through Pn, respectively,
         and whose nth "cdr" matches Pn+1; or
         - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
         the identifier ... and F is a proper list of at least n forms,
         the first n of which match P1 through Pn, respectively,
         and each remaining element of F matches Pn+1; or
         - P is a vector of the form #(P1 ...Pn) and F is a vector of n forms
         that match P1 through Pn; or
         - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
         the identifier ... and F is a vector of n or more forms the first n
         of which match P1 through Pn, respectively, and each remaining element
         of F matches Pn+1" */
        for (var subinput = inputDatum.getFirstChild(), i = 0;
             subinput;
             subinput = subinput.getNextSibling(), ++i) {

            if (i === len - 1) {
                // If there's an ellipsis in the pattern, break out to deal with it.
                break;
            } else if (i >= len) {
                /* If there's no ellipsis in the pattern and the input is longer
                 than the pattern, this is a failure. */
                return false;
            } else if (!this.subtransformers_[i].matchInput(
                    subinput, literalIds, definitionEnv, useEnv, bindings)) {
                /* If pattern matching on the subinput and subpattern fails, this is
                 a failure. */
                return false;
            }
        }

        if (maybeEllipsis) {
            /* Corner case:
             an empty input like () cannot match a pattern like (x y ...) */
            return (!inputDatum.getFirstChild() && len > 1) ?
                false :
                maybeEllipsis.matchInput(
                    /** @type {!Datum} */ (subinput),
                    literalIds, definitionEnv, useEnv, bindings);
        } else {
            // Dotted-list patterns cannot end in ellipses.
            let toMatchAgainst;

            if (inputDatum instanceof List) {
                toMatchAgainst = new SiblingBuffer().
                    appendSibling(/** @type {!Datum} */ (subinput)).
                    toList(List);
            } else if (inputDatum.isImproperList()) {
                toMatchAgainst = subinput.getNextSibling()
                    ? new SiblingBuffer().appendSibling(subinput).toList(DottedList)
                    : subinput;
            }

            return this.subtransformers_[i].matchInput(
                /** @type {!Datum} */ (toMatchAgainst),
                literalIds, definitionEnv, useEnv, bindings);
        }
    }
}

exports = ListLikeTransformer;
