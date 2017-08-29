goog.module('r5js.ast.CompoundDatum');

const CdrHelper = goog.require('r5js.ast.CdrHelper');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const RenameHelper = goog.require('r5js.RenameHelper');
const RenameUtil = goog.require('r5js.RenameUtil');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {Nonterminal, Nonterminals} = goog.require('r5js.parse.Nonterminals');

class CompoundDatum extends Datum {
    constructor() {
        super();
        /** @private {?Datum} */ this.firstChild_ = null;
        /** @private {?CdrHelper} */ this.cdrHelper_ = null;
        /** @protected {number|undefined} */ this.qqLevel;
    }

    /** @return {?Datum} */
    getFirstChild() {
        return this.firstChild_;
    }

    /** @param {!Datum} firstChild */
    setFirstChild(firstChild) {
        this.firstChild_ = firstChild;
    }

    /** @return {number|undefined} */
    getQQLevel() {
        return this.qqLevel;
    }

    /**
     * Clears the first child.
     * TODO bl is this necessary?
     */
    clearFirstChild() {
        this.firstChild_ = null;
    }

    /** @override */
    clone(parent) {
        const ans = /** @type {!CompoundDatum} */ (super.clone(parent));
        if (this.firstChild_) {
            const buf = new SiblingBuffer();
            this.forEachChild(child => buf.appendSibling(child.clone(ans)));
            ans.firstChild_ = buf.toSiblings();
        }
        return ans;
    }

    /**
     * TODO bl: this is intended to have the exact semantics of the library
     * procedure equal?, but I'm not sure that it does.
     * @param {!CompoundDatum} other Datum to compare against.
     * @return {boolean}
     */
    isEqual(other) {
        let thisChild, otherChild;
        for (thisChild = this.firstChild_, otherChild = other.firstChild_;
             thisChild && otherChild;
             thisChild = thisChild.getNextSibling(),
                 otherChild = otherChild.getNextSibling()) {
            if (thisChild instanceof CompoundDatum
                && otherChild instanceof CompoundDatum
                && !thisChild.isEqual(otherChild)) {
                return false;
            }
        }

        return !(thisChild || otherChild);
    }

    /** @override */
    fixParserSensitiveIds(helper) {
        if (this.hasParse(Nonterminals.LAMBDA_EXPRESSION)) {
            this.fixParserSensitiveIdsLambda_(helper);
        } else if (this.hasParse(Nonterminals.DEFINITION)) {
            this.fixParserSensitiveIdsDef_(helper);
        } else {
            for (let cur = this.firstChild_; cur; cur = cur.getNextSibling()) {
                cur.fixParserSensitiveIds(helper);
            }
        }
        super.fixParserSensitiveIds(helper);
    }

    /**
     * TODO bl: document what this method does.
     * @param {!RenameHelper} helper A rename helper.
     * @private
     */
    fixParserSensitiveIdsLambda_(helper) {
        const formalRoot = this.at(Nonterminals.FORMALS);
        const newHelper = new RenameHelper(helper);

        if (formalRoot instanceof Identifier) { // (lambda x ...)
            let id = formalRoot.getPayload();
            if (RenameUtil.isParserSensitiveId(id)) {
                formalRoot.setPayload(newHelper.addRenameBinding(id));
            }
        } else { // (lambda (x y) ...) or (lambda (x . y) ...)
            (/** @type {!CompoundDatum} */ (formalRoot)).forEachChild(
                function (child) {
                    child = /** @type {!Identifier} */ (child);
                    let id = child.getPayload();
                    if (RenameUtil.isParserSensitiveId(id)) {
                        child.setPayload(newHelper.addRenameBinding(id));
                    }
                });
        }

        formalRoot.getNextSibling().fixParserSensitiveIds(newHelper);
    }

    /**
     * TODO bl: document what this method does.
     * @param {!RenameHelper} helper A rename helper.
     * @private
     */
    fixParserSensitiveIdsDef_(helper) {
        const maybeVar = /** @type {?Identifier} */ (this.at(Nonterminals.VARIABLE));
        let id;

        if (maybeVar) { // (define foo +)
            id = maybeVar.getPayload();
            if (RenameUtil.isParserSensitiveId(id)) {
                maybeVar.setPayload(helper.addRenameBinding(id));
            }
        } else { // (define (foo x y) (+ x y))
            const vars = /** @type {!CompoundDatum} */ (this.firstChild_.getNextSibling());
            const name = /** @type {!Identifier} */ (vars.firstChild_);
            const newHelper = new RenameHelper(helper);
            for (let cur = name.getNextSibling(); cur; cur = cur.getNextSibling()) {
                cur = /** @type {!Identifier} */ (cur);
                id = cur.getPayload();
                if (RenameUtil.isParserSensitiveId(id)) {
                    cur.setPayload(newHelper.addRenameBinding(id));
                }
            }
            vars.getNextSibling().fixParserSensitiveIds(newHelper);
            const namePayload = name.getPayload();
            if (RenameUtil.isParserSensitiveId(namePayload)) {
                name.setPayload(helper.addRenameBinding(namePayload));
            }
        }
    }

    /**
     * @param {!Nonterminal} type
     * @return {?Datum}
     */
    at(type) {
        for (let cur = this.firstChild_; cur; cur = cur.getNextSibling()) {
            if (cur.peekParse() === type) {
                return cur;
            }
        }
        return null;
    }

    /**
     * @param {!CdrHelper} cdrHelper A cdr helper.
     * @return {!Datum} This object, for chaining.
     */
    setCdrHelper(cdrHelper) {
        this.cdrHelper_ = cdrHelper;
        return this;
    }

    /** @return {?CdrHelper} The CdrHelper for this Datum, if one exists. */
    getCdrHelper() {
        return this.cdrHelper_;
    }

    /**
     * @return {?CompoundDatum} The first child of this datum that is
     * itself a list, or null if no such datum exists.
     */
    firstSublist() {
        for (let child = this.firstChild_; child; child = child.getNextSibling()) {
            if (child instanceof CompoundDatum) {
                return child;
            }
        }
        return null;
    }

    /** @override */
    resetDesugars() {
        super.resetDesugars();
        this.forEachChild(child => child.resetDesugars());
    }

    /**
     * @param {function(this: T, !Datum)} callback
     * @param {T=} context
     * @template T
     */
    forEachChild(callback, context=undefined) {
        for (let cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
            callback.call(context, cur);
        }
    }

    /**
     * Map isn't the best word, since the function returns an array
     * but the children are represented as a linked list.
     * @param {function(this:SCOPE, !Datum):T} f Function for transforming
     * an individual child.
     * @param {SCOPE=} context Optional receiver for f.
     * @return {!Array<T>} Array of transformed children.
     * @template SCOPE,T
     */
    mapChildren(f, context=undefined) {
        const ans = [];
        for (let cur = this.getFirstChild(); cur; cur = cur.getNextSibling()) {
            ans.push(f.call(context, cur));
        }
        return ans;
    }

    /**
     * This penetrates quotations because it's used in quasiquote evaluation.
     * @param {function(!Datum):boolean} predicate Children passing
     * this predicate are transformed according to the transform parameter.
     * @param {function(!Datum):?Datum} transform Function
     * that will transform children that pass the predicate.
     * @return {!Datum} This object, for chaining.
     */
    replaceChildren(predicate, transform) {

        for (var cur = this.firstChild_, prev;
             cur;
             prev = cur, cur = cur.getNextSibling()) {
            if (predicate(/** @type {!Datum} */(cur))) {
                const tmp = cur.getNextSibling();
                cur.setNextSibling(null);
                /* We have to assign to cur so prev will be set correctly
                 in the next iteration. */
                if (cur = transform(/** @type {!Datum} */(cur))) {

                    if (prev) {
                        prev.setNextSibling(cur);
                    } else {
                        this.firstChild_ = cur;
                    }

                    /* If cur suddenly has a sibling, it must have been inserted
                     by the transform. That is, the transform wants to insert
                     multiple siblings in place of the single node. (Use case: in

                     `(1 ,@(list 2 3) 4)

                     the members of the sublist (2 3), not the sublist itself,
                     should be inserted into the main list.)

                     In this case we should skip ahead to the last sibling inserted
                     by the transform in order to avoid accidentally running the
                     transform on those newly-inserted siblings, which would
                     presumably not be wanted. */
                    if (cur.getNextSibling()) {
                        cur = cur.lastSibling();
                    }

                    cur.setNextSibling(/** @type {!Datum} */ (tmp));
                }

                /* If transform returned null, that means the current node
                 should be spliced out of the list. */
                else {
                    prev.setNextSibling(/** @type {!Datum} */ (tmp));
                    cur = prev;
                }
            } else if (cur instanceof CompoundDatum) {
                cur.replaceChildren(predicate, transform);
            }
        }
        return this;
    }

    /**
     * Example:
     *
     * `(a `(b ,(+ x y) ,(foo ,(+ z w) d) e) f)
     *
     * should be decorated as
     *
     * `1(a `2(b ,2(+ x y) ,2(foo ,1(+ z w) d) e) f)
     *
     * @param {number} qqLevel The level of quasiquotation.
     * @return {!Datum} This object, for chaining.
     */
    setQuasiquotationLevel(qqLevel) {
        this.forEachChild(child => {
            if (child instanceof CompoundDatum) {
                child.setQuasiquotationLevel(qqLevel);
            }
        });
        return this;
    }
}

exports = CompoundDatum;
