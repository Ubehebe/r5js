goog.module('r5js.UserDefinedProcedure');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const ProcCall = goog.require('r5js.ProcCall');
const Procedure = goog.require('r5js.Procedure');
const Quote = goog.require('r5js.ast.Quote');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {List} = goog.require('r5js.ast.List');
const {Nonterminals} = goog.require('r5js.parse.Nonterminals');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');
const {Terminals} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');
const {extractDefinition} = goog.require('r5js.ast.util');

class UserDefinedProcedure extends Procedure {
    /**
     * @param {!Array<string>} formalsArray The procedure's formal parameters, in order.
     * @param {?Datum} bodyStart
     * @param {!IEnvironment} env An environment.
     * @param {string=} name The procedure's name. It has no semantic
     *     importance; it's just used for pretty-printing debugs and messages
     *     to the user. If not given, one will be created.
     */
    constructor(formalsArray, bodyStart, env, name=undefined) {
        super();

        /** @const @protected */
        this.formalsArray = formalsArray;

        /** @const @private {!IEnvironment} */
        this.env_ = env.child();

        /** @const @private {?ProcCallLike}*/
        this.body_ = bodyStart ? this.setupBody_(bodyStart) : null;

        /** @const @private {?ProcCallLike} */
        this.last_ = this.body_ ? ProcCallLike.getLast(this.body_) : null;

        /** @const @private */
        this.name_ = name || ('' + goog.getUid(this));
    }

    /** @return {string} */
    getName() {
        return this.name_;
    }

    /**
     * @param {!Datum} bodyStart
     * @return {!ProcCallLike}
     * @private
     */
    setupBody_(bodyStart) {
        const helper = new LetrecBindingsHelper();
        const letrecBindings = helper.collectLetrecBindings(bodyStart);
        if (letrecBindings.isEmpty()) {
            return /** @type {!ProcCallLike} */ (
                helper.getLast().sequence(this.env_));
        } else {
            const letrec = new List(letrecBindings.toSiblings());
            letrec.setNextSibling(/** @type {!Datum} */ (helper.getLast()));
            return new ProcCall(new Identifier('letrec'), letrec);
        }
    }

    /**
     * @param {!IEnvironment} env Environment to clone with.
     * @return {!UserDefinedProcedure} A clone of this procedure, with the given environment.
     * @suppress {const} for reassignment to body_ and last_.
     */
    cloneWithEnv(env) {
        const ans = new this.constructor(this.formalsArray, null /* bodyStart */, env);
        ans.env_.setClosuresFrom(this.env_); // non-cloning ok?
        ans.body_ = this.body_;
        ans.last_ = this.last_;
        return ans;
    }

    /**
     * @param {!ProcCallLike} procCallLike
     * @private
     * @suppress {checkTypes} procCallLike.getNext() can return null,
     * but apparently this is required. TODO bl investigate.
     */
    setContinuation_(procCallLike) {
        /* This will be a vacuous write for a tail call. But that is
         probably still faster than checking if we are in tail position and,
         if so, explicitly doing nothing. */
        if (this.last_) {
            this.last_.setNext(procCallLike.getNext());
            this.last_.setResultName(procCallLike.getResultName());
        }
    }

    /**
     * @param {!ProcCallLike} procCallLike
     * @return {boolean} True iff this procedure is in tail position.
     * @private
     * TODO bl are we sure this covers all forms of tail recursion in R5RS?
     */
    isTailCall_(procCallLike) {
        if (this.last_ === procCallLike) {
            // a good place to see if tail recursion is actually working :)
            // console.log('TAIL RECURSION!!!');
            return true;
        } else return false;
    }

    /** @override */
    toString() {
        return 'proc:' + this.name_;
    }

    /**
     * @param {!IEnvironment} env The environment to set.
     * @private
     */
    setEnv_(env) {
        if (this.body_) {
            this.body_.setStartingEnv(env);
        }
    }

    /**
     * @param {number} numActuals The number of arguments passed to the procedure
     * during evaluation.
     * @protected
     */
    checkNumArgs(numActuals) {
        if (numActuals !== this.formalsArray.length) {
            throw Error.incorrectNumArgs(this.toString(), this.formalsArray.length, numActuals);
        }
    }

    /**
     * @param {!Array<!Value>} args
     * @param {!IEnvironment} env
     * @protected
     */
    bindArgs(args, env) {
        for (let i = 0; i < this.formalsArray.length; ++i) {
            env.addBinding(this.formalsArray[i], args[i]);
        }
    }

    /**
     * Example: suppose we have
     *
     * (define (foo x y) (+ x (* 2 y)))
     *
     * The body of this procedure is desugared as
     *
     * (* 2 y [_0 (+ x _0 [_1 ...])])
     *
     * Then we have the (nested) procedure call
     *
     * (+ 1 (foo 3 4))
     *
     * which is desugared as
     *
     * (foo 3 4 [foo' (+ 1 foo' [_2 ...])])
     *
     * We bind the arguments ("1" and "2") to the formal parameters ("x" and "y"),
     * append the ProcCall's continuation to the end of the Procedure's
     * continuation, and advance to the beginning of the Procedure's body.
     * Thus, on the next iteration of the trampoline loop, we will have
     * the following:
     *
     * (* 2 y [_0 (+ x _0 [foo' (+ 1 foo' [_2 ...])])])
     * @override
     */
    evaluate(args, procCallLike, trampolineHelper, env) {
        const procCallEnv = procCallLike.getEnv();

        //If we're at a tail call we can reuse the existing environment.
        // Otherwise create a new environment pointing back to the current one.
        const newEnv = this.isTailCall_(procCallLike)
            ? procCallEnv.allowRedefs()
            : this.env_.child().addClosuresFrom(this.env_);

        const next = procCallLike.getNext();
        /* Remember to discard the new environment
         at the end of the procedure call. */
        if (procCallEnv && next && !next.getEnv()) {
            next.setStartingEnv(procCallEnv);
        }

        // Do some bookkeeping to prepare for jumping into the procedure
        this.setContinuation_(procCallLike);
        this.checkNumArgs(args.length);
        this.bindArgs(args, newEnv);
        this.setEnv_(newEnv);

        // And away we go
        trampolineHelper.setNext(
            /** @type {!ProcCallLike} */ (this.body_));
    }
}

class LetrecBindingsHelper {
    constructor() {
        /** @const @private */ this.bindings_ = new SiblingBuffer();
        /** @private {?Datum} */ this.last_ = null;
    }

    /**
     * R5RS 5.2.2: "A <body> containing internal definitions can always be
     * converted into a completely equivalent letrec expression."
     * @param {!Datum} bodyStart
     * @return {!SiblingBuffer}
     */
    collectLetrecBindings(bodyStart) {
        for (var cur = bodyStart;
             cur && cur.peekParse() === Nonterminals.DEFINITION;
             cur = cur.getNextSibling()) {
            cur = /** @type {!CompoundDatum} */ (cur);
            const firstChild = cur.getFirstChild();
            if (firstChild instanceof Identifier &&
                firstChild.getPayload() === Terminals.DEFINE) {
                this.bindings_.appendSibling(extractDefinition(cur));
            } else {
                cur.forEachChild(this.collectLetrecBindingsForChild_, this);
            }
        }
        this.last_ = cur;
        return this.bindings_;
    }

    /**
     * @param {!Datum} node
     * @private
     */
    collectLetrecBindingsForChild_(node) {
        if (!(node instanceof CompoundDatum)
            || node instanceof Quote) {
            return;
        }

        const firstChild = node.getFirstChild();

        if (firstChild instanceof Identifier
            && firstChild.getPayload() === Terminals.DEFINE) {
            this.bindings_.appendSibling(extractDefinition(node));
        } else if (node instanceof CompoundDatum) {
            node.forEachChild(this.collectLetrecBindingsForChild_, this);
        }
    }

    /** @return {?Datum} */
    getLast() {
        return this.last_;
    }
}

exports = UserDefinedProcedure;

