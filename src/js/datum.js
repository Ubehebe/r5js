/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.ast.Literal');
goog.provide('r5js.ast.SimpleDatum');
goog.provide('r5js.Datum');


goog.require('r5js.ast.Node');
goog.require('r5js.Continuable');
goog.require('r5js.ContinuableHelper');
goog.require('r5js.DatumType');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.OutputMode');
goog.require('r5js.RenameHelper');
goog.require('r5js.SiblingBuffer');


/**
 * TODO bl: this is out of control. Create an interface and have each
 * type implement it (or wrap them, in the case of JavaScript primitives).
 * @typedef {Array|boolean|Function|number|Object|r5js.Datum|r5js.Macro|r5js.Procedure|string}
 */
r5js.PayloadType;


/** @typedef {function(!r5js.Datum, !r5js.IEnvironment):
* (!r5js.Datum|!r5js.Continuable|!r5js.ITransformer|!r5js.Macro|null)}
 * TODO bl: narrow this typedef.
 */
r5js.DesugarFunc;



/**
 * @struct
 * @constructor
 */
r5js.Datum = function() {
    /** @private {r5js.Datum} */
    this.firstChild_ = null;

    /** @private {r5js.Datum} */
    this.nextSibling_ = null;

    /**
     * Only for last children.
     * @private {r5js.Datum}
     */
    this.parent_ = null;

    /** @private {r5js.PayloadType} */
    this.payload_;

    /** @const @private {!Array.<!r5js.parse.Nonterminal>} */
    this.nonterminals_ = [];

    /** @const @private {!Array.<!r5js.DesugarFunc>} */
    this.desugars_ = [];

    /** @private {number} */
    this.nextDesugar_ = -1;

    /** @private */ this.immutable_ = false;

    /** @protected {number|undefined} */
    this.qqLevel;

    /** @private {r5js.CdrHelper} */
    this.cdrHelper_;
};


/**
 * @param {!r5js.OutputMode} outputMode
 * @return {string}
 */
r5js.Datum.prototype.stringForOutputMode = goog.abstractMethod;


/**
 * @param {function(this: T, !r5js.Datum)} callback
 * @param {T=} opt_context
 * @template T
 */
r5js.Datum.prototype.forEachChild = function(callback, opt_context) {
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        callback.call(opt_context, cur);
    }
};


/**
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.setImmutable = function() {
    this.immutable_ = true;
    return this;
};


/** @return {r5js.Datum} */
r5js.Datum.prototype.getParent = function() {
    return this.parent_;
};


/** @param {!r5js.Datum} parent */
r5js.Datum.prototype.setParent = function(parent) {
    this.parent_ = parent;
};


/** @return {r5js.Datum} */
r5js.Datum.prototype.getFirstChild = function() {
    return this.firstChild_;
};


/** @param {!r5js.Datum} firstChild */
r5js.Datum.prototype.setFirstChild = function(firstChild) {
    this.firstChild_ = firstChild;
};


/** @return {r5js.Datum} */
r5js.Datum.prototype.getNextSibling = function() {
    return this.nextSibling_;
};


/** @param {!r5js.Datum} nextSibling */
r5js.Datum.prototype.setNextSibling = function(nextSibling) {
    this.nextSibling_ = nextSibling;
};


/** @return {number|undefined} */
r5js.Datum.prototype.getQQLevel = function() {
    return this.qqLevel;
};


/** @return {r5js.PayloadType} */
r5js.Datum.prototype.getPayload = function() {
    return this.payload_;
};


/** @param {!r5js.PayloadType} payload */
r5js.Datum.prototype.setPayload = function(payload) {
    this.payload_ = payload;
};


/**
 * @return {boolean} True iff {@link r5js.Datum.setImmutable} has been called
 * on this Datum.
 */
r5js.Datum.prototype.isImmutable = function() {
    return this.immutable_;
};

/**
 * This penetrates quotations because it's used in quasiquote evaluation.
 * @param {function(!r5js.Datum):boolean} predicate Children passing
 * this predicate are transformed according to the transform parameter.
 * @param {function(!r5js.Datum):r5js.Datum} transform Function
 * that will transform children that pass the predicate.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.replaceChildren = function(predicate, transform) {

    for (var cur = this.firstChild_, prev; cur; prev = cur,cur = cur.nextSibling_) {
        if (predicate(/** @type {!r5js.Datum} */(cur))) {
            var tmp = cur.nextSibling_;
            cur.nextSibling_ = null;
            /* We have to assign to cur so prev will be set correctly
             in the next iteration. */
            if (cur = transform(/** @type {!r5js.Datum} */(cur))) {

                if (prev) {
                    prev.nextSibling_ = cur;
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
                if (cur.nextSibling_) {
                    cur = cur.lastSibling();
                }

                cur.nextSibling_ = tmp;
            }

            /* If transform returned null, that means the current node
            should be spliced out of the list. */
            else {
                prev.nextSibling_ = tmp;
                cur = prev;
            }
        } else {
            cur.replaceChildren(predicate, transform);
        }
    }
    return this;
};


/**
 * @param {r5js.Datum} parent Datum to use for the parent of the clone, if any.
 * @return {!r5js.Datum} A new clone of this Datum object.
 */
r5js.Datum.prototype.clone = function(parent) {

    /* Invariant: although cyclical Datum structures can be created by
     the programmer (through set-cdr!, etc.), they will never be cloned.
     They are created by mutation, i.e. once a value is already bound in an
     Environment, and once that happens, we never clone it again. */

    var ans = new this.constructor();

    ans.payload_ = this.payload_;

    if (this.parent_) {
        ans.parent_ = this.parent_;
    }
    if (this.firstChild_) {
        var buf = new r5js.SiblingBuffer();
        for (var child = this.firstChild_; child; child = child.nextSibling_) {
            buf.appendSibling(child.clone(ans));
        }
        ans.firstChild_ = buf.toSiblings();
    }
    // We only need the parent_ pointer on the last sibling.
    if (!this.nextSibling_) {
        ans.parent_ = parent;
    }
    if (this.immutable_) {
        ans.immutable_ = true;
    }

    return ans;
};


/** @param {!r5js.parse.Nonterminal} type */
r5js.Datum.prototype.setParse = function(type) {
    this.nonterminals_.push(type);
};

/** @param {!r5js.DesugarFunc} desugarFunc */
r5js.Datum.prototype.setDesugar = function(desugarFunc) {
    this.desugars_.push(desugarFunc);
    ++this.nextDesugar_;
};


/** @return {r5js.parse.Nonterminal|null} */
r5js.Datum.prototype.peekParse = function() {
        var len = this.nonterminals_.length;
        return len > 0 ? this.nonterminals_[len - 1] : null;
};

/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {boolean} True iff this Datum parses as the given nonterminal.
 * @private
 */
r5js.Datum.prototype.hasParse_ = function(nonterminal) {
    if (this.nonterminals_) {
        var len = this.nonterminals_.length;
        for (var i = 0; i < len; ++i) {
            if (this.nonterminals_[i] === nonterminal) {
                return true;
            }
        }
    }
    return false;
};

/**
 * @param {!r5js.parse.Nonterminal} type
 * @return {r5js.Datum}
 */
r5js.Datum.prototype.at = function(type) {
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        if (cur.peekParse() === type) {
            return cur;
        }
    }
    return null;
};


/**
 * @return {r5js.Datum} The first child of this datum that is itself a list,
 * or null if no such datum exists.
 * TODO bl: move to list base class when it exists.
 */
r5js.Datum.prototype.firstSublist = function() {
    for (var child = this.firstChild_; child; child = child.nextSibling_) {
        if (child instanceof r5js.ast.List) {
            return child;
        }
    }
    return null;
};

/**
 * @param {!r5js.Datum} sibling Sibling to append.
 */
r5js.Datum.prototype.appendSibling = function(sibling) {
    if (!this.nextSibling_) {
        if (this.parent_) {
            // Propagate the parent_ field
            sibling.parent_ = this.parent_;
            // Only the last sibling needs a link back to the parent_
            this.parent_ = null;
        }
        this.nextSibling_ = sibling;
    } else {
        this.nextSibling_.appendSibling(sibling);
    }
};

/**
 * If we used this to append n children in a row, it would take time O(n^2).
 * But we don't actually use it like that. When building a list like (X*),
 * we build up the list of X's in linear time, then call appendChild
 * once to append the whole list as a child of the list root.
 * We do incur some overhead when building a list like (X+ . X): in this case,
 * the X+ list is appended in one go, and then we have to re-traverse that list
 * once to append the final X. I expect this to be rare enough not to matter
 * in practice, but if necessary we could keep track of the root's final child.
 * @param {!r5js.Datum} child Child to append.
 */
r5js.Datum.prototype.appendChild = function(child) {
    if (!this.firstChild_) {
        this.firstChild_ = child;
    } else {
        this.firstChild_.appendSibling(child);
    }
};


/**
 * Map isn't the best word, since the function returns an array
 * but the children are represented as a linked list.
 * @param {function(this:SCOPE, !r5js.Datum):T} f Function for transforming
 * an individual child.
 * @param {SCOPE=} opt_context Optional receiver for f.
 * @return {!Array.<T>} Array of transformed children.
 * @template SCOPE,T
 */
r5js.Datum.prototype.mapChildren = function(f, opt_context) {
    var ans = [];
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        ans.push(f.call(opt_context, cur));
    }
    return ans;
};

/**
 * @return {boolean} True if this datum represents an improper list.
 * TODO bl: remove. Callers shouldn't be dispatching on this. Rather, the
 * list/dotted list behavior differences should be built into the Datum
 * subclasses.
 */
r5js.Datum.prototype.isImproperList = function() {
    return false;
};


/**
 * TODO bl: document why you would call this method.
 */
r5js.Datum.prototype.resetDesugars = function() {
    if (this.nextDesugar_ === -1) {
        this.nextDesugar_ += this.desugars_.length;
    }
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        cur.resetDesugars();
    }
};

/**
 * @param {!r5js.IEnvironment} env TODO bl
 * @param {boolean=} opt_forceContinuationWrapper TODO bl document
 * @return {!r5js.Datum|!r5js.Continuable|!r5js.ITransformer|!r5js.Macro|null}
 */
r5js.Datum.prototype.desugar = function(env, opt_forceContinuationWrapper) {
    var desugarFn = (this.desugars_ && this.nextDesugar_ >= 0) ?
        this.desugars_[this.nextDesugar_--] : null;
    var ans;
    if (desugarFn) {
        ans = desugarFn(this, env);
    } else if (this.firstChild_ &&
        this.firstChild_.payload_ === r5js.parse.Terminals.BEGIN) {
        ans = this.firstChild_.nextSibling_ ? this.firstChild_.nextSibling_.sequence(env) : null;
    } else {
        ans = this;
    }

    if (opt_forceContinuationWrapper && !(ans instanceof r5js.Continuable)) {
        ans = newIdShim(ans);
    }
    return ans;
};


/**
 * @param {!r5js.IEnvironment} env TODO bl
 * @return {r5js.Continuable}
 */
r5js.Datum.prototype.sequence = function(env) {
    var first = null, tmp, curEnd;
    for (var cur = this; cur; cur = cur.nextSibling_) {
        if (tmp = cur.desugar(env)) {

            /* Nodes that have no desugar functions (for example, variables
             and literals) desugar as themselves. Sometimes this is OK
             (for example in Datum.sequenceOperands), but here we need to be
             able to connect the Continuable objects correctly, so we
             wrap them. */
            if (!(tmp instanceof r5js.Continuable)) {
                tmp = newIdShim(tmp);
            }

            if (!first) {
                first = tmp;
            } else if (curEnd) {
                curEnd.nextContinuable = tmp;
            }

            curEnd = tmp.getLastContinuable().continuation;
        }
    }

    return first; // can be undefined
};


/**
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.ast.Literal = function() {
    goog.base(this);
};
goog.inherits(r5js.ast.Literal, r5js.Datum);


/**
 * TODO bl: this is intended to have the exact semantics of the library
 * procedure equal?, but I'm not sure that it does.
 * (I put it in JavaScript for fast access from the macro subsystem,
 * which needs it in one case.)
 * @param {!r5js.Datum} other Datum to compare against.
 */
r5js.Datum.prototype.isEqual = function(other) {
    if (this.payload_ === other.payload_) {
        var thisChild, otherChild;
        for (thisChild = this.firstChild_,otherChild = other.firstChild_;
             thisChild && otherChild;
             thisChild = thisChild.nextSibling_,otherChild = otherChild.nextSibling_) {
            if (!thisChild.isEqual(otherChild)) {
                return false;
            }
        }

        return !(thisChild || otherChild);

    } else return false;
};


/**
 * Datums representing identifiers, strings, and characters
 * all have payloads of type string. If they all unwrapped as JavaScript
 * strings, it would be impossible to re-wrap them correctly
 * (noninjective mapping). We choose to store identifiers unwrapped
 * because they're expected to be more common than the other two.
 *
 * Environment specifiers cannot be unwrapped to their Environment
 * payloads because Environment values in Environments already have
 * a meaning, namely, a redirect to look up the name in some other
 * Environment.
 *
 * Finally, the vector stuff may need to be overhauled.
 */
r5js.Datum.prototype.unwrap = function() {
    return this.payload_ !== undefined
        ? this.payload_
        : this;
};

/**
 * @return {!r5js.Datum} The last sibling of this Datum, or this Datum if it's
 * the last sibling.
 */
r5js.Datum.prototype.lastSibling = function() {
    return this.nextSibling_ ? this.nextSibling_.lastSibling() : this;
};


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
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.setQuasiquotationLevel = function(qqLevel) {
    this.forEachChild(function(child) { child.setQuasiquotationLevel(qqLevel); });
    return this;
};


/**
 * @param {!r5js.CdrHelper} cdrHelper A cdr helper.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.setCdrHelper = function(cdrHelper) {
    this.cdrHelper_ = cdrHelper;
    return this;
};

/**
 * @return {r5js.CdrHelper} The CdrHelper for this Datum, if one exists.
 */
r5js.Datum.prototype.getCdrHelper = function() {
    return this.cdrHelper_;
};

/** @return {string} */
function newCpsName() {
    /* TODO bl: goog.getUid requires an object parameter, so this method
       creates a throwaway object. Requiring this function to take an object
       parameter could reduce garbage. */
    return r5js.Datum.CPS_PREFIX_ + goog.getUid(new Object());
}

/** @return {string} */
function newAnonymousLambdaName() {
    /* TODO bl: goog.getUid requires an object parameter, so this method
    creates a throwaway object. Requiring this function to take an object
    parameter could reduce garbage. */
    return r5js.Datum.PROC_PREFIX_ + goog.getUid(new Object());
}

/**
 * Not a valid identifier prefix so we can easily tell these apart.
 * @const {string}
 * @private
 */
r5js.Datum.CPS_PREFIX_ = '@';


/** @const @private {string} */
r5js.Datum.PROC_PREFIX_ = 'proc';




/**
 * TODO bl: document what this method does.
 * @return {r5js.Datum}
 */
r5js.Datum.prototype.closestAncestorSibling = function() {
    if (this.nextSibling_) {
        return this.nextSibling_;
    } else if (!this.parent_) {
        return null;
    } else {
        return this.parent_.closestAncestorSibling();
    }
};



/**
 * See comments at the top of Parser.
 * @param {string} name identifier name to check.
 * @return {boolean} True iff the given name is parser-sensitive.
 */
function isParserSensitiveId(name) {
    switch (name) {
        case r5js.parse.Terminals.BEGIN:
        case r5js.parse.Terminals.DEFINE:
        case r5js.parse.Terminals.DEFINE_SYNTAX:
        case r5js.parse.Terminals.IF:
        case r5js.parse.Terminals.LAMBDA:
        case r5js.parse.Terminals.LET_SYNTAX:
        case r5js.parse.Terminals.LETREC_SYNTAX:
        case r5js.parse.Terminals.QUASIQUOTE:
        case r5js.parse.Terminals.QUOTE:
        case r5js.parse.Terminals.SET:
        case r5js.parse.Terminals.UNQUOTE:
        case r5js.parse.Terminals.UNQUOTE_SPLICING:
            return true;
        default:
            return false;
    }
}



/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 * @private
 */
r5js.Datum.prototype.fixParserSensitiveIdsLambda_ = function(helper) {
    var formalRoot = this.at(r5js.parse.Nonterminals.FORMALS);

    var newHelper = new r5js.RenameHelper(helper);
    var cur;

    // (lambda (x y) ...) or (lambda (x . y) ...)
    if (formalRoot.firstChild_) {
        for (cur = formalRoot.firstChild_; cur; cur = cur.nextSibling_) {
            var id = /** @type {string} */ (cur.payload_);
            if (isParserSensitiveId(id)) {
                cur.payload_ = newHelper.addRenameBinding(id);
            }
        }
    }

    // (lambda x ...)
    else if (cur && isParserSensitiveId(/** @type {string} */(formalRoot.payload_))) {
        cur.payload_ = newHelper.addRenameBinding(
            /** @type {string} */ (formalRoot.payload_));
    }

    formalRoot.nextSibling_.fixParserSensitiveIds(newHelper);
};

/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 * @private
 */
r5js.Datum.prototype.fixParserSensitiveIdsDef_ = function(helper) {
    var maybeVar = this.at(r5js.parse.Nonterminals.VARIABLE);

    if (maybeVar) {
        var id = /** @type {string} */ (maybeVar.payload_);
        if (isParserSensitiveId(id)) {
            maybeVar.payload_ =helper.addRenameBinding(id);
        }
    } else {
        var vars = this.firstChild_.nextSibling_;
        var name = vars.firstChild_;
        var newHelper = new r5js.RenameHelper(helper);
        for (var cur = name.nextSibling_; cur; cur = cur.nextSibling_) {
            var payload = /** @type {string} */ (cur.payload_);
            if (isParserSensitiveId(payload)) {
                cur.payload_ = newHelper.addRenameBinding(payload);
            }
        }
        vars.nextSibling_.fixParserSensitiveIds(newHelper);
        var namePayload = /** @type {string} */ (name.payload_);
        if (isParserSensitiveId(namePayload)) {
            name.payload_ = helper.addRenameBinding(namePayload);
        }
    }
};

/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 */
r5js.Datum.prototype.fixParserSensitiveIds = function(helper) {
    if (this.hasParse_(r5js.parse.Nonterminals.LAMBDA_EXPRESSION)) {
        this.fixParserSensitiveIdsLambda_(helper);
    } else if (this.hasParse_(r5js.parse.Nonterminals.DEFINITION)) {
        this.fixParserSensitiveIdsDef_(helper);
    } else if (isParserSensitiveId(/** @type {string} */ (this.payload_))) {
        this.payload_ =
            helper.getRenameBinding(/** @type {string} */(this.payload_)) ||
                this.payload_;
    } else {
        for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
            cur.fixParserSensitiveIds(helper);
        }
    }

    if (this.nextSibling_) {
        this.nextSibling_.fixParserSensitiveIds(helper);
    }
};


/**
 * @param {T} payload
 * @extends {r5js.ast.Literal}
 * @struct
 * @constructor
 * @template T
 */
r5js.ast.SimpleDatum = function(payload) {
  goog.base(this);
  this.setPayload(payload);
};
goog.inherits(r5js.ast.SimpleDatum, r5js.ast.Literal);

