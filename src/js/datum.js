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
    /** @protected {r5js.Datum} */
    this.firstChild_ = null;

    /** @private {r5js.Datum} */
    this.nextSibling_ = null;

    /**
     * Only for last children.
     * @private {r5js.Datum}
     */
    this.parent_ = null;

    /** @const @private {!Array.<!r5js.parse.Nonterminal>} */
    this.nonterminals_ = [];

    /** @const @private {!Array.<!r5js.DesugarFunc>} */
    this.desugars_ = [];

    /** @private {number} */
    this.nextDesugar_ = -1;

    /** @private */ this.immutable_ = false;

    /** @protected {number|undefined} */
    this.qqLevel;

    /** @protected {r5js.CdrHelper} */
    this.cdrHelper_;
};


/**
 * @param {!r5js.OutputMode} outputMode
 * @return {string}
 */
r5js.Datum.prototype.stringForOutputMode = goog.abstractMethod;


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


/**
 * @return {boolean} True iff {@link r5js.Datum.setImmutable} has been called
 * on this Datum.
 */
r5js.Datum.prototype.isImmutable = function() {
    return this.immutable_;
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

    if (this.parent_) {
        ans.parent_ = this.parent_;
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
};

/**
 * @param {!r5js.IEnvironment} env TODO bl
 * @param {boolean=} opt_forceContinuationWrapper TODO bl document
 * @return {!r5js.Datum|!r5js.Continuable|!r5js.ITransformer|!r5js.Macro|null}
 */
r5js.Datum.prototype.desugar = function(env, opt_forceContinuationWrapper) {
    var desugarFn = (this.desugars_ && this.nextDesugar_ >= 0) ?
        this.desugars_[this.nextDesugar_--] : null;
    var ans = desugarFn ? desugarFn(this, env) : this;
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
 * @param {!r5js.Datum} other
 * @return {boolean} Whether the two datums are equivalent in the sense of eqv?
 * For most kinds of datum, this means reference equality.
 * @see R5RS 6.1
 */
r5js.Datum.prototype.eqv = function(other) {
    return this === other;
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
    return this;
};

/**
 * @return {!r5js.Datum} The last sibling of this Datum, or this Datum if it's
 * the last sibling.
 */
r5js.Datum.prototype.lastSibling = function() {
    return this.nextSibling_ ? this.nextSibling_.lastSibling() : this;
};


/**
 * @param {!r5js.CdrHelper} cdrHelper A cdr helper.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.setCdrHelper = function(cdrHelper) {
    this.cdrHelper_ = cdrHelper;
    return this;
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
    var id;

    if (formalRoot instanceof r5js.ast.Identifier) { // (lambda x ...)
        id = formalRoot.getPayload();
        if (isParserSensitiveId(id)) {
            formalRoot.setPayload(newHelper.addRenameBinding(id));
        }
    } else { // (lambda (x y) ...) or (lambda (x . y) ...)
        (/** @type {!r5js.ast.CompoundDatum} */ (formalRoot)).forEachChild(
            function(child) {
            child = /** @type {!r5js.ast.Identifier} */ (child);
            id = child.getPayload();
            if (isParserSensitiveId(id)) {
                child.setPayload(newHelper.addRenameBinding(id));
            }
        });
    }

    formalRoot.nextSibling_.fixParserSensitiveIds(newHelper);
};

/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 * @private
 */
r5js.Datum.prototype.fixParserSensitiveIdsDef_ = function(helper) {
    var maybeVar = /** @type {r5js.ast.Identifier} */ (
        this.at(r5js.parse.Nonterminals.VARIABLE));
    var id;

    if (maybeVar) { // (define foo +)
        id = maybeVar.getPayload();
        if (isParserSensitiveId(id)) {
            maybeVar.setPayload(helper.addRenameBinding(id));
        }
    } else { // (define (foo x y) (+ x y))
        var vars = this.firstChild_.nextSibling_;
        var name = /** @type {!r5js.ast.Identifier} */ (vars.firstChild_);
        var newHelper = new r5js.RenameHelper(helper);
        for (var cur = name.nextSibling_; cur; cur = cur.nextSibling_) {
            cur = /** @type {!r5js.ast.Identifier} */ (cur);
            id = cur.getPayload();
            if (isParserSensitiveId(id)) {
                cur.setPayload(newHelper.addRenameBinding(id));
            }
        }
        vars.nextSibling_.fixParserSensitiveIds(newHelper);
        var namePayload = name.getPayload();
        if (isParserSensitiveId(namePayload)) {
            name.setPayload(helper.addRenameBinding(namePayload));
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
    } else {
        for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
            cur.fixParserSensitiveIds(helper);
        }
    }

    if (this.nextSibling_) {
        this.nextSibling_.fixParserSensitiveIds(helper);
    }
};

