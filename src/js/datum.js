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


goog.provide('r5js.data');
goog.provide('r5js.Datum');


goog.require('r5js.Continuable');
goog.require('r5js.ContinuableHelper');
goog.require('r5js.DatumType');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.RenameHelper');
goog.require('r5js.Macro');
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

    /**
     * @private {r5js.Type|null}
     * TODO bl make non-nullable.
     */
    this.type_;

    /** @private {r5js.PayloadType} */
    this.payload_;

    /** @const @private {!Array.<!r5js.parse.Nonterminal>} */
    this.nonterminals_ = [];

    /** @const @private {!Array.<!r5js.DesugarFunc>} */
    this.desugars_ = [];

    /** @private {number} */
    this.nextDesugar_ = -1;

    /**
     * Only for procedures.
     * @private {string|undefined}
     */
    this.name_;

    /** @type {boolean} */
    this.immutable_ = false;

    /** @private {number|undefined} */
    this.qqLevel_;

    /** @private {r5js.CdrHelper} */
    this.cdrHelper_;
};



/**
 * Executes a callback on this Datum and on each of its children, in order.
 * TODO bl: there are too many of these utility functions.
 * Reduce to a minimal set.
 * @param {function(!r5js.Datum)} callback Callback to execute.
 */
r5js.Datum.prototype.forEach = function(callback) {
    /* Quotations are like pseudo-leaves in the datum tree, so they should
     be opaque to this function. */
    if (!this.isQuote()) {
        callback(this);
        for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
            cur.forEach(callback);
        }
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
    return this.qqLevel_;
};


/** @return {string|undefined} */
r5js.Datum.prototype.getName = function() {
    return this.name_;
};


/** @return {r5js.PayloadType} */
r5js.Datum.prototype.getPayload = function() {
    return this.payload_;
};


/** @param {!r5js.PayloadType} payload */
r5js.Datum.prototype.setPayload = function(payload) {
    this.payload_ = payload;
};


/** @return {!r5js.Type|null} */
r5js.Datum.prototype.getType = function() {
    return this.type_;
};


/** @param {!r5js.Type} type */
r5js.Datum.prototype.setType = function(type) {
    this.type_ = type;
};

/**
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.setImmutableOnQuote = function() {
    if (this.firstChild_) {
        switch (this.firstChild_.type_) {
            case r5js.DatumType.LIST:
            case r5js.DatumType.DOTTED_LIST:
            case r5js.DatumType.VECTOR:
                this.firstChild_.setImmutable();
        }
    }
    return this;
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

function newEmptyList() {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.LIST;
    return ans;
}



/**
 * @return {boolean} True iff this Datum represents an empty list.
 */
r5js.Datum.prototype.isEmptyList = function() {
    return this.isList() && !this.firstChild_;
};

/**
 * @param {!r5js.Datum} other Datum to compare against.
 * @returns {boolean} True iff both Datum objects have the same type.
 */
r5js.Datum.prototype.sameTypeAs = function(other) {
    return this.type_ === other.type_;
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

    var ans = new r5js.Datum();

    ans.type_ = this.type_;
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
    if (this.name_) {
        ans.name_ = this.name_;
    }
    if (this.immutable_) {
        ans.immutable_ = true;
    }

    return ans;
};

/**
 * TODO bl document what this method does.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.unescapeStringLiteral = function() {
    this.payload_ = this.payload_.replace(/\\(["\\])/g, "$1");
    return this;
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

/**
 * TODO bl: document what this method does.
 */
r5js.Datum.prototype.unsetParse = function() {
    this.nonterminals_.length = 0;
    for (var child = this.firstChild_; child; child = child.nextSibling_) {
        child.unsetParse();
    }
};

/**
 * @return {*} TODO bl
 */
r5js.Datum.prototype.peekParse = function() {
    if (this.nonterminals_) {
        var len = this.nonterminals_.length;
        if (len > 0) {
            return this.nonterminals_[len - 1];
        }
    }
    return null;
};

/**
 * @param {*} nonterminal TODO bl
 * @return {boolean} True iff this Datum parses as the given nonterminal.
 */
r5js.Datum.prototype.hasParse = function(nonterminal) {
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
 * @param {string} type TODO bl
 * @returns {*} TODO bl
 */
r5js.Datum.prototype.at = function(type) {
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        /* The first clause is a convenience for things like node.at('(');
         the second is a convenience for things like node.at('expression') */
        if (cur.type_ === type || cur.peekParse() === type) {
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
 * @param {!r5js.Datum} child Child to append.
 */
r5js.Datum.prototype.prependChild = function(child) {
    var oldFirstChild = this.firstChild_;
    this.firstChild_ = child;
    child.nextSibling_ = oldFirstChild;
};

/**
 * Map isn't the best word, since the function returns an array
 * but the children are represented as a linked list.
 * @param {function(!r5js.Datum):!r5js.Datum} f Function for transforming
 * an individual child.
 * @return {!Array.<!r5js.Datum>} Array of transformed children.
 */
r5js.Datum.prototype.mapChildren = function(f) {
    var ans = [];
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        ans.push(f(/** @type {!r5js.Datum} */(cur)));
    }
    return ans;
};

/** @return {boolean} True if this datum represents an improper list. */
r5js.Datum.prototype.isImproperList = function() {
    return this.type_ === r5js.DatumType.DOTTED_LIST;
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
 * @param {boolean=} forceContinuationWrapper TODO bl document
 * @return {*} TODO bl
 */
r5js.Datum.prototype.desugar = function(env, forceContinuationWrapper) {
    var desugarFn = (this.desugars_ && this.nextDesugar_ >= 0) ?
        this.desugars_[this.nextDesugar_--] : null;
    var ans;
    if (desugarFn) {
        ans = desugarFn(this, env);
    } else if (this.firstChild_ && this.firstChild_.payload_ === 'begin') {
        ans = this.firstChild_.nextSibling_ ? this.firstChild_.nextSibling_.sequence(env) : null;
    } else {
        ans = this;
    }

    if (forceContinuationWrapper && !(ans instanceof r5js.Continuable)) {
        ans = newIdShim(ans);
    }
    return ans;
};


/**
 * @return {!r5js.Datum} Dereferences this Datum if possible; otherwise just
 * returns this Datum.
 */
r5js.Datum.prototype.maybeDeref = function () {
    return this.type_ === r5js.DatumType.REF ?
    /** @type {!r5js.Datum} */ (this.payload_) :
        this;
};



/**
 * @return {r5js.Macro} TODO bl
 */
r5js.Datum.prototype.getMacro = function() {
    if (this.payload_ instanceof r5js.Macro) {
        return this.payload_.setIsLetOrLetrecSyntax();
    } else {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    }
};

/**
 * @param {!Array} array TODO bl Narrow the generic type.
 * @return {!r5js.Datum} A new Datum representing the given array.
 */
function newVectorDatum(array) {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.VECTOR;
    ans.payload_ = array;
    return ans;
}

/** @return {boolean} True iff this datum represents a lambda. */
r5js.Datum.prototype.isProcedure = function() {
  return this.type_ === r5js.DatumType.LAMBDA;
};

/** @return {boolean} True iff this datum represents an input or output port. */
r5js.Datum.prototype.isPort = function() {
    return this.isInputPort() || this.isOutputPort();
};

/** @return {boolean} True iff this datum represents an input port. */
r5js.Datum.prototype.isInputPort = function() {
    return this.type_ === r5js.DatumType.INPUT_PORT;
};

/** @return {boolean} True iff this datum represents an output port. */
r5js.Datum.prototype.isOutputPort = function() {
    return this.type_ === r5js.DatumType.OUTPUT_PORT;
};

/** @return {boolean} True iff this datum represents a macro. */
r5js.Datum.prototype.isMacro = function() {
    return this.type_ === r5js.DatumType.MACRO;
};


/**
 * @return {boolean} True iff this datum represents an environment specifier.
 */
r5js.Datum.prototype.isEnvironmentSpecifier = function() {
    return this.type_ === r5js.DatumType.ENVIRONMENT_SPECIFIER;
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
 * @return {boolean} True iff this datum represents a list.
 */
r5js.Datum.prototype.isList = function() {
    return this.type_ === r5js.DatumType.LIST;
};

/**
 * @return {boolean} True iff this datum represents a vector.
 */
r5js.Datum.prototype.isVector = function() {
    return this.type_ === r5js.DatumType.VECTOR;
};

/**
 * @return {boolean} True iff this datum represents a vector
 * and is backed by a JavaScript array.
 * See {@link r5js.Datum.convertVectorToArrayBacked}.
 * TODO bl: this method doesn't actually check that the datum represents
 * a vector.
 */
r5js.Datum.prototype.isArrayBacked = function() {
    return !!this.payload_;
};

/**
 * Vector literals are constructed by the reader as linked lists
 * with no random access, while vectors created programmatically
 * via make-vector can just use JavaScript arrays. Instead of building
 * logic into the reader to convert its inefficient vectors to array-backed
 * ones, we check in every primitive vector procedure if the vector
 * is array-backed, and mutate it in place if it isn't. There may
 * be bugs involving the lost child/sibling pointers.
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.convertVectorToArrayBacked = function () {
    this.payload_ = [];
    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_)
        this.payload_.push(cur);
    this.firstChild_ = null;
    return this;
};

/** @return {boolean} True iff this datum represents a boolean. */
r5js.Datum.prototype.isBoolean = function() {
    return this.type_ === r5js.DatumType.BOOLEAN;
};

/** @return {boolean} True iff this datum represents an identifier. */
r5js.Datum.prototype.isIdentifier = function() {
    return this.type_ === r5js.DatumType.IDENTIFIER;
};

/** @return {boolean} True iff this datum represents a character. */
r5js.Datum.prototype.isCharacter = function() {
    return this.type_ === r5js.DatumType.CHARACTER;
};

/** @return {boolean} True iff this datum represents a number. */
r5js.Datum.prototype.isNumber = function() {
    return this.type_ === r5js.DatumType.NUMBER;
};

/** @return {boolean} True iff this datum represents a string. */
r5js.Datum.prototype.isString = function() {
    return this.type_ === r5js.DatumType.STRING;
};

/**
 * @return {boolean} True iff this datum represents a boolean, identifier,
 * character, number, string, or lambda.
 * TODO bl: would "isPrimitive" be a better name?
 */
r5js.Datum.prototype.isLiteral = function() {
    switch (this.type_) {
        case r5js.DatumType.BOOLEAN:
        case r5js.DatumType.IDENTIFIER:
        case r5js.DatumType.CHARACTER:
        case r5js.DatumType.NUMBER:
        case r5js.DatumType.STRING:
        case r5js.DatumType.LAMBDA:
        case r5js.DatumType.QUOTE:
            return true;
        default:
        return false;
    }
};

/**
 * @return {boolean} True iff this datum represents a quotation (quote or ').
 */
r5js.Datum.prototype.isQuote = function() {
    return this.type_ === r5js.DatumType.QUOTE ||
        (this.isList() && !!this.firstChild_ && this.firstChild_.payload_ === 'quote');
    // todo bl should datums know about this?
};

/** @return {boolean} True iff this datum represents a quasiquotation (`). */
r5js.Datum.prototype.isQuasiquote = function() {
    return this.type_ === r5js.DatumType.QUASIQUOTE;
};

/**
 * @return {boolean} True iff this datum has no type.
 * TODO bl why is this useful?
 */
r5js.Datum.prototype.isUndefined = function() {
  return this.type_ === null;
};

/**
 * In most situations, we want to detect both unquote (,) and
 * unquote-splicing (,@).
 * @return {boolean} True iff this datum represents an unquote
 * or an unquote-splicing.
 */
r5js.Datum.prototype.isUnquote = function() {
    return this.type_ === r5js.DatumType.UNQUOTE ||
        this.type_ === r5js.DatumType.UNQUOTE_SPLICING;
};

/**
 * TODO bl: this is intended to have the exact semantics of the library
 * procedure equal?, but I'm not sure that it does.
 * (I put it in JavaScript for fast access from the macro subsystem,
 * which needs it in one case.)
 * @param {!r5js.Datum} other Datum to compare against.
 */
r5js.Datum.prototype.isEqual = function(other) {
    if (other instanceof r5js.Datum
        && this.type_ === other.type_
        && this.payload_ === other.payload_) {
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
 * @return {!r5js.Datum} A new Datum representing a quotation.
 */
r5js.Datum.prototype.quote = function() {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.QUOTE;
    ans.firstChild_ = this;
    return ans;
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

    return (this.payload_ !== undefined
        && !this.isVector() // watch out for 0's and falses
        && !this.isEnvironmentSpecifier()
        && !this.isString()
        && !this.isCharacter()
        && !this.isUndefined()
        && !this.isPort())
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
 * (x . ()) is equivalent to (x). It is useful to perform this normalization
 * prior to evaluation time to simplify the Scheme procedure "list?".
 * With normalization, we can merely say, (list? x) iff x.isList().
 * Without normalization, we would also have to check if x is an
 * improper list, and if so, whether its last element was an empty list.
 *
 * This is also an opportune time to do these:
 *
 * (quote x) -> 'x
 * (quasiquote x) -> `x
 * (unquote x) -> ,x
 * (unquote-splicing x) -> ,@x
 *
 * so we don't have to worry about these synonyms during evaluation proper.
 *
 * @return {!r5js.Datum} This object, for chaining.
 */
r5js.Datum.prototype.normalizeInput = function() {

    if (this.firstChild_) {
        switch (this.firstChild_.payload_) {
            case 'quote':
                this.type_ = r5js.DatumType.QUOTE;
                this.firstChild_ = this.firstChild_.nextSibling_;
                break;
            case 'quasiquote':
                this.type_ = r5js.DatumType.QUASIQUOTE;
                this.firstChild_ = this.firstChild_.nextSibling_;
                break;
            case 'unquote':
                this.type_ = r5js.DatumType.UNQUOTE;
                this.firstChild_ = this.firstChild_.nextSibling_;
                break;
            case 'unquote-splicing':
                this.type_ = r5js.DatumType.UNQUOTE_SPLICING;
                this.firstChild_ = this.firstChild_.nextSibling_;
                break;
        }
    }

    var isImproperList = this.isImproperList();

    for (var child = this.firstChild_; child; child = child.nextSibling_) {
        child.normalizeInput();
        if (isImproperList && child.nextSibling_ && !child.nextSibling_.nextSibling_) {
            var maybeEmptyList = child.nextSibling_;
            if (maybeEmptyList.isList() && !maybeEmptyList.firstChild_) {
                child.parent_ = child.nextSibling_.parent_;
                child.nextSibling_ = null;
                this.type_ = r5js.DatumType.LIST;
            }
        }
    }

    if (this.isString()) {
        this.unescapeStringLiteral();
    }

    return this.setImmutableOnQuote();
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
r5js.Datum.prototype.decorateQuasiquote = function(qqLevel) {

    if (this.isQuasiquote()) {
        this.qqLevel_ = qqLevel;
    } else if (this.isUnquote()) {
        this.qqLevel_ = qqLevel+1;
    }

    for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
        if (cur.isQuasiquote()) {
            cur.decorateQuasiquote(qqLevel+1);
        } else if (cur.isUnquote()) {
            cur.decorateQuasiquote(qqLevel-1);
        } else {
            cur.decorateQuasiquote(qqLevel);
        }
    }

    return this;
};

/**
 * Notice that our representation of lists is not recursive:
 * the "second element" of (x y z) is y, not (y z). So we provide this function
 * as an aid whenever we want that recursive property. Mainly, this is for cdr:
 * we allocate a new head-of-list object and point it to the second element
 * of the list in question.
 *
 * Unfortunately, this approach breaks referential transparency:
 * (cdr x) does not point to the same region of memory as
 * x.firstChild_.nextSibling_. So we have to build in special logic
 * to the primitive equivalence predicates, and especially into the primitive
 * mutation procedures (set-car! and set-cdr!).
 * That is what {@link r5js.CdrHelper} does.
 *
 * Conceptually, it would not be difficult to switch to an internal car/cdr
 * representation, and the performance would be similar. But practically,
 * it would involve a lot of refactoring, because the pointers are manipulated
 * directly (without function calls) all over the place.
 * So it's a "nice to have".
 *
 * @param {boolean} dotted True iff this should be a dotted list.
 * @return {!r5js.Datum} A new Datum representing the siblings of this datum.
 */
r5js.Datum.prototype.siblingsToList = function(dotted) {
    var ans = new r5js.Datum();
    ans.type_ = dotted ? r5js.DatumType.DOTTED_LIST : r5js.DatumType.LIST;
    ans.firstChild_ = this;
    return ans;
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
 * @return {boolean} True iff this Datum is in a quasiquotation and should be
 * unquoted (i.e. starts with a ,).
 */
r5js.Datum.prototype.shouldUnquote = function() {
    return this.isIdentifier() && this.payload_.charAt(0) === ',';
};

/**
 * This is a subcase of shouldUnquote, because unquotes and unquote-splicings
 * have pretty much the same logic.
 * @return {boolean} TODO bl
 */
r5js.Datum.prototype.shouldUnquoteSplice = function() {
    return this.isIdentifier() && this.payload_.charAt(1) === '@';
};

/**
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @return {!r5js.Datum} New Datum representing this datum's definition.
 */
r5js.Datum.prototype.extractDefinition = function() {
    var variable = this.at('variable');
    var list = newEmptyList();
    if (variable) {
        list.prependChild(this.at('expression'));
    } else {
        var formalsList = this.firstChild_.nextSibling_;
        variable = formalsList.firstChild_;
        var bodyStart = formalsList.nextSibling_;
        var lambda = newEmptyList();
        lambda.firstChild_ = bodyStart;
        var newFormalsList = formalsList;
        newFormalsList.firstChild_ = newFormalsList.firstChild_.nextSibling_;
        if (newFormalsList.isImproperList() && !newFormalsList.firstChild_.nextSibling_) {
            lambda.prependChild(r5js.data.newIdOrLiteral(newFormalsList.firstChild_.payload_));
        } else {
            lambda.prependChild(newFormalsList);
        }
        lambda.prependChild(r5js.data.newIdOrLiteral('lambda'));
        list.prependChild(lambda);
    }
    list.prependChild(variable);
    return list;
};

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
        case 'begin':
        case 'define':
        case 'define-syntax':
        case 'if':
        case 'lambda':
        case 'let-syntax':
        case 'letrec-syntax':
        case 'quasiquote':
        case 'quote':
        case 'set!':
        case 'unquote':
        case 'unquote-splicing':
            return true;
        default:
            return false;
    }
}

/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 */
r5js.Datum.prototype.fixParserSensitiveIdsLambda = function(helper) {
    var formalRoot = this.at('formals');

    var newHelper = new r5js.RenameHelper(helper);
    var cur;

    // (lambda (x y) ...) or (lambda (x . y) ...)
    if (formalRoot.firstChild_) {
        for (cur = formalRoot.firstChild_; cur; cur = cur.nextSibling_) {
            if (isParserSensitiveId(cur.payload_)) {
                cur.payload_ = newHelper.addRenameBinding(cur.payload_);
            }
        }
    }

    // (lambda x ...)
    else if (cur && isParserSensitiveId(formalRoot.payload_)) {
        cur.payload_ = newHelper.addRenameBinding(formalRoot.payload_);
    }

    formalRoot.nextSibling_.fixParserSensitiveIds(newHelper);
};

/**
 * TODO bl: document what this method does.
 * @param {!r5js.RenameHelper} helper A rename helper.
 */
r5js.Datum.prototype.fixParserSensitiveIdsDef = function(helper) {
    var maybeVar = this.at('variable');

    if (maybeVar) {
        if (isParserSensitiveId(maybeVar.payload_)) {
            maybeVar.payload_ = helper.addRenameBinding(maybeVar.payload_);
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

    if (this.hasParse('lambda-expression')) {
        this.fixParserSensitiveIdsLambda(helper);
    } else if (this.hasParse('definition')) {
        this.fixParserSensitiveIdsDef(helper);
    } else if (isParserSensitiveId(/** @type {string} */ (this.payload_))) {
        this.payload_ =
            helper.getRenameBinding(/** @type {string} */(this.payload_)) ||
                this.payload_;
    } else if (this.isQuote()) {
        ; // no-op
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
 * Convenience functions for manipulating {@link r5js.Datum} objects.
 */
r5js.data = {};


/**
 * @param {r5js.PayloadType} payload
 * @param {!r5js.Type=} opt_type The type of the Datum.
 * If not given, defaults to {@link r5js.DatumType.IDENTIFIER}.
 * @return {!r5js.Datum} New Datum of given type with given payload.
 */
r5js.data.newIdOrLiteral = function(payload, opt_type) {
    // todo bl: we're sometimes creating these with undefined payloads! Investigate.
    var ans = new r5js.Datum();
    ans.type_ = opt_type || r5js.DatumType.IDENTIFIER;
    ans.payload_ = payload;
    return ans;
};


/**
 * @param {string} name Name of the procedure.
 * @param {Function|!r5js.Procedure} procedure TODO bl.
 * @return {!r5js.Datum} New Datum representing the given procedure.
 */
r5js.data.newProcedureDatum = function(name, procedure) {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.LAMBDA;
    ans.payload_ = procedure;
    ans.name_ = name;
    return ans;
};


/**
 * @param {!r5js.Port} port The port to use.
 * @return {!r5js.Datum} New Datum representing an input port.
 */
r5js.data.newInputPortDatum = function(port) {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.INPUT_PORT;
    ans.payload_ = port;
    return ans;
};


/**
 * @param {!r5js.Port} port The port to use.
 * @return {!r5js.Datum} New Datum representing an output port.
 */
r5js.data.newOutputPortDatum = function(port) {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.OUTPUT_PORT;
    ans.payload_ = port;
    return ans;
};

/**
 * @param {!r5js.Datum} deref Datum to dereference.
 * @return {!r5js.Datum} New Datum capable of dereferencing the given Datum.
 */
r5js.data.newDatumRef = function(deref) {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.REF;
    ans.payload_ = deref;
    return ans;
};


/**
 *
 * @param {!r5js.Macro} macro Macro object.
 * @return {!r5js.Datum} New Datum representing the given macro.
 */
r5js.data.newMacroDatum = function(macro) {
    var ans = new r5js.Datum();
    ans.type_ = r5js.DatumType.MACRO;
    ans.payload_ = macro;
    return ans;
};


/**
 * @param {!r5js.PayloadType} result The result to potentially wrap.
 * @param {!r5js.Type=} opt_type TODO bl
 * @return {r5js.PayloadType} The result, wrapped in a {@link r5js.Datum}
 *         if necessary.
 */
r5js.data.maybeWrapResult = function(result, opt_type) {

    if (result === null
        || result instanceof r5js.Datum
        || result instanceof r5js.Continuation
        || result instanceof r5js.Macro
        || result instanceof r5js.JsObjOrMethod /* JS interop (experimental) */) {
        return result; // no-op, strictly for convenience
    }

    var ans = new r5js.Datum();
    ans.payload_ = result;
    if (goog.isDef(opt_type)) {
        ans.type_ = opt_type;
    } else {
        // If no type was supplied, we can deduce it in most (not all) cases
        switch (typeof result) {
            case 'boolean':
                ans.type_ = r5js.DatumType.BOOLEAN;
                break;
            case 'number':
                ans.type_ = r5js.DatumType.NUMBER;
                break;
            case 'string':
                ans.type_ = r5js.DatumType.IDENTIFIER;
                break;
            case 'object':
                if (result instanceof r5js.Procedure) {
                    ans.type_ = r5js.DatumType.LAMBDA;
                    break;
                }
            default:
                throw new r5js.InternalInterpreterError('cannot deduce type from value '
                    + result + ': noninjective mapping from values to types');
        }
    }
    return ans;
};


