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
goog.provide('r5js.DottedList');
goog.provide('r5js.Lambda');
goog.provide('r5js.List');
goog.provide('r5js.Quasiquote');
goog.provide('r5js.Ref');
goog.provide('r5js.Unquote');
goog.provide('r5js.UnquoteSplicing');


goog.require('r5js.ast.Node');
goog.require('r5js.Continuable');
goog.require('r5js.ContinuableHelper');
goog.require('r5js.DatumType');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.OutputMode')
goog.require('r5js.RenameHelper');
goog.require('r5js.SiblingBuffer');

// TODO bl circular dependency goog.require('r5js.ast.Identifier');
// TODO bl circular dependency goog.require('r5js.ast.Quote');

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
    if (!(this instanceof r5js.ast.Quote)) {
        callback(this);
        for (var cur = this.firstChild_; cur; cur = cur.nextSibling_) {
            cur.forEach(callback);
        }
    }
};


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
    return this.qqLevel_;
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
            case r5js.parse.Terminals.LPAREN: // TODO bl
            case r5js.parse.Terminals.LPAREN_DOT:
            case r5js.parse.Terminals.LPAREN_VECTOR:
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


/**
 * @return {boolean} True iff this Datum represents an empty list.
 */
r5js.Datum.prototype.isEmptyList = function() {
    return this instanceof r5js.List && !this.firstChild_;
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

    var ans = new this.constructor();

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

/** @return {boolean} True if this datum represents an improper list. */
r5js.Datum.prototype.isImproperList = function() {
    return this.type_ === r5js.parse.Terminals.LPAREN_DOT;
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
 * @return {boolean} Whether this datum is a quotation that needs to be
 * normalized.
 */
r5js.Datum.prototype.isNonNormalizedQuotation = function() {
        return this instanceof r5js.List &&
            !!this.firstChild_ &&
            this.firstChild_.payload_ === r5js.parse.Terminals.QUOTE;
};


/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.Unquote = function(firstChild) {
    goog.base(this);
    this.type_ = r5js.parse.Terminals.COMMA;
    if (firstChild) {
        this.firstChild_ = firstChild;
    }
};
goog.inherits(r5js.Unquote, r5js.Datum);

/** @override */
r5js.Unquote.prototype.stringForOutputMode = function(outputMode) {
    var children = this.mapChildren(function(child) {
        return child.stringForOutputMode(outputMode);
    });
    return r5js.parse.Terminals.COMMA + children.join(' ');
};


/** @override */
r5js.Unquote.prototype.setQuasiquotationLevel = function(qqLevel) {
        this.qqLevel_ = qqLevel;
        return goog.base(this, 'setQuasiquotationLevel', qqLevel-1);
};



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.UnquoteSplicing = function(firstChild) {
  goog.base(this);
    this.type_ = r5js.parse.Terminals.COMMA_AT;
    if (firstChild) {
        this.firstChild_ = firstChild;
    }
};
goog.inherits(r5js.UnquoteSplicing, r5js.Datum);


/** @override */
r5js.UnquoteSplicing.prototype.stringForOutputMode = function(outputMode) {
    var children = this.mapChildren(function(child) {
        return child.stringForOutputMode(outputMode);
    });
    return r5js.parse.Terminals.COMMA_AT + children.join(' ');
};

/** @override */
r5js.UnquoteSplicing.prototype.setQuasiquotationLevel = function(qqLevel) {
    this.qqLevel_ = qqLevel;
    return goog.base(this, 'setQuasiquotationLevel', qqLevel-1);
};



/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.Quasiquote = function(firstChild) {
    goog.base(this);
    this.type_ = r5js.parse.Terminals.BACKTICK;
    if (firstChild) {
        this.firstChild_ = firstChild;
    }
};
goog.inherits(r5js.Quasiquote, r5js.Datum);


/** @override */
r5js.Quasiquote.prototype.stringForOutputMode = function(outputMode) {
    var children = this.mapChildren(function(child) {
        return child.stringForOutputMode(outputMode);
    });
    return r5js.parse.Terminals.BACKTICK + children.join(' ');
};

/**
 * Example: `(1 ,(+ 2 3)) should desugar as (+ 2 3 [_0 (id (1 _0) [_2 ...])])
 * @param {!r5js.IEnvironment} env TODO bl.
 * @param {string} cpsName TODO bl.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum when called.
 * @return {*} TODO bl.
 * @suppress {const} for the assignment to continuation.lastResultName,
 * which may indicate a bug. TODO bl investigate.
 */
r5js.Quasiquote.prototype.processQuasiquote = function(
    env, cpsName, parserProvider) {

    var newCalls = new r5js.ContinuableHelper();

    var qqLevel = this.getQQLevel();

    this.replaceChildren(
        function(node) {
            return (node instanceof r5js.Unquote ||
                node instanceof r5js.UnquoteSplicing) &&
                node.getQQLevel() === qqLevel;
        },
        function(node) {
            var asContinuable = /** @type {!r5js.Continuable} */ (parserProvider(
                /** @type {!r5js.Datum} */(node.getFirstChild())).
                parse(r5js.parse.Nonterminals.EXPRESSION).
                desugar(env, true));
            var continuation = asContinuable.getLastContinuable().continuation;
            /* Throw out the last result name and replace it with another
             identifier (also illegal in Scheme) that will let us know if it's
             unquotation or unquotation with splicing. */
            continuation.lastResultName = node.getType() + '' + goog.getUid(new Object());
            newCalls.appendContinuable(asContinuable);
            return new r5js.ast.Identifier(continuation.lastResultName);
        });

    var newDatum = new r5js.ast.Quote(this.firstChild_);

    newCalls.appendContinuable(newIdShim(newDatum, cpsName));
    var ans = newCalls.toContinuable();
    return ans && ans.setStartingEnv(env);
};


/** @override */
r5js.Quasiquote.prototype.setQuasiquotationLevel = function(qqLevel) {
            this.qqLevel_ = qqLevel+1;
    return goog.base(this, 'setQuasiquotationLevel', this.qqLevel_);
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
 * @param {string} name Name of the procedure.
 * @param {!r5js.PrimitiveProcedure|!r5js.Procedure} procedure TODO bl.
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.Lambda = function(name, procedure) {
    goog.base(this);
    this.type_ = r5js.parse.Terminals.LAMBDA;
    this.payload_ = procedure;

    /** @const @private {string} */
    this.name_ = name;
};
goog.inherits(r5js.Lambda, r5js.Datum);


/** @return {string} */
r5js.Lambda.prototype.getName = function() {
    return this.name_;
};


/** @override */
r5js.Lambda.prototype.stringForOutputMode = function(outputMode) {
    return r5js.PrimitiveProcedure.isImplementedBy(
        this.getPayload()) ? this.name_ :
        'proc:' + this.getPayload().name;
};


/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.List = function(firstChild) {
  goog.base(this);
    this.type_ = r5js.parse.Terminals.LPAREN;
    if (firstChild) {
        this.firstChild_ = firstChild;
    }
    /** @private {boolean} */
    this.dirty_ = false;
};
goog.inherits(r5js.List, r5js.Datum);


/** Marks dirty. */
r5js.List.prototype.markDirty = function() {
    this.dirty_ = true;
};


/** @return {boolean} */
r5js.List.prototype.isDirty = function() {
    return this.dirty_;
};


/** @override */
r5js.List.prototype.stringForOutputMode = function(outputMode) {
    /* Note: this will be an infinite loop for cyclical data
     structures created by the programmer through set-cdr!, etc.
     Some implementations do nice things, like print "holes" where
     a cycle starts. But the R5RS standard does not seem to define
     external representations for lists (vectors, etc.) that contain
     cycles. In general, the spirit of the standard seems to be that
     the programmer is responsible for mayhem caused by the creation
     of such structures.

     There is one exception: list? (a library procedure) must return
     false for cyclical lists. Accordingly, I've written the
     cycle-detecting logic wholly in Scheme, not bothering
     to reimplement it here. */
    var children = this.mapChildren(function(child) {
        return child.stringForOutputMode(outputMode);
    });
    return this.getType() + children.join(' ') + ')';
};


/**
 * @param {r5js.Datum} firstChild
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.DottedList = function(firstChild) {
    goog.base(this);
    this.type_ = r5js.parse.Terminals.LPAREN_DOT;
    if (firstChild) {
        this.firstChild_ = firstChild;
    }
};
goog.inherits(r5js.DottedList, r5js.Datum);


/** @override */
r5js.DottedList.prototype.stringForOutputMode = function(outputMode) {
    var children = this.mapChildren(function(child) {
        return child.stringForOutputMode(outputMode);
    });
    // Insert the dot at the next-to-last location.
    children.splice(-1, 0, r5js.parse.Terminals.DOT);
    return r5js.parse.Terminals.LPAREN +
        children.join(' ') +
        r5js.parse.Terminals.RPAREN;
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
 * @return {!r5js.Datum} The normalized quotation datum.
 * @private
 * TODO bl: remove the type switch in favor of overrides on the subclasses.
 */
r5js.Datum.prototype.normalizeQuotation_ = function() {
    if (!this.firstChild_) {
        return this;
    }
        switch (this.firstChild_.payload_) {
            case r5js.parse.Terminals.QUOTE:
                return new r5js.ast.Quote(this.firstChild_.nextSibling_);
            case r5js.parse.Terminals.QUASIQUOTE:
                return new r5js.Quasiquote(this.firstChild_.nextSibling_);
            case r5js.parse.Terminals.UNQUOTE:
                return new r5js.Unquote(this.firstChild_.nextSibling_);
            case r5js.parse.Terminals.UNQUOTE_SPLICING:
                return new r5js.UnquoteSplicing(this.firstChild_.nextSibling_);
            default:
                return this;
        }
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
 * @return {!r5js.Datum} The normalized datum.
 */
r5js.Datum.prototype.normalizeInput = function() {
    var thisNormalized = this.normalizeQuotation_();

    for (var cur = thisNormalized.getFirstChild(), prev;
         cur;
         prev = cur, cur = cur.getNextSibling()) {
        var curNormalized = cur.normalizeInput();
        if (curNormalized !== cur) {
            if (prev) {
                prev.nextSibling_ = curNormalized;
            } else {
                thisNormalized.firstChild_ = curNormalized;
            }
            curNormalized.nextSibling_ = cur.nextSibling_;
        }
        cur = curNormalized;
    }

    return thisNormalized.setImmutableOnQuote();
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
    return dotted ? new r5js.DottedList(this) : new r5js.List(this);
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
 * Munges definitions to get them in a form suitable for let-type bindings.
 * Example:
 * (define (foo x y z) ...) => (foo (lambda (x y z) ...))
 * @return {!r5js.Datum} New Datum representing this datum's definition.
 */
r5js.Datum.prototype.extractDefinition = function() {
    var variable = this.at(r5js.parse.Nonterminals.VARIABLE);
    if (variable) {
        var expr = this.at(r5js.parse.Nonterminals.EXPRESSION);
        variable.nextSibling_ = null; // TODO bl
        return new r5js.SiblingBuffer().
            appendSibling(variable).
            appendSibling(/** @type {!r5js.Datum} */(expr)).
            toList();
    } else {
        var formalsList = this.firstChild_.nextSibling_;
        variable = formalsList.firstChild_;
        var bodyStart = formalsList.nextSibling_;
        formalsList.firstChild_ = formalsList.firstChild_.nextSibling_;
        var lambda = r5js.Datum.prepareLambdaForDefinition_(bodyStart, formalsList);
        variable.nextSibling_ = null; // TODO bl
        return new r5js.SiblingBuffer().
            appendSibling(variable).
            appendSibling(lambda).
            toList();
    }
};


/**
 * @param bodyStart
 * @param {!r5js.Datum} formalsList
 * @return {!r5js.Datum}
 * @private
 */
r5js.Datum.prepareLambdaForDefinition_ = function(bodyStart, formalsList) {
    var buffer = new r5js.SiblingBuffer();
    buffer.appendSibling(new r5js.ast.Identifier(r5js.parse.Terminals.LAMBDA));
    if (formalsList.isImproperList() && !formalsList.firstChild_.nextSibling_) {
        buffer.appendSibling(new r5js.ast.Identifier(
            /** @type {string} */ (formalsList.firstChild_.payload_)));
    } else {
        formalsList.nextSibling_ = null;
        buffer.appendSibling(formalsList);
    }
    buffer.appendSibling(bodyStart);
    return buffer.toList();
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
    } else if (this instanceof r5js.ast.Quote) {
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
 * @param {!r5js.Datum} deref Datum to dereference.
 * @extends {r5js.Datum}
 * @struct
 * @constructor
 */
r5js.Ref = function(deref) {
  goog.base(this);
    this.payload_ = deref;
};
goog.inherits(r5js.Ref, r5js.Datum);


/** @return {!r5js.Datum} */
r5js.Ref.prototype.deref = function() {
    return /** @type {!r5js.Datum} */ (this.payload_);
};


/** @override */
r5js.Ref.prototype.stringForOutputMode = function(outputMode) {
    return this.getPayload().stringForOutputMode(outputMode);
};


/**
 * @extends {r5js.ast.Literal}
 * @struct
 * @constructor
 */
r5js.ast.SimpleDatum = function() {
  goog.base(this);
};
goog.inherits(r5js.ast.SimpleDatum, r5js.ast.Literal);

