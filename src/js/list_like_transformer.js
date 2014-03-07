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


goog.provide('r5js.ListTransformer');
goog.provide('r5js.ListLikeTransformer');
goog.provide('r5js.QuoteTransformer');
goog.provide('r5js.VectorTransformer');


goog.require('r5js.DatumType');
goog.require('r5js.EllipsisTransformer');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.SiblingBuffer');


/**
 * @param {!r5js.Type} type The type of this transformer.
 * @implements {r5js.ITransformer}
 * @struct
 * @constructor
 */
r5js.ListLikeTransformer = function(type) {
    /** @const {!r5js.Type} */
    this.type = type;

    /** @const @private {!Array.<!r5js.ITransformer>} */
    this.subtransformers_ = [];
};

r5js.ListLikeTransformer.prototype.addSubtransformer = function(subtransformer) {
    this.subtransformers_.push(subtransformer);
    return this;
};


/** @return {string} */
r5js.ListLikeTransformer.prototype.getName = function() {
    return this.subtransformers_[0].datum.getPayload();
};

/** @override */
r5js.ListLikeTransformer.prototype.forEachSubtransformer = function(callback, args) {
        for (var i = 0; i < this.subtransformers_.length; ++i) {
            callback(this.subtransformers_[i], args);
        }
};

/**
 * @param {!r5js.Datum} inputDatum
 * @return {boolean}
 * @private
 */
r5js.ListLikeTransformer.prototype.couldMatch_ = function(inputDatum) {
    switch (this.type) {
        case r5js.DatumType.DOTTED_LIST:
            // Dotted list patterns can match proper or dotted list inputs
            return inputDatum.isList() || inputDatum.isImproperList();
        default:
            throw new r5js.InternalInterpreterError('enum changed');
    }
};

/** @override */
r5js.ListLikeTransformer.prototype.matchInput = function(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
    var len = this.subtransformers_.length;
    var maybeEllipsis = this.subtransformers_[len-1] instanceof r5js.EllipsisTransformer
        && this.subtransformers_[len-1];

    if (!this.couldMatch_(inputDatum))
        return false;

    /* R5RS 4.3.2: "an input form F matches a pattern P if and only if [...]
     - P is a list (P1 ... Pn) and F is a list of n forms match P1 through Pn, respectively; or
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
    for (var subinput = inputDatum.getFirstChild(), i=0;
         subinput;
         subinput = subinput.getNextSibling(), ++i) {

        if (i === len - 1 &&
            (maybeEllipsis || this.type === r5js.DatumType.DOTTED_LIST)) {
            // If there's an ellipsis in the pattern, break out to deal with it.
            break;
        } else if (i >= len) {
            /* If there's no ellipsis in the pattern and the input is longer
             than the pattern, this is a failure. */
            return false;
        } else if (!this.subtransformers_[i].matchInput(subinput, literalIds, definitionEnv, useEnv, bindings)) {
            /* If pattern matching on the subinput and subpattern fails, this is
             a failure. */
            return false;
        }
    }

    if (maybeEllipsis) {
        /* Corner case:
         an empty input like () cannot match a pattern like (x y ...) */
        return (!inputDatum.getFirstChild() && len > 1)
            ? false
            : maybeEllipsis.matchInput(subinput, literalIds, definitionEnv, useEnv, bindings);
    }

    // Dotted-list patterns cannot end in ellipses.
    else if (this.type === r5js.DatumType.DOTTED_LIST) {
        var toMatchAgainst;

        if (inputDatum.isList()) {
            toMatchAgainst = subinput.siblingsToList();
        } else if (inputDatum.isImproperList()) {
            if (subinput.getNextSibling())
                toMatchAgainst = subinput.siblingsToList(true);
            else
                toMatchAgainst = subinput;
        }

        return this.subtransformers_[i].matchInput(toMatchAgainst, literalIds, definitionEnv, useEnv, bindings);
    }

    /* If we matched all of the input without getting through all of
     the pattern, this is a failure. */
    else {
        return i === len;
    }
};

/** @override */
r5js.ListLikeTransformer.prototype.toDatum = function (bindings) {

    var buf = new r5js.SiblingBuffer();
    var len = this.subtransformers_.length;

    for (var i = 0; i < len; ++i) {
        var success = /** @type {!r5js.Datum|boolean} */ (
            this.subtransformers_[i].toDatum(bindings));
        if (success === false) {
            return false;
        } else {
            buf.appendSibling(/** @type {!r5js.Datum} */ (success));
        }
    }

    return buf.toList(this.type);
};


/**
 * @extends {r5js.ListLikeTransformer}
 * @struct
 * @constructor
 */
r5js.QuoteTransformer = function() {
    goog.base(this, r5js.DatumType.QUOTE);
};
goog.inherits(r5js.QuoteTransformer, r5js.ListLikeTransformer);


/**
 * This is a no-op mainly so we don't accidentally rename identifiers inside
 * quotes in {@link r5js.Transformer#setupIds}.
 * @override
 */
r5js.QuoteTransformer.prototype.forEachSubtransformer = goog.nullFunction;


/**
 * @extends {r5js.ListLikeTransformer}
 * @struct
 * @constructor
 */
r5js.VectorTransformer = function() {
    goog.base(this, r5js.DatumType.VECTOR);
};
goog.inherits(r5js.VectorTransformer, r5js.ListLikeTransformer);


/** @override */
r5js.VectorTransformer.prototype.couldMatch_ = function(inputDatum) {
    // Vector patterns match only vector inputs
    return inputDatum.isVector();
};


/**
 * @extends {r5js.ListLikeTransformer}
 * @struct
 * @constructor
 */
r5js.ListTransformer = function() {
  goog.base(this, r5js.DatumType.LIST);
};
goog.inherits(r5js.ListTransformer, r5js.ListLikeTransformer);


/** @override */
r5js.ListTransformer.prototype.couldMatch_ = function(inputDatum) {
    // Proper list patterns can match only proper list inputs
    return inputDatum.isList();
};