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


goog.provide('r5js.tmp.list_like_transformer');


goog.require('r5js.InternalInterpreterError');

/* ListLikeTransformer, EllipsisTransformer, and IdOrLiteralTransformer
all "implement" the following "interface":

{
    forEachSubtransformer: function(callback, args) { ... },
    matchInput: function(inputDatum, literalIds, definitionEnv, useEnv, bindings) { ... },
    toDatum: function(bindings) { ... }
}

In fact, they all used to have a common prototypal ancestor. But since
they didn't share any implementations of these functions, nor any common state,
all the ancestor did was throw "pure virtual" exceptions if a function
hadn't been overridden. In view of this lack of usefulness, I got rid of the
"superclass". */
function ListLikeTransformer(type) {
    this.type = type;
    this.subtransformers = [];
}

ListLikeTransformer.prototype.addSubtransformer = function(subtransformer) {
    this.subtransformers.push(subtransformer);
    return this;
};

ListLikeTransformer.prototype.forEachSubtransformer = function(callback, args) {
    /* This is a no-op mainly so we don't accidentally rename identifiers
     inside quotes in Transformer.prototype.setupIds. */
    if (this.type !== "'") {
        for (var i = 0; i < this.subtransformers.length; ++i)
            callback(this.subtransformers[i], args);
    }
};

ListLikeTransformer.prototype.couldMatch = function(inputDatum) {
    switch (this.type) {
        case '(':
            // Proper list patterns can match only proper list inputs
            return inputDatum.isList();
        case '.(':
            // Dotted list patterns can match proper or dotted list inputs
            return inputDatum.isList() || inputDatum.isImproperList();
        case '#(':
            // Vector patterns match only vector inputs
            return inputDatum.isVector();
        default:
            throw new r5js.InternalInterpreterError('enum changed');
    }
};

ListLikeTransformer.prototype.matchInput = function(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
    var len = this.subtransformers.length;
    var maybeEllipsis = this.subtransformers[len-1] instanceof EllipsisTransformer
        && this.subtransformers[len-1];

    if (!this.couldMatch(inputDatum))
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
    for (var subinput = inputDatum.firstChild, i=0;
         subinput;
         subinput = subinput.nextSibling, ++i) {

        // If there's an ellipsis in the pattern, break out to deal with it.
        if (i === len - 1 && (maybeEllipsis || this.type === '.('))
            break;

        /* If there's no ellipsis in the pattern and the input is longer
         than the pattern, this is a failure. */
        else if (i >= len)
            return false;

        /* If pattern matching on the subinput and subpattern fails, this is
         a failure. */
        else if (!this.subtransformers[i].matchInput(subinput, literalIds, definitionEnv, useEnv, bindings))
            return false;
    }

    if (maybeEllipsis) {
        /* Corner case:
         an empty input like () cannot match a pattern like (x y ...) */
        return (!inputDatum.firstChild && len > 1)
            ? false
            : maybeEllipsis.matchInput(subinput, literalIds, definitionEnv, useEnv, bindings);
    }

    // Dotted-list patterns cannot end in ellipses.
    else if (this.type === '.(') {
        var toMatchAgainst;

        if (inputDatum.isList()) {
            toMatchAgainst = subinput.siblingsToList();
        } else if (inputDatum.isImproperList()) {
            if (subinput.nextSibling)
                toMatchAgainst = subinput.siblingsToList(true);
            else
                toMatchAgainst = subinput;
        }

        return this.subtransformers[i].matchInput(toMatchAgainst, literalIds, definitionEnv, useEnv, bindings);
    }

    /* If we matched all of the input without getting through all of
     the pattern, this is a failure. */
    else {
        return i === len;
    }
};

ListLikeTransformer.prototype.toDatum = function (bindings) {

    var buf = new SiblingBuffer();
    var len = this.subtransformers.length;
    var success;

    for (var i = 0; i < len; ++i) {
        success = this.subtransformers[i].toDatum(bindings);
        if (success === false)
            return false;
        else
            buf.appendSibling(success);
    }

    return buf.toList(this.type);
};

ListLikeTransformer.prototype.toString = function () {
    var ans = this.type === '#(' ? this.type : '(';
    if (this.subtransformers.length === 0) {
        return ans + ')';
    } else {
        for (var i = 0; i < this.subtransformers.length - 1; ++i)
            ans += this.subtransformers[i].toString() + ' ';
        if (this.type === '.(')
            ans += '. ';
        return ans + this.subtransformers[i].toString() + ')';
    }
};