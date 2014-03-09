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


goog.provide('r5js.IdOrLiteralTransformer');


goog.require('r5js.OutputMode');


/**
 * @param {!r5js.Datum} datum
 * @implements {r5js.ITransformer}
 * @struct
 * @constructor
 */
r5js.IdOrLiteralTransformer = function(datum) {
    /** @const @private {!r5js.Datum} */
    this.datum_ = datum;
};

/** @override */
r5js.IdOrLiteralTransformer.prototype.matchInput = function(
    inputDatum, literalIds, definitionEnv, useEnv, bindings) {
    if (this.datum_.isIdentifier()) {
        /* R5RS 4.3.2: "A subform in the input matches a literal identifier
         if and only if it is an identifier and either both its occurrence
         in the macro expression and its occurrence in the macro definition
         have the same lexical binding, or the two identifiers are equal
         and both have no lexical binding." */
        if (literalIds[this.datum_.getPayload()]) {
            if (inputDatum.isIdentifier()) {
                var name = inputDatum.getPayload();
                // Both have no lexical binding
                if (name === this.datum_.getPayload()
                    && (!definitionEnv.hasBindingRecursive(name, false)
                    && !useEnv.hasBindingRecursive(name, false))) {
                    bindings.addTemplateBinding(name, inputDatum);
                    return true;
                } else if (definitionEnv.get(name) === useEnv.get(name)) {
                    bindings.addTemplateBinding(name, inputDatum);
                    return true;
                } else return false;
            } else return false;
        }
        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a non-literal identifier [...]".
         That is, non-literal identifiers match anything. */
        else {
            bindings.addTemplateBinding(this.datum_.getPayload(), inputDatum);
            return true;
        }
    } else {
        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a datum and F is equal to P in the sense of the equal?
         procedure." */
        return inputDatum.isEqual(this.datum_);
    }
};

/** @override */
r5js.IdOrLiteralTransformer.prototype.toDatum = function(bindings) {
    return bindings.resolveDatum(this.datum_);
};

r5js.IdOrLiteralTransformer.prototype.toString = function() {
    return this.datum_.stringForOutputMode(r5js.OutputMode.DISPLAY);
};


/** @return {!r5js.Datum} */
r5js.IdOrLiteralTransformer.prototype.getDatum = function() {
    return this.datum_;
};

/**
 * @param {function(this: T, !r5js.ITransformer, number)} callback
 * @param {number} ellipsisLevel
 * @param {T=} opt_context
 * @template T
 * @override
 * TODO bl why is it necessary to repeat the params from the interface?
 */
r5js.IdOrLiteralTransformer.prototype.forEachSubtransformer = function(
    callback, ellipsisLevel, opt_context) {
    callback.call(opt_context, this, ellipsisLevel);
};

