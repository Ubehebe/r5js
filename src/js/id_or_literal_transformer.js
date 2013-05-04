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


goog.provide('r5js.tmp.id_or_literal_transformer');

// See comments at top of ListLikeTransformer.
function IdOrLiteralTransformer(datum) {
    this.datum = datum;
}

IdOrLiteralTransformer.prototype.matchInput = function(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
    if (this.datum.isIdentifier()) {
        /* R5RS 4.3.2: "A subform in the input matches a literal identifier
         if and only if it is an identifier and either both its occurrence
         in the macro expression and its occurrence in the macro definition
         have the same lexical binding, or the two identifiers are equal
         and both have no lexical binding." */
        if (literalIds[this.datum.payload]) {
            if (inputDatum.isIdentifier()) {
                var name = inputDatum.payload;
                // Both have no lexical binding
                if (name === this.datum.payload
                    && (!definitionEnv.hasBindingRecursive(name)
                    && !useEnv.hasBindingRecursive(name))) {
                    bindings.addTemplateBinding(name, inputDatum);
                    return true;
                } else if (definitionEnv.get(name, true) === useEnv.get(name, true)) {
                    bindings.addTemplateBinding(name, inputDatum);
                    return true;
                } else return false;
            } else return false;
        }
        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a non-literal identifier [...]".
         That is, non-literal identifiers match anything. */
        else {
            bindings.addTemplateBinding(this.datum.payload, inputDatum);
            return true;
        }
    } else {
        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a datum and F is equal to P in the sense of the equal?
         procedure." */
        return inputDatum.isEqual(this.datum);
    }
};

IdOrLiteralTransformer.prototype.toDatum = function(bindings) {
    return bindings.resolveDatum(this.datum);
};

IdOrLiteralTransformer.prototype.toString = function() {
    return this.datum.toString();
};

IdOrLiteralTransformer.prototype.forEachSubtransformer = function(callback, args) {
    callback(this, args);
};

