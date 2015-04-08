/* Copyright 2011-2014 Brendan Linn

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

goog.provide('r5js.ast.Identifier');


goog.require('r5js.Datum');
goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.parse.Terminals');
goog.require('r5js.ProcCallLike');



r5js.ast.Identifier = /** @extends {r5js.ast.SimpleDatum<string>} */ class extends r5js.ast.SimpleDatum {
    /** @param {string} name */
    constructor(name) {
        super(name);
    }

    /**
     * @return {boolean} True iff this Datum is in a quasiquotation and should be
     * unquoted (i.e. starts with a ,).
     */
    shouldUnquote() {
        return this.payload.charAt(0) === r5js.parse.Terminals.COMMA;
    }

    /**
     * This is a subcase of shouldUnquote, because unquotes and unquote-splicings
     * have pretty much the same logic.
     * @return {boolean} TODO bl.
     * @suppress {accessControls} for r5js.Datum.CPS_PREFIX_
     */
    shouldUnquoteSplice() {
        return this.payload.charAt(1) === r5js.Datum.CPS_PREFIX_;
    }

    /** @override */
    fixParserSensitiveIds(helper) {
        if (isParserSensitiveId(this.payload)) {
            const renamedAs = helper.getRenameBinding(this.payload);
            if (renamedAs) {
                this.setPayload(renamedAs);
            }
        }
    }

    /** @override */
    toProcCallLike() {
        return new r5js.IdShim_(this);
    }
};

/**
 * If a nonterminal in the grammar has no associated desugar function,
 * desugaring it will be a no-op. That is often the right behavior,
 * but sometimes we would like to wrap the datum in a Continuable
 * object for convenience on the trampoline. For example, the program
 * "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]).
 *
 * We represent these id shims as ProcCalls whose operatorNames are null
 * and whose firstOperand is the payload.
 * @param {!r5js.ast.Identifier} payload
 * @param {string=} opt_continuationName Optional name of the continuation.
 * @extends {r5js.ProcCallLike}
 * @struct
 * @constructor
 * @private
 */
r5js.IdShim_ = function(payload, opt_continuationName) {
    r5js.IdShim_.base(this, 'constructor', opt_continuationName);
    /** @const @private */ this.firstOperand_ = payload;
};
goog.inherits(r5js.IdShim_, r5js.ProcCallLike);


/** @override */
r5js.IdShim_.prototype.evalAndAdvance = function(
    resultStruct, env, parserProvider) {
    const ans = this.tryIdentifier_(this.firstOperand_);

    if (ans !== null) {
        this.bindResult(ans);
        resultStruct.setValue(ans);
    }

    const nextContinuable = this.getNext();

    if (nextContinuable) {
        resultStruct.setNext(nextContinuable);
    }
};


/**
 * @param {!r5js.ast.Identifier} id
 * @return {?r5js.runtime.Value}
 * @private
 */
r5js.IdShim_.prototype.tryIdentifier_ = function(id) {
    return this.getEnv().get(/** @type {string} */ (id.getPayload()));
};