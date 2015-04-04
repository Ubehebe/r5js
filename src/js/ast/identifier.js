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
};
