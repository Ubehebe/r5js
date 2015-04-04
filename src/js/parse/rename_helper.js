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


goog.provide('r5js.RenameHelper');



/**
 * @struct
 * @constructor
 */
r5js.RenameHelper = class {

    /**
     * @param {r5js.RenameHelper} parent The parent helper, if any.
     */
    constructor(parent) {
        /** @const @private {!Object<string, string>} */ this.bindings_ = {};
        /** @const @private */ this.parent_ = parent;
    }

    /**
     * @param {string} from Name to add a renaming for.
     * @return {string} A new name for the given name.
     */
    addRenameBinding(from) {
        const to = newCpsName();
        this.bindings_[from] = to;
        return to;
    }

    /**
     * @param {string} name Name to look up rename binding for.
     * @return {?string} The renaming of this name, or null if this object
     * has no such binding.
     */
    getRenameBinding(name) {
        const maybe = this.bindings_[name];
        if (maybe) {
            return maybe;
        } else if (this.parent_) {
            return this.parent_.getRenameBinding(name);
        } else {
            return null;
        }
    }

    /** @return {boolean} True iff the helper was used. */
    wasUsed() {
        for (const name in this.bindings_) {
            return true;
        }
        return false;
    }
};