goog.module('r5js.RenameHelper');

const RenameUtil = goog.require('r5js.RenameUtil');

class RenameHelper {
    /** @param {?RenameHelper} parent The parent helper, if any. */
    constructor(parent) {
        /** @const @private {!Object<string, string>} */ this.bindings_ = {};
        /** @const @private */ this.parent_ = parent;
    }

    /**
     * @param {string} from Name to add a renaming for.
     * @return {string} A new name for the given name.
     */
    addRenameBinding(from) {
        const to = RenameUtil.newCpsName();
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
}

exports = RenameHelper;