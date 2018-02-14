import {newCpsName} from '../parse/rename_util';

export class RenameHelper {
    private readonly bindings_: {[key: string]: string} = {};

    constructor(private readonly parent_: RenameHelper|null=null) {}

    /**
     * @param from Name to add a renaming for.
     * @return A new name for the given name.
     */
    addRenameBinding(from: string): string {
        const to = newCpsName();
        this.bindings_[from] = to;
        return to;
    }

    /**
     * @param name Name to look up rename binding for.
     * @return The renaming of this name, or null if this object has no such binding.
     */
    getRenameBinding(name: string): string|null {
        const maybe = this.bindings_[name];
        if (maybe) {
            return maybe;
        } else if (this.parent_) {
            return this.parent_.getRenameBinding(name);
        } else {
            return null;
        }
    }

    /** @return True iff the helper was used. */
    wasUsed(): boolean {
        for (const name in this.bindings_) {
            return true;
        }
        return false;
    }
}
