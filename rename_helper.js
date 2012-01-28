function RenameHelper(parent) {
    this.bindings = {}; // don't use this directly
    this.parent = parent;
}

RenameHelper.prototype.addRenameBinding = function(from) {
    var to = newCpsName();
    this.bindings[from] = to;
    return to;
};

RenameHelper.prototype.getRenameBinding = function(name) {
    var maybe = this.bindings[name];
    if (maybe)
        return maybe;
    else if (this.parent)
        return this.parent.getRenameBinding(name);
    else return null;
};

RenameHelper.prototype.wasUsed = function() {
    for (var name in this.bindings)
        return true;
    return false;
};
