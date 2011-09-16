function Datum() {
    /* No need to set this stuff until it's needed, just here for documentation
     this.firstChild = null;
     this.nextSibling = null;
     this.type = null;
     this.payload = null;
     this.nonterminals = [];*/
}

Datum.prototype.setParse = function(type) {
    if (!this.nonterminals)
        this.nonterminals = [];
    this.nonterminals.push(type);
};

Datum.prototype.unsetParse = function() {
    this.nonterminals = null;
    for (var child = this.firstChild; child; child = child.nextSibling)
        child.unsetParse();
};

Datum.prototype.sanitize = function() {
    this.parent = null;
    this.unsetParse();
    return this;
};

Datum.prototype.getParse = function() {
    return this.nonterminals.pop();
};

Datum.prototype.peekParse = function() {
    if (this.nonterminals) {
        var len = this.nonterminals.length;
        if (len > 0)
            return this.nonterminals[len - 1];
    }

    return null;
};

Datum.prototype.matchChild = function(predicate) {
    for (var child = this.firstChild; child; child = child.nextSibling)
        if (predicate(child))
            return child;
    return null;
};

Datum.prototype.childWithParse = function(type) {
  return this.matchChild(function(datum) { return datum.peekParse() === type; });
};

Datum.prototype.appendSibling = function(sibling) {
    if (!this.nextSibling) {
        if (this.parent) {
            // Propagate the parent field
            sibling.parent = this.parent;
            // Only the last sibling needs a link back to the parent
            this.parent = null;
        }
        this.nextSibling = sibling;
    }
    else
        this.nextSibling.appendSibling(sibling);
};

/* If we used this to append n children in a row, it would take time O(n^2).
 But we don't actually use it like that. When building a list like (X*), we build
 up the list of X's in linear time, then call appendChild once to append the
 whole list as a child of the list root. We do incur some overhead when building
 a list like (X+ . X): in this case, the X+ list is appended in one go, and then
 we have to re-traverse that list once to append the final X. I expect this to be
 rare enough not to matter in practice, but if necessary we could keep track of
 the root's final child. */
Datum.prototype.appendChild = function(child) {
    if (!this.firstChild)
        this.firstChild = child;
    else this.firstChild.appendSibling(child);
};

Datum.prototype.collectPayloads = function(ansBuffer) {
    if (!ansBuffer)
        ansBuffer = [];
    if (this.payload) {
        ansBuffer.push(this.payload);
    }
    else {
        for (var child = this.firstChild; child; child = child.nextSibling)
            child.collectPayloads(ansBuffer);
    }
    return ansBuffer;
};

// Convenience function
Datum.prototype.isImproperList = function() {
    return this.type === '.(';
};