/* See the comment to Datum.prototype.siblingsToList for an explanation
 of what this class does. */
function CdrHelper(head, startOfCdr) {
    this.head = head;
    this.startOfCdr = startOfCdr;
}

CdrHelper.prototype.getCdrHelper = function() {
    /* todo bl: this used to return this.head.getCdrHelper(), but
     I am not sure that's necessary. */
    return null;
};

// Basically, call set-car! on the master list.
CdrHelper.prototype.setCar = function(car) {
    if (this.head.isImmutable())
        throw new ImmutableError(this.head.toString());
    this.head.firstChild.nextSibling = car;
};

// Basically, call set-cdr! on the master list.
CdrHelper.prototype.setCdr = function(cdr) {
    if (this.head.isImmutable())
        throw new ImmutableError(this.head.toString());
    this.startOfCdr.nextSibling = cdr;
    if (!cdr.isList()) {
        var cur = this;
        do {
            cur.head.type = '.(';
        } while (cur = cur.head.getCdrHelper());
    }
};

/* Two CdrHelpers are equal iff they point to the same list and have
 the same offset. */
CdrHelper.prototype.equals = function(cdrHelper) {
    return this.head === cdrHelper.head
        && this.startOfCdr === cdrHelper.startOfCdr;
};

/* A CdrHelper resolves to a given Datum iff it points to that list
and its offset is that list's first child. */
CdrHelper.prototype.resolvesTo = function(datum) {
    if (!datum)
        return false;
    else if (this.head === datum)
        return this.startOfCdr === datum.firstChild;
    else
        return false;
};



