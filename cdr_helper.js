function CdrHelper(head, startOfCdr) {
    this.head = head;
    this.startOfCdr = startOfCdr;
}

CdrHelper.prototype.getCdrHelper = function() {
    return this.head.getCdrHelper();
};

// Basically, call set-car! on the master list.
CdrHelper.prototype.setCar = function(car) {
    this.head.firstChild.nextSibling = car;
};

// Basically, call set-cdr! on the master list.
CdrHelper.prototype.setCdr = function(cdr) {
    this.startOfCdr.nextSibling = cdr;
    if (!cdr.isList())
        this.head.type = '.(';
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



