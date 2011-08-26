function SchemePair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
} // bl do we need this as a primitive?

function listToArray(list) {
    var ans = [];
    while (list instanceof SchemePair) {
        ans.push(list.car);
        list = list.cdr;
    }
    return ans;
}

function arrayToList(array) {
    var cur = null;
    var prev;
    for (var i = array.length - 1; i >= 0; i--) {
        prev = new SchemePair(array[i], cur);
        cur = prev;
    }
    return cur;
}

function SchemeChar(c) {
    this.c = c;
}

SchemeChar.prototype.toString = function() {
    return this.c;
};

function SchemeString(s) {
    this.s = s;
}

SchemeString.prototype.toString = function() {
    return this.s;
};

function SchemeProcedure(requiredFormals, maybeDottedFormal, env, body) {
    this.requiredFormals = requiredFormals;
    this.maybeDottedFormal = maybeDottedFormal;
    this.env = shallowCopy(env);
    this.body = body;
}

SchemeProcedure.prototype.toString = function() {
    return "[procedure]"; // bl lame
};

SchemeProcedure.prototype.checkNumArgs = function(numActuals) {
    if (numActuals < this.requiredFormals.length)
        throw new TooFewArgs(this.toString(), this.requiredFormals.length, numActuals);
    if (!this.maybeDottedFormal && numActuals > this.requiredFormals.length)
        throw new IncorrectNumArgs(this.toString(), this.requiredFormals.length, numActuals);
};

SchemeProcedure.prototype.bindArgs = function(args) {
    var envCopy = shallowCopy(this.env);
    for (var i = 0; i < this.requiredFormals.length; ++i)
        envCopy[this.requiredFormals[i]] = args[i];
    if (this.maybeDottedFormal)
        envCopy[this.maybeDottedFormal] = arrayToList(args.slice(this.requiredFormals.length, args.length));
    return envCopy;
};

function shallowCopy(hash) {
    var ans = {};
    for (var k in hash)
        ans[k] = hash[k];
    return ans;
}

function SchemePort(portno) {
    this.portno = portno;
}

SchemePort.prototype.toString = function() {
    return "[Port " + this.portno + "]";
};

function SchemeDatum(tree, programText) {
    this.tree = tree;
    this.programText = programText;
    this.datumText = null;
}

SchemeDatum.prototype.toString = function() {
    return this.datumText || (this.datumText = recoverText(this.tree, this.programText));
};