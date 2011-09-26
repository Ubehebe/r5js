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

function SchemeProcedure(formalsArray, isDotted, bodyStart, env) {
    this.formalsArray = formalsArray;
    this.isDotted = isDotted;
    this.body = bodyStart;
    this.env = shallowCopy(env);
}

SchemeProcedure.prototype.toString = function() {
    return "[procedure]"; // bl lame
};

SchemeProcedure.prototype.checkNumArgs = function(numActuals) {

    if (!this.isDotted) {
        if (numActuals !== this.formalsArray.length)
            throw new IncorrectNumArgs(this.toString(), this.formalsArray.length, numActuals);
    } else {
        var minNumArgs = this.formalsArray.length - 1;
        if (numActuals < minNumArgs)
            throw new TooFewArgs(this.toString(), minNumArgs, numActuals);
    }
};

SchemeProcedure.prototype.bindArgs = function(args) {
    var envCopy = shallowCopy(this.env);

    for (var i = 0; i < this.formalsArray.length - 1; ++i)
        envCopy[this.formalsArray[i]] = args[i];

    if (!this.isDotted) {
        envCopy[this.formalsArray[this.formalsArray.length - 1]]
            = args[this.formalsArray.length - 1];
    } else {
        // Roll up the remaining arguments into a list
        var list = newEmptyList();
        // Go backwards and do prepends to avoid quadratic performance
        for (var i = args.length - 1; i >= this.formalsArray.length - 1; --i)
            list.prependChild(args[i]);
        envCopy[this.formalsArray[this.formalsArray.length - 1]] = list;
    }
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

