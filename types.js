function SchemePair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
} // bl do we need this as a primitive?

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
        var cur, prev, first;
        for (var i = this.formalsArray.length - 1; i < args.length; ++i) {
            if (!first) {
                first = args[i];
                prev = first;
            } else {
                cur = args[i];
                prev.appendSibling(cur);
                prev = cur;
            }
        }
        var remainder = new Datum();
        remainder.type = '(';
        remainder.appendChild(first);

        // todo bl I'm quite sure this isn't right...

        envCopy[this.formalsArray[this.formalsArray.length - 1]] = remainder;
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

