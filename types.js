function SchemePair(car, cdr) {
    this.car = car;
    this.cdr = cdr;
} // bl do we need this as a primitive?

// todo bl do we need this type?
function EmptyList() {}
EmptyList.prototype.toString = function() { return '()'; };
var emptyList = new EmptyList();

SchemePair.prototype.toString = function() {
    // Feels dirty doing iteration on cars and cdrs
    var buf = [];
    for (var cur = this; cur; cur = cur.cdr)
        buf.push(cur.car.toString());
    return '(' + buf.join(' ') + ')';
};

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
            cur = new SchemePair(args[i], null);
            if (!first)
                first = cur;
            else
                prev.cdr = cur;

            prev = cur;
        }
        envCopy[this.formalsArray[this.formalsArray.length - 1]] = first ? first : emptyList;
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

