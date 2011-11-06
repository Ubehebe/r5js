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

function SchemeProcedure(formalsArray, isDotted, bodyStart, env, name) {
    this.formalsArray = formalsArray;
    this.isDotted = isDotted;
    this.env = shallowHashCopy(env);

    if (bodyStart) {
        this.body = bodyStart.seqThrowawayAllButLast(this.env);
        /* todo bl make a Continuable class so we can say
         continuable.getLastContinuable() and not have to put the logic here */
        this.lastContinuable = this.body.continuation.nextContinuable
            ? this.body.continuation.getLastContinuable()
            : this.body;
        this.savedContinuation = this.lastContinuable.continuation;
    }

    /* This is a convenience parameter for dealing with recursion in
     named procedures. If we are here, we are in the midst of defining
     a procedure, which means that the env parameter does not yet
     have a binding for it. So we set it up manually.

     todo bl: this design may be changed when I implement tail recursion.
     */
    this.name = name;
    this.env[name] = newProcedureDatum(this);
}

SchemeProcedure.prototype.setContinuation = function(c) {
    this.lastContinuable.continuation = c;
};

SchemeProcedure.prototype.resetContinuation = function() {
    this.lastContinuable.continuation = this.savedContinuation;
};

SchemeProcedure.prototype.clone = function() {
    var ans = new SchemeProcedure();
    ans.formalsArray = shallowArrayCopy(this.formalsArray);
    ans.isDotted = this.isDotted;
    ans.body = this.body.clone();
    ans.env = shallowHashCopy(this.env);
    return ans;
};

SchemeProcedure.prototype.eval = function(args) {
    return this.body.evalSiblingsReturnLast(this.bindArgs(args));
};

SchemeProcedure.prototype.toString = function() {
    return 'proc:' + this.name;
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

SchemeProcedure.prototype.bindArgs = function(args, env) {
    var envCopy = env || shallowHashCopy(this.env);

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

function shallowArrayCopy(array) {
    var ans = [];
    for (var i=0; i<array.length; ++i)
        ans[i] = array[i];
    return ans;
}

function shallowHashCopy(hash) {
    var ans = {};
    for (var i in hash)
    ans[i] = hash[i];
    return ans;
}

function SchemePort(portno) {
    this.portno = portno;
}

SchemePort.prototype.toString = function() {
    return "[Port " + this.portno + "]";
};

