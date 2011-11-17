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
    this.isDotted = isDotted;
    this.env = shallowHashCopy(env);

    if (bodyStart) {
        this.body = bodyStart.sequence(this.env);
        this.lastContinuable = this.body.getLastContinuable();
        this.savedContinuation = this.lastContinuable.continuation;
    }

    this.renameFormals(formalsArray);

    /* This is a convenience parameter for dealing with recursion in
     named procedures. If we are here, we are in the midst of defining
     a procedure, which means that the env parameter does not yet
     have a binding for it. So we set it up manually.

     todo bl: this design may be changed when I implement tail recursion.
     */
    this.name = name;
    this.env[name] = newProcedureDatum(this);
}

/* When defining a SchemeProcedure, we rename its formal parameters
    to be globally unique. This saves us having to manage multiple bindings
    at trampoline time, at the cost of an extra pass over the (desugared) body
    at definition time.

    Example:

    (define (foo x) (* x 100))

    The procedure body desugars as

    (* x 100 [_0 ...])

    Now consider the sequence

    (define x 1)
    (+ x (foo 3))

    The last expression desugars as

    (foo 3 [foo' (+ x foo' [_1 ...])])

    Without parameter renaming, at evaluation time, the trampoline will
    bind 3 to x and append the procedure call's continuation to the end
    of the SchemeProcedure's desugared body:

    (* x 100 [foo' (+ x foo' [_1 ...])])

    This is incorrect because both x's will resolve to 3. But with parameter
    renaming, the formal parameter x is renamed to something like _2,
    so we get

    (* _2 100 [foo' (+ x foo' [_1 ...])])

    which is correct.

    An alternative to parameter renaming is to resolve identifiers in the
    procedure call's continuation before appending to the SchemeProcedure's
    body. So in the above example

    [foo' (+ x foo' [_1 ...])]

    would resolve to

    [foo' (+ 1 foo' [_1 ...])]

    which we could safely append to the SchemeProcedure's body. But this
    approach requires a linear walk of the continuation chain every time it
    shows up on the trampoline, which seems vastly inferior to walking the
    SchemeProcedure's body once, at definition time. */
SchemeProcedure.prototype.renameFormals = function(formalsArray) {

    this.formalsArray = [];
    var replacementDict = {};
    var name;
    for (var i=0; i<formalsArray.length; ++i) {
        name = newCpsName();
        this.formalsArray.push(name);
        replacementDict[formalsArray[i]] = name;
    }
    this.body.renameIds(replacementDict);

};

SchemeProcedure.prototype.setContinuation = function(c) {
    this.lastContinuable.continuation = c;
};

SchemeProcedure.prototype.resetContinuation = function() {
    this.lastContinuable.continuation = this.savedContinuation;
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

    var name, i;

    for (i = 0; i < this.formalsArray.length - 1; ++i) {
        name = this.formalsArray[i];
        env.addRepeatedBinding(name, args[i], this.formalsHistogram[name] || 0);
    }

    /* Thanks to non-scoped JavaScript local variables,
     i is now this.formalsArray.length - 1. */
    name = this.formalsArray[i];
    if (!this.isDotted) {
        env.addRepeatedBinding(name, args[i], this.formalsHistogram[name] || 0);
    } else {
        // Roll up the remaining arguments into a list
        var list = newEmptyList();
        // Go backwards and do prepends to avoid quadratic performance
        for (var j = args.length - 1; j >= this.formalsArray.length - 1; --j)
            list.prependChild(args[j]);
        env.addRepeatedBinding(name, list, this.formalsHistogram[name] || 0);
    }
};

function shallowArrayCopy(array) {
    var ans = [];
    for (var i = 0; i < array.length; ++i)
        ans[i] = array[i];
    return ans;
}

function SchemePort(portno) {
    this.portno = portno;
}

SchemePort.prototype.toString = function() {
    return "[Port " + this.portno + "]";
};

