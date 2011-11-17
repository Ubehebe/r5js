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
    this.env = env.clone();
    this.formalsArray = formalsArray;

    /* The name is just used for pretty-printing,
    not for resolving stuff in the environment. */
    this.name = name;

    /* Prime the parameter histogram.
        See Continuable.parameterHistogram for more information. */
    this.parameterHistogram = {};
    for (var i=0; i<formalsArray.length; ++i)
        this.parameterHistogram[formalsArray[i]] = 0;


    if (bodyStart) {
        this.body = bodyStart.sequence(this.env);
        this.body.parameterHistogram(this.parameterHistogram);
        this.lastContinuable = this.body.getLastContinuable();
        this.savedContinuation = this.lastContinuable.continuation;
    }

}

/* This function is a no-op for tail calls, thus should support an unlimited
    number of active tail calls. Example:

    (define (len xs buf) (if (null? xs) buf (len (cdr xs) (+ 1 buf))))

    The body is desugared as

    (null? xs [_0 {_0
        ? (id buf [_1 ...])
        : (cdr xs [_2 (+ 1 buf [_3 (len _2 _3 [_4 ...])])])
        }])

    and the SchemeProcedure's last continuation is some fake value
    representing the branch.

    Now consider the evaluation of the expression

    (+ 1 (len '(a b c) 0))

    This is desugared as

    (len '(a b c) 0 [_5 (+ 1 _5 [_6 ...])])

    The trampoline will work as follows:

    (len '(a b c) 0 [_5 (+ 1 _5 [_6 ...])])
    1. Bind '(a b c) to xs
    2. Bind 0 to buf
    3. Set the SchemeProcedure's last continuation to [_5 (+ 1 _5 [_6 ...])].
        Since this is not equal to the current value (the fake continuation
        representing the branch), we have to clone it.
    4. Advance to the SchemeProcedure's body

    At the branch in the body, the alternate will be selected, and its last
    continuation will be set to [_5 (+ 1 _5 [_6 ...])]:

    (cdr xs [_2 (+ 1 buf [_3 (len _2 _3 [_5 (+ 1 _5 [_6 ...])])])])

    Now we do some more trampolining and arrive at the next
    nonprimitive procedure call:

    (len _2 _3 [_5 (+ 1 _5 [_6 ...])])
    5. Bind _2 ( = '(b)) to xs
    6. Bind _3 (= 1) to buf
    7. Set the SchemeProcedure's last continuation to [_5 (+ 1 _5 [_6 ...])].
        But it already has this value -- see step 3. So do nothing.
    8. Advance to the SchemeProcedure's body.

    This should work for simple kinds of tail recursion, but I need to verify
    it works for all the tail call sites required by the Scheme standard.
 */
SchemeProcedure.prototype.setContinuation = function(c) {
    if (this.lastContinuable.continuation !== c)
        this.lastContinuable.continuation = c.clone();
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
        env.addRepeatedBinding(name, args[i], this.parameterHistogram[name] || 0);
    }

    /* Thanks to non-scoped JavaScript local variables,
     i is now this.formalsArray.length - 1. */
    name = this.formalsArray[i];
    if (!this.isDotted) {
        env.addRepeatedBinding(name, args[i], this.parameterHistogram[name] || 0);
    } else {
        // Roll up the remaining arguments into a list
        var list = newEmptyList();
        // Go backwards and do prepends to avoid quadratic performance
        for (var j = args.length - 1; j >= this.formalsArray.length - 1; --j)
            list.prependChild(args[j]);
        env.addRepeatedBinding(name, list, this.parameterHistogram[name] || 0);
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

