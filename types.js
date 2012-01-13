function SchemeChar(c) {
    this.c = c;
}

SchemeChar.prototype.toString = function() {
    return this.c;
};

function newEnvironmentSpecifier(version) {
    return newIdOrLiteral(new Environment(null, version), 'environment-specifier');
}

function SchemeString(s) {
    this.s = s;
}

SchemeString.prototype.toString = function() {
    return this.s;
};

function SchemeProcedure(formalsArray, isDotted, bodyStart, env, name) {
    this.isDotted = isDotted;
    this.env = new Environment(name, env);
    this.formalsArray = formalsArray;

    /* The name is just used for pretty-printing,
    not for resolving stuff in the environment. */
    this.name = name;

    if (bodyStart) {
        this.body = bodyStart.sequence(this.env);
        this.lastContinuable = this.body.getLastContinuable();
        this.savedContinuation = this.lastContinuable.continuation;
    }
}

SchemeProcedure.prototype.doBindArgsAtRootEnv = function() {
    this.bindArgsAtRootEnv = true;
};

SchemeProcedure.prototype.setMustAlreadyBeBound = function(dict) {
    this.mustAlreadyBeBound = dict;
};

/* The only SchemeProcedure objects with null bodies should be
 definitions, since we have to insert their bodies later in parsing. For
 example:

 (define x 1)
 (+ x x)

 The definition gets desugared to ((lambda (x) null) 1). The expression
 (+ x x) later becomes the anonymous lambda's body:

 ((lambda (x) (+ x x)) 1)
 */
SchemeProcedure.prototype.isDefinition = function() {
    return !this.body;
};

SchemeProcedure.prototype.setBody = function(bodyContinuable) {
    if (!this.isDefinition())
        throw new InternalInterpreterError('not a definition: ' + this);
    this.body = bodyContinuable;
    this.lastContinuable = this.body.getLastContinuable();
    this.savedContinuation = this.lastContinuable.continuation;
};

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
SchemeProcedure.prototype.setContinuation = function(c, env) {
    /* The first part of this check is to avoid a null pointer dereference if
     the procedure has no body. Such an occurrence is not allowed by the
     Scheme grammar, but internally we rewrite definitions as dummy
     procedures, and programs are allowed to have no expressions, as in

     (define x 1)

     This should translate to something like

     ((lambda (x) <nothing>) 1)

     See comments in constructTopLevelDefs. */

    if (this.lastContinuable
        && this.lastContinuable.continuation !== c) {
        this.lastContinuable.continuation = c.clone();
        var nextContinuable = this.lastContinuable.continuation.nextContinuable;
        // todo bl hack!
        if (nextContinuable && nextContinuable.subtype instanceof Branch) {
            nextContinuable.subtype.consequent.setStartingEnv(env);
            if (nextContinuable.subtype.alternate)
                nextContinuable.subtype.alternate.setStartingEnv(env);
        }
    }
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

    if (this.bindArgsAtRootEnv) {
        /* This is just a shim used by top-level definitions. We implement
         definitions as procedures: for example,

         (define x 1)
         (define y 2)
         ...

         is like

         ((lambda (x) ((lambda (y) ...) 2) 1)

         This is fine when we're reading whole programs at a go. But for REPL-
         like functionality, we would have to make sure subsequent expressions
         targeted the original endpoint ..., which means the trampoline might
         have to return it.

         Instead of worrying about that, if we know this is a top-level
         definition, just send the binding all the way up the chain. Ha! */
        env = env.rootEnv();
    }

    var name, i;

    if (this.mustAlreadyBeBound) {
        for (name in this.mustAlreadyBeBound)
            if (!env.hasBindingRecursive(name))
                throw new UnboundVariable('cannot set undefined variable: ' + name);
    }

    for (i = 0; i < this.formalsArray.length - 1; ++i) {
        name = this.formalsArray[i];
        env.addBinding(name, args[i]);
    }

    if (this.formalsArray.length > 0) {

        /* Thanks to non-scoped JavaScript local variables,
         i is now this.formalsArray.length - 1. */
        name = this.formalsArray[i];
        if (!this.isDotted) {
            env.addBinding(name, args[i]);
        } else {
            // Roll up the remaining arguments into a list
            var list = newEmptyList();
            // Go backwards and do prepends to avoid quadratic performance
            for (var j = args.length - 1; j >= this.formalsArray.length - 1; --j)
                list.prependChild(args[j]);
            env.addBinding(name, list);
        }
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

