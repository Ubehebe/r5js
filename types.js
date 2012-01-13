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

SchemeProcedure.prototype.setContinuation = function(c) {
    /* This will be a vacuous write for a tail call. But that is
    probably still faster than checking if we are in tail position and,
    if so, explicitly doing nothing. */
    if (this.lastContinuable)
        this.lastContinuable.continuation = c;
};

SchemeProcedure.prototype.setEnv = function(env) {
    /* todo bl is it possible to have a procedure body whose first
     continuable is a branch? hopefully not, and I can remove
     the second check. */
    if (this.body) {
        if (this.body.subtype instanceof ProcCall)
            this.body.subtype.setEnv(env, true);
        else
            throw new InternalInterpreterError(
                'invariant incorrect -- procedure does not begin with proc call');
    }
};

// todo bl are we sure this covers all forms of tail recursion in R5RS?
SchemeProcedure.prototype.isTailCall = function(c) {
  if (this.lastContinuable && this.lastContinuable.continuation === c) {
               // a good place to see if tail recursion is actually working :)
            // console.log('TAIL RECURSION!!!');
      return true;
  } else return false;
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

