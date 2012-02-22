function newEnvironmentSpecifier(version) {
    return newIdOrLiteral(new Environment(null, version), 'environment-specifier');
}

function SchemeProcedure(formalsArray, isDotted, bodyStart, env, name) {
    this.isDotted = isDotted;
    this.env = new Environment(name, env);
    this.formalsArray = formalsArray;

    /* This name has no semantic importance. It's just used for
    pretty-printing debugs and messages to the user. */
    this.name = name;

    if (bodyStart) {

        /* R5RS 5.2.2: "A <body> containing internal definitions can always
        be converted into a completely equivalent letrec expression." */
        var letrecBindings = new SiblingBuffer();
        for (var cur = bodyStart; cur && cur.peekParse() === 'definition'; cur = cur.nextSibling) {
                cur.forEach(function(node) {
                    if (node.firstChild && node.firstChild.payload === 'define')
                        letrecBindings.appendSibling(node.extractDefinition());
                });
        }

        if (letrecBindings.isEmpty()) {
            this.body = cur.sequence(this.env);
        } else {
            var letrec = newEmptyList();
            letrec.firstChild = letrecBindings.toSiblings();
            letrec.nextSibling = cur;
            this.body = newProcCall(newIdOrLiteral('letrec'), letrec, new Continuation(newCpsName()));
        }

        this.lastContinuable = this.body.getLastContinuable();
        this.savedContinuation = this.lastContinuable.continuation;
    }
}

SchemeProcedure.prototype.cloneWithEnv = function(env) {
    var ans = new SchemeProcedure(this.formalsArray, this.isDotted, null, env, this.name + "'-" + (uniqueNodeCounter++));
    ans.env.closures = this.env.closures; // non-cloning ok?
    ans.body = this.body;
    ans.lastContinuable = this.lastContinuable;
    return ans;
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

    var name, i;

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

