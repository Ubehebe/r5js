/* An Environment stores three common kinds of objects:
    - Datums (most Scheme values: numbers, identifiers, etc.)
    - SchemeProcedures (native Scheme procedures)
    - JavaScript functions ('primitive' Scheme procedures)

    There is a fourth kind of object, a Continuation, which can get stored
    when calling "magical" procedures like call/cc, where the current
    continuation is bound to a formal parameter.

    Environment.prototype.get will only ever return Datums and Continuations;
    it will wrap SchemeProcedures and JavaScript functions in Datum
    wrappers before returning, to allow for things like

    (cons + (lambda () "hi!"))

    A drawback is that comparisons on stuff retrieved from an Environment
    may need to be unwrapped:

    var x = env.get('foo');
    var y = env.get('foo');
    x == y // false if foo is a SchemeProcedure or JavaScript function!

    If you know your key should retrieve a SchemeProcedure or JavaScript
    function, you can use Environment.prototype.getProcedure to avoid the
    wrapping and unwrapping.
 */
function Environment(name, enclosingEnv) {
    this.name = name; // just for use in pretty-printing
    if (enclosingEnv) {
        this.enclosingEnv = enclosingEnv;
        // useful for debugging console.log('created env ' + this + ' referencing ' + enclosingEnv);
    }
    this.bindings = {}; // hey, never use this; use this.get() instead
}

// See comments in Environment.prototype.addBinding.
Environment.prototype.unspecifiedSentinel = new Object();

/* Just for environments defined in the standard; users shouldn't be able to
    add to them. */
Environment.prototype.seal = function() {
    this.sealed = true;
};

Environment.prototype.allowRedefs = function() {
    this.redefsOk = true;
    return this;
};

Environment.prototype.clone = function(name) {

      if (this.enclosingEnv)
        throw new InternalInterpreterError('clone should only be used during '
        + 'interpreter bootstrapping');

    var cloned = new Environment(name);

    for (var name in this.bindings) {
        var val = this.bindings[name];
        cloned.bindings[name] = val instanceof SchemeMacro
            ? val.clone(cloned)
            : val;
    }

    return cloned;
};

/* Intended just to be used as a sanity check during startup,
 to make sure we don't multiply define builtin procedures. */
Environment.prototype.hasBinding = function(name) {
    /* This is not a bug, since we store values that could confuse JavaScript,
        like #f and 0, inside datum objects. */
    return this.bindings[name];
};

Environment.prototype.hasBindingRecursive = function(name) {
    /* This won't foul up because bindings can't have primitives like false
        that could confuse the comparison (they're always wrapped in datums) */
    return this.bindings[name]
        || (this.enclosingEnv && this.enclosingEnv.hasBindingRecursive(name));
};

Environment.prototype.addAll = function(otherEnv) {
    var name;
    for (name in this.bindings)
        throw new InternalInterpreterError(this.name + ' is not empty');

    var otherBindings = otherEnv.bindings;
    for (name in otherBindings)
        this.bindings[name] = otherBindings[name];

    return this;
};

/* todo bl do I understand why almost nothing is cloned going into or
 out of Environments? */
Environment.prototype.get = function(name) {

    var maybe = this.bindings[name];

    if (maybe) {
        // Redirects for free ids in macro transcriptions
        if (maybe instanceof Environment)
            return maybe.get(name);
        /* Wrap primitive procedures in a Datum. We could store primitive
         procedures already wrapped. But if getProcedure() is expected
         to be more common for primitive procedures than get(), I think it
         is better to only wrap them for get(). Only profiling can say. */
        else if (typeof maybe === 'function')
            return newProcedureDatum(name, maybe);
        else if (maybe === this.unspecifiedSentinel)
            return null;
        else if (maybe instanceof Datum && maybe.isProcedure())
            return maybe.clone(true); // todo bl clean up (is necessary)
        // Everything else
        else return maybe;
    }
    // If the current environment has no binding for the name, look one level up
    else if (this.enclosingEnv)
        return this.enclosingEnv.get(name);
    else
        throw new UnboundVariable(name + ' in env ' + this.name);
};

Environment.prototype.getProcedure = function(name) {
    var maybe = this.bindings[name];

    if (maybe) {
        if (maybe instanceof Environment)
            return maybe.getProcedure(name);
        else if (maybe instanceof Datum && maybe.isProcedure()) {
            if (maybe.hasClosure())
                maybe.payload.env = maybe.closure;
            return maybe.payload;
        } else if (typeof maybe === 'function'
            || maybe instanceof SchemeMacro
            || maybe instanceof Continuation) {
            return maybe;
        } else throw new InternalInterpreterError(name + ' is not a proc!');
    } else if (this.enclosingEnv)
        return this.enclosingEnv.getProcedure(name);
    else
        return null;
};

Environment.prototype.addBinding = function(name, val) {

    if (this.sealed) {
        throw new InternalInterpreterError('tried to bind ' + name + ' in sealed environment ' + this);
    }

    else if (!this.bindings[name] || this.redefsOk || name.charAt(0) === '@') {

        // useful for debugging if (val instanceof Datum)
        //    console.log(this + ' addBinding ' + name + ' = ' + val);

        /* Macros require a backlink to the environment they were defined in to resolve
         literal identifiers. todo bl: is there a better place to put this? */
        if (val instanceof SchemeMacro)
            val.definitionEnv = this;

        if (val === null) {
            /* A value of null on the trampoline means an unspecified value.
             For example, the JavaScript implementation of display returns null.
             In order to distinguish between an unbound variable (error) and
             a variable bound to an unspecified value (not an error), we use
             Environment.prototype.unspecifiedSentinel. I suppose we could
             bind null or undefined, but this would probably lead to bugs in
             conditionals (if (this.bindings[env]) ...) */
            this.bindings[name] = this.unspecifiedSentinel;
        } else if (val instanceof SchemeProcedure) { /* non-primitive procedure */
            this.bindings[name] = newProcedureDatum(name, val);
        } else if (typeof val === 'function' /* primitive procedure */
            || val instanceof Datum /* lots of stuff, including wrapped procedures
         (We need wrapped procedures to support returning
         SchemeProcedure objects into different environments. The Datum
         wrapper has a backlink to the closure in which it was created.) */
            || val instanceof Continuation /* call-with-current-continuation etc. */
            || val instanceof Array /* values and call-with-values */
            || val instanceof SchemeMacro /* macros */
            || val instanceof Environment /* Redirects for free ids in macro transcriptions */) {
            this.bindings[name] = val;
        } else {
            throw new InternalInterpreterError('tried to store '
                + name
                + ' = '
                + val
                + ', which is not an acceptable value');
        }
    } else {
        throw new InternalInterpreterError('redefining '
            + name
            + ' in same env, not allowed');
    }
};

Environment.prototype.rootEnv = function() {
    return this.enclosingEnv && !this.enclosingEnv.sealed
        ? this.enclosingEnv.rootEnv()
        : this;
};

Environment.prototype.toString = function() {
    return this.name;
};

/* R5RS 5.2.1: "At the top level of a program, a definition

 (define <variable> <expression>)

 has essentially the same effect as the assignment expression

 (set! <variable> <expression>)

 if <variable> is bound. If <variable> is not bound, however, then
 the definition will bind <variable> to a new location before performing
 the assignment, whereas it would be an error to perform a set! on
 an unbound variable."

 We use the isTopLevel parameter to perform the override mentioned. */
Environment.prototype.mutate = function(name, newVal, isTopLevel) {
    if (this.bindings[name] || isTopLevel) {
        this.bindings[name] = null;
        this.addBinding(name, newVal);
    } else if (this.enclosingEnv) {
        this.enclosingEnv.mutate(name, newVal, isTopLevel);
    } else throw new UnboundVariable(name);
};