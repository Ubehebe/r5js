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
    if (enclosingEnv)
        this.enclosingEnv = enclosingEnv;
    this.bindings = {}; // hey, never use this; use this.get() instead
}

/* Just for environments defined in the standard; users shouldn't be able to
    add to them. */
Environment.prototype.seal = function() {
    this.sealed = true;
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

Environment.prototype.get = function(name) {

    var maybe = this.bindings[name];

    if (maybe) {
        if (typeof maybe === 'function')
            return newProcedureDatum(name, maybe);
        else if (maybe instanceof SchemeProcedure)
            return newProcedureDatum(maybe.name, maybe);
        else
            return maybe;
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
        if (maybe instanceof Datum)
            throw new InternalInterpreterError(name + ' is not a procedure!');
        return maybe;
    }
    else if (this.enclosingEnv)
        return this.enclosingEnv.getProcedure(name);
    else
        return null;
};

Environment.prototype.addBinding = function(name, val) {

    if (this.sealed)
        throw new InternalInterpreterError('tried to bind ' + name + ' in sealed environment ' + this);

    /* Macros require a backlink to the environment they were defined in to resolve
        literal identifiers. todo bl: is there a better place to put this? */
    if (val instanceof SchemeMacro)
        val.definitionEnv = this;

    if (!this.bindings[name]) {
        if (val instanceof Datum) {
            /* If we're about to store a wrapped SchemeProcedure
             or JavaScript function, unwrap it first. */
            if (val.isProcedure())
                this.bindings[name] = val.payload;
            else
                this.bindings[name] = val;
        } else if (typeof val === 'function' /* primitive procedure */
            || val instanceof SchemeProcedure /* library/user procedure */
            || val instanceof Continuation /* call-with-current-continuation etc. */
            || val instanceof Array /* values and call-with-values */
            || val instanceof SchemeMacro /* macros */) {
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

Environment.prototype.toString = function() {
    var ans = this.name + ':\n';
    for (var name in this.bindings) {
        ans += name + ' => ';
        if (typeof this.bindings[name] === 'function')
            ans += '(...js...), ';
        else
            ans += this.bindings[name].toString() + ', ';
        ans += '\n';
    }
    return ans;
};