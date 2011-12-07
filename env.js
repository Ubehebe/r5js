var allEnvironments = [];

function Environment(name, enclosingEnv) {
    this.name = name || 'global'; // just for use in pretty-printing
    if (enclosingEnv)
        this.enclosingEnv = enclosingEnv;
    this.bindings = {}; // hey, never use this; use this.get() instead
    allEnvironments.push(this); // just for debugging
}

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

Environment.prototype.get = function(name) {

    var maybe = this.bindings[name];

    if (maybe)
        return maybe;
    // If the current environment has no binding for the name, look one level up
    else if (this.enclosingEnv)
        return this.enclosingEnv.get(name);
    else
        throw new UnboundVariable(name + ' in env ' + this.name);
};

Environment.prototype.addBinding = function(name, val) {
    if (!this.bindings[name])
        this.bindings[name] = val;
    else throw new InternalInterpreterError('warning, redefining ' + name);
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