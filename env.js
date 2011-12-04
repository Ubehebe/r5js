/* todo bl: we seem not to need binding stacks anymore, since
    we're also nesting Environment objects which serves much the same
    purpose. */

var allEnvironments = [];

function Environment(name, enclosingEnv) {
    this.name = name || 'global'; // just for use in pretty-printing
    if (enclosingEnv)
        this.enclosingEnv = enclosingEnv;
    this.bindings = {}; // hey, never use this; use this.get() instead
    allEnvironments.push(this); // just for debugging
}

/* Intended just to be used as a sanity check during startup,
 to make sure we don't multiply define builtin procedures. In general,
 multiple bindings for a given name are fine. */
Environment.prototype.hasBinding = function(name) {
    return this.bindings[name] !== undefined;
};

Environment.prototype.get = function(name) {

    var allBindingsForName = this.bindings[name];
    if (!allBindingsForName || allBindingsForName.length === 0) {
        // If the current environment has no binding for the name, look one level up
        if (this.enclosingEnv)
            return this.enclosingEnv.get(name);
        else throw new UnboundVariable(name + ' in env ' + this.name);
    } else {
        return allBindingsForName[allBindingsForName.length - 1];
    }
};

Environment.prototype.addBinding = function(name, val) {
    if (!this.bindings[name])
        this.bindings[name] = [val];
    else throw new InternalInterpreterError('warning, redefining ' + name);
};

Environment.prototype.toString = function() {
    var ans = this.name + ':\n';
    for (var name in this.bindings) {
        ans += name + ' => [';
        for (var i = 0; i < this.bindings[name].length; ++i)
            ans += typeof this.bindings[name][i] === 'function'
                ? '(...js...), '
                : this.bindings[name][i].toString() + ', ';
        ans += ']\n';
    }
    return ans;
};