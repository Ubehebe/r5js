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
    }
    var topmostBindingForName = allBindingsForName[allBindingsForName.length - 1];

    if (topmostBindingForName instanceof BindingCounter) {
        // If the topmost binding is exhausted, pop it and try the call again.
        if (topmostBindingForName.exhausted()) {
            allBindingsForName.pop();
            return this.get(name);
        }

        // Otherwise, return and decrement that binding.
        else {
            return topmostBindingForName.getAndDecrement();
        }
    }

    /* If we did not get a BindingCounter object, we must not be dealing with
     a formal parameter, so we really don't have to worry about
     repeated bindings. Just return the binding. */
    else {
        return topmostBindingForName;
    }
};

Environment.prototype.addBinding = function(name, val) {

    if (!this.bindings[name])
        this.bindings[name] = [val];
    else
        this.bindings[name].push(val);
};

// Should only be called from within procs, where we have id frequency data
Environment.prototype.addRepeatedBinding = function(name, val, numRepetitions) {

    var allBindingsForName = this.bindings[name];
    var topmostBindingForName = allBindingsForName
        && allBindingsForName[allBindingsForName.length - 1];
    var reuseTopmostBinding = topmostBindingForName
        && topmostBindingForName instanceof BindingCounter
        && topmostBindingForName.exhausted();

    /* If the topmost binding is exhausted, it will never be needed again,
     so we can reuse its BindingCounter object. This is not merely an
     optimization. If we didn't do this (or something equivalent, such as
     popping the exhausted object), the stack of exhausted bindings
     would accumulate during tail calls, meaning that we couldn't support
     an unbounded number of active tail calls.

     todo bl: I discovered this invariant post hoc, which means I need to
     think about it more.
     */
    if (reuseTopmostBinding) {
        topmostBindingForName.val = val;
        topmostBindingForName.numRemaining = numRepetitions;
    } else {

        var toPush = new BindingCounter(val, numRepetitions);
        if (!allBindingsForName)
            this.bindings[name] = [toPush];
        else
            allBindingsForName.push(toPush);
    }
};

Environment.prototype.extendBindingLifetimes = function(histogram) {

    var allBindingsForName, topmostBindingsForName;

    for (var name in histogram) {
        allBindingsForName = this.bindings[name];
        topmostBindingsForName = allBindingsForName[allBindingsForName.length - 1];
        if (!(topmostBindingsForName instanceof BindingCounter))
            throw new InternalInterpreterError('invariant incorrect: not in pop mode for ' + name);
        topmostBindingsForName.incBy(histogram[name]);
    }
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

function BindingCounter(val, numRemaining) {
    this.val = val;
    this.numRemaining = numRemaining;
}

BindingCounter.prototype.getAndDecrement = function() {
    --this.numRemaining;
    return this.val;
};

BindingCounter.prototype.incBy = function(delta) {
    this.numRemaining += delta;
};

BindingCounter.prototype.exhausted = function() {
    return this.numRemaining === 0;
};

BindingCounter.prototype.toString = function() {
    return this.val.toString() + ' (' + this.numRemaining + ' uses)';
};

BindingCounter.prototype.clone = function() {
    return new BindingCounter(this.val, this.numRemaining);
};