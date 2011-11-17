function Environment() {
    this.bindings = {}; // hey, never use this; use this.get() instead
}

/* Intended just to be used as a sanity check during startup,
 to make sure we don't multiply define builtin procedures. In general,
 multiple bindings for a given name are fine. */
Environment.prototype.hasBinding = function(name) {
    return this.bindings[name] !== undefined;
};

Environment.prototype.get = function(name) {

    var allBindings = this.bindings[name];
    if (!allBindings)
        throw new UnboundVariable(name);
    var topmostBinding = allBindings[allBindings.length - 1];
    if (topmostBinding instanceof BindingCounter) {
        if (topmostBinding.exhausted()) {
            console.log('exhausted binding for ' + name);
            allBindings.pop();
            return this.get(name);
        } else {
            var ans = topmostBinding.getAndDecrement();
            console.log('got ' + ans + ' for ' + name + ', ' + topmostBinding.numRemaining + ' uses left');
            return ans;
        }
    } else return topmostBinding;
};

Environment.prototype.addBinding = function(name, val) {

    if (!this.bindings[name])
        this.bindings[name] = [val];
    else
        this.bindings[name].push(val);

};

// Should only be called from within procs, where we have id frequency data
Environment.prototype.addRepeatedBinding = function(name, val, numRepetitions) {

    var toPush = new BindingCounter(val, numRepetitions);

    if (!this.bindings[name])
        this.bindings[name] = [toPush];
    else
        this.bindings[name].push(toPush);

    console.log('addRepeatedBinding ' + numRepetitions + ': now\n' + this.toString());

};

Environment.prototype.toString = function() {
    var ans = '';
    for (var name in this.bindings) {
        ans += name + ' => [';
        for (var i=0; i < this.bindings[name].length; ++i)
            ans += typeof this.bindings[name][i] === 'function'
        ? '(...js...), '
                : this.bindings[name][i].toString() + ', ';
        ans += ']\n';
    }
    return ans;
};

Environment.prototype.clone = function() {
    var ans = new Environment();
    var val;
    for (var name in this.bindings) {
        val = this.bindings[name];
        ans.bindings[name] = val instanceof BindingCounter
            ? val.clone()
            : val;

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
}

BindingCounter.prototype.exhausted = function() {
    return this.numRemaining === 0;
};

BindingCounter.prototype.toString = function() {
    return this.val.toString() + ' (' + this.numRemaining + ' uses)';
};

BindingCounter.prototype.clone = function() {
    // todo bl is this the right semantics for cloning?
    // Or should we reset the counter?
    return new BindingCounter(this.val, this.numRemaining);
};