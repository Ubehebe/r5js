Datum.prototype.eval = function(env) {
    return this.values.pop()(this, env);
};

Datum.prototype.evalSiblingsReturnAll = function(env) {
    var ans = [];
    // The cur.value check ensures new Datum().evalSiblings() is [] for convenience.
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        ans.push(cur.eval(env));
    return ans;
};

Datum.prototype.evalSiblingsReturnLast = function(env) {
    var ans;
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        ans = cur.eval(env);
    return ans;
};