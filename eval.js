Datum.prototype.eval = function(env) {
    return this.values.pop()(this, env);
};

/* For when we want to evaluate a list of things and return all their values
 (for example, we have to evaluate all operands to a procedure-call). */
Datum.prototype.evalSiblingsReturnAll = function(env) {
    var ans = [];
    // The cur.values check ensures new Datum().evalSiblings() is [] for convenience.
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        ans.push(cur.eval(env));
    return ans;
};

/* For when we want to evaluate a list of things for their side effects, except that
 we want to return the value of the last. For example, a list of expressions. */
Datum.prototype.evalSiblingsReturnLast = function(env) {
    var ans;
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        ans = cur.eval(env);
    return ans;
};

/* For when we want to evaluate something for its side effects
 (for example a list of definitions). */
Datum.prototype.evalSiblingsReturnNone = function(env) {
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        cur.eval(env);
};

// Top-level convenience function
function eval(text, env, lhs) {
    return new Parser(
        new Reader(
            new Scanner(text)
        ).read()
    ).parse(lhs)
        .eval(env);
}