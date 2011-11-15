Datum.prototype.eval = function(env) {
    return this.values.pop()(this, env);
};

Datum.prototype.desugar = function(env, forceContinuationWrapper) {
    var desugarFn = this.desugars && this.desugars.pop();
	var ans = desugarFn ? desugarFn(this, env) : this;
	console.log('desugar ' + this + ' => ' + ans);
    if (forceContinuationWrapper && !ans.continuation /* todo bl: use Continuable instead */)
        ans = new ContinuationWrapper(ans, newCpsName());
	return ans;
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

Datum.prototype.sequence = function (env, disableContinuationWrappers, cpsNames) {
    var first, tmp, curEnd;
    for (var cur = this; cur; cur = cur.nextSibling) {
        /* This check is necessary because node.desugar can return null for some
         nodes (when it makes sense for the node to drop off the tree before
         evaluation, e.g. for definitions). */
        if (tmp = cur.desugar(env)) {
            if (cpsNames && tmp.continuation) {
                // todo bl hack
                if (tmp.continuation.nextContinuable instanceof Branch)
                    cpsNames.push(tmp.continuation.nextContinuable.continuation.lastResultName);
                else
                    cpsNames.push(tmp.continuation.lastResultName);
            }

            /* Node that have no desugar functions (for example, variables
             and literals) desugar as themselves. Usually this is OK,
             but when we need to sequence them (for example, the program
             "1 2 3"), we have to wrap them in an object in order to set the
             continuations properly. */
            if (!tmp.continuation && !disableContinuationWrappers)
                tmp = new ContinuationWrapper(tmp);

            if (tmp.continuation) {
                if (!first)
                    first = tmp;
                else if (curEnd)
                    curEnd.nextContinuable = tmp;

                curEnd = tmp.continuation;
            }
        }
    }
    return first; // can be undefined
};

/* For when we want to evaluate a list of things for their side effects, except that
 we want to return the value of the last. For example, a list of expressions. */
Datum.prototype.evalSiblingsReturnLast = function(env, continuation) {
    var ans;
    continuation.nextList.prependChild(this);
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        ans = cur.eval(env, continuation);
    return continuation.inject(ans);
};

/* For when we want to evaluate something for its side effects
 (for example a list of definitions). */
Datum.prototype.evalSiblingsReturnNone = function(env) {
    for (var cur = this; cur && cur.values; cur = cur.nextSibling)
        cur.eval(env);
};

function desugar(text, env, lhs) {
    return new Parser(
        new Reader(
            new Scanner(text)
        ).read()
    ).parse(lhs)
        .desugar(env);
}