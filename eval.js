Datum.prototype.eval = function(env, continuation) {

    console.log('trampoline start');

    var result;

    for (var cur = this; cur; cur = continuation.next()) {
        console.log('trampoline: cur is ' + cur);
        result = cur.values.pop()(cur, env, continuation).result;
    }

    console.log('trampoline exhausted');
    return result;
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

// Top-level convenience function
function eval(text, env, lhs) {
    return new Parser(
        new Reader(
            new Scanner(text)
        ).read()
    ).parse(lhs)
        .eval(env, new Continuation());
}

function trampoline(node, env) {

    var ans;

    while (node) {

        if (!node.isList() || !node.firstChild || !node.firstChild.payload)
            throw new InternalInterpreterError('unexpected datum ' + node);

        var proc = env[node.firstChild.payload];

        if (typeof proc === 'function') {
            var args = gatherArgs(node.firstChild, env);
            var next = args.pop();
            ans = proc.apply(null, args);
            var nextName = next && next.at('(').firstChild.payload;
            if (nextName) {
                env[nextName] = ans;
            }
            node = next.at('(').nextSibling;
        }

        else if (proc instanceof Datum && proc.isProcedure()) {
            var unwrappedProc = proc.payload.clone();
            var args = gatherArgs(node.firstChild, env);
            var next = args.pop();
            unwrappedProc.bindArgs(args, env);
            node = unwrappedProc.body;
            node.lastSibling().nextSibling = next;
        }

        else throw new InternalInterpreterError('unexpected operator ' + proc);

    }

    console.log('end of trampoline, result ' + ans);
    return ans;
}

function gatherArgs(operator, env) {

    var args = [];

    for (var cur = operator.nextSibling; cur && cur.nextSibling; cur = cur.nextSibling) {
        if (cur.isIdentifier())
            args.push(env[cur.payload]);
        else if (cur.payload)
            args.push(maybeWrapResult(cur.payload, cur.type));
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    if (cur && cur.isList() && cur.firstChild && cur.firstChild.payload === 'lambda')
        args.push(cur);

    return args;
}

function Continuation(next) {
    this.nextList = newEmptyList();
    if (next)
        this.nextList.appendChild(next);
    this.cur = this.nextList.firstChild;
    /*this.result = null;
     */

}

Continuation.prototype.remember = function(firstSibling) {
    if (firstSibling) {
        // todo bl i got stack overflows here by creating a cycle!
        this.nextList.prependSiblings(firstSibling);
        this.cur = firstSibling;
    }
    return this;
};

Continuation.prototype.next = function() {
    var ans = this.cur;
    this.cur = this.cur && this.cur.nextSibling;
    return ans;
};

Continuation.prototype.inject = function(result) {
    this.result = result;
    return this;
};