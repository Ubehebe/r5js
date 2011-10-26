Datum.prototype.eval = function(env) {
    return this.values.pop()(this, env);
};

Datum.prototype.desugar = function(env) {
    var desugarFn = this.desugars && this.desugars.pop();
	var ans = desugarFn ? desugarFn(this, env) : this;
	console.log('desugar ' + this + ' => ' + ans);
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

Datum.prototype.seqThrowawayAllButLast = function(env) {
    var first, tmp, curEnd;
    for (var cur = this; cur; cur = cur.nextSibling) {
        /* This check is necessary because node.desugar can return null for some
            nodes (when it makes sense for the node to drop off the tree before
            evaluation, e.g. for definitions). */
        if (tmp = cur.desugar(env)) {
            if (!first) {
                first = tmp;
            }
            else if (curEnd) {
                curEnd.nextSibling = tmp;
            }

            if (tmp.isList())
                curEnd  = tmp.firstChild.lastSibling().getContinuationEndpoint();
        }
    }
    return first;
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

function desugar(text, env, lhs) {
    return new Parser(
        new Reader(
            new Scanner(text)
        ).read()
    ).parse(lhs)
        .desugar(env);
}

function trampoline(node, env) {

    var ans;
    var args, next;

    while (node
        && node.isList()
        && node.firstChild
        && node.firstChild.payload !== undefined) {

        console.log('trampoline: ' + node);

        var proc = env[node.firstChild.payload];

        if (typeof proc === 'function') {
            args = gatherArgs(node.firstChild, env);
            next = args.pop(); // the continuation
            ans = proc.apply(null, args);
            var nextName = next && next.at('(').firstChild.payload;
            if (nextName) {
                env[nextName] = ans;
            }
            node = next.at('(').nextSibling;
        }

        else if (proc instanceof Datum && proc.isProcedure()) {
            var unwrappedProc = proc.payload.clone();
            args = gatherArgs(node.firstChild, env);
            next = args.pop();
            unwrappedProc.bindArgs(args, env);
            node = unwrappedProc.body;
            node.lastSibling().nextSibling = next;
        }

        else throw new InternalInterpreterError('unexpected operator ' + proc + ' for key ' + node.firstChild.payload);

    }

    ans = ans || node;

    console.log('end of trampoline, result ' + ans);
    return ans;
}

function gatherArgs(operator, env) {

    var args = [];

    for (var cur = operator.nextSibling; cur && cur.nextSibling; cur = cur.nextSibling) {
        if (cur.isIdentifier())
            args.push(env[cur.payload]);
        else if (cur.payload !== undefined)
            args.push(maybeWrapResult(cur.payload, cur.type));
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    if (cur && cur.isList() && cur.firstChild && cur.firstChild.payload === 'lambda')
        args.push(cur);

    return args;
}