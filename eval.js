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
            if (!first)
                first = tmp;
            else if (curEnd)
                curEnd.nextSibling = tmp;

            if (tmp.isList())
                curEnd = tmp.getContinuationEndpoint();
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

function desugar(text, env, lhs) {
    return new Parser(
        new Reader(
            new Scanner(text)
        ).read()
    ).parse(lhs)
        .desugar(env);
}

/* Executes a sequence of CPS calls without growing the stack. */
function trampoline(node, env) {

    var ans;
    var args, next;
    var primitiveName;

    while (isCpsExecutable(node)) {

        console.log('trampoline: ' + node);

        if (node.type === 'branch_shim') {
            node = node.firstChild;
            continue;
        }

        // Built-in identity function.
        else if (node.firstChild.type === 'id_shim') {
            var cur = node.firstChild.nextSibling;
            var next = cur.nextSibling.at('(');
            var nextName = next.firstChild.payload;
            ans = cur.isIdentifier()
                ? env[cur.payload]
                : maybeWrapResult(cur, cur.type);
            env[nextName] = ans;
            node = next.nextSibling;
            continue;
        }

        else if (node.firstChild.payload === 'if') {
            var test = node.firstChild.nextSibling;
            var testResult = test.isIdentifier() ? env[test.payload] : test;
            node = (testResult.unwrap() === false)
                ? test.nextSibling.nextSibling // alternate
                : test.nextSibling; // consequent
            continue;
        }

        var proc = env[node.firstChild.payload];

        /* If the proc is a primitive, call the JavaScript and bind the answer
         to the beginning of the next continuation if necessary. */
        if (typeof proc === 'function') {
            primitiveName = node.firstChild.payload;
            args = gatherArgs(node.firstChild, env);
            next = args.pop(); // the continuation
            ans = proc.apply(null, args);
            var nextName = next && next.at('(').firstChild.payload;
            if (nextName) {
                env[nextName] = ans;
                console.log('bound ' + ans + ' to ' + nextName);
            }
            node = next.at('(').nextSibling;
        }

        /* If the proc is written in Scheme, bind the arguments and execute
         the proc's body, remembering the continuation. */
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

    ans = ans || node; // todo bl clean up. has to do with ans not being set from non-primitive procs

    /* The value of ((lambda () +)) is the identifier "+", so that in complex
        forms like (((lambda () +)) 200 3) we can evaluate the operator
        and look up the resulting identifier in the environment. */
    if (typeof ans === 'function')
        ans = primitiveName;

    console.log('end of trampoline, result: ');
    console.log(ans);
    return ans;
}

/* CPS-executable form is (<operator> <operand>* <continuation>),
 as given in Datum.prototype.cpsSanityCheck. This function just checks
 the operator, since we want it to run in constant time from
 the trampoline. */
function isCpsExecutable(node) {
    if (!node)
        return false;
    return node.type === 'branch_shim' ||
        (node.isList()
            && node.firstChild
            && (node.firstChild.type === 'id_shim' || node.firstChild.payload !== undefined));
}

function gatherArgs(operator, env) {

    var args = [];

    for (var cur = operator.nextSibling; cur && cur.nextSibling; cur = cur.nextSibling) {
        if (cur.isIdentifier())
            args.push(env[cur.payload]);
        else if (cur.isQuote())
            args.push(cur.firstChild);
        else if (cur.payload !== undefined)
            args.push(maybeWrapResult(cur.payload, cur.type));
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    if (cur && cur.isList() && cur.firstChild && cur.firstChild.payload === 'lambda')
        args.push(cur);

    return args;
}