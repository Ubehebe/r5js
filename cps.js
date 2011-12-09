function Continuation(lastResultName) {

    this.lastResultName = lastResultName;

    /* Example: (g (f x y) z) desugared is
     (f x y [f' (g f' z [g' ...])])
     The continuation c is [f' (g f' z [g' ...])]
     c.lastResultName is f'
     c.nextContinuable is (g f' z ...)
     */
}

Continuation.prototype.toString = function() {
    return '[' + this.lastResultName + ' ' + this.nextContinuable + ']';
};

/* todo bl this seems to be required for non-tail recursion, but it is slow.
 Can we improve or eliminate it? */
Continuation.prototype.clone = function() {
    var ans = new Continuation(this.lastResultName);
    if (this.nextContinuable) {
        ans.nextContinuable = new Continuable(
            this.nextContinuable.subtype,
            this.nextContinuable.continuation.clone());
        // todo bl
        if (this.nextContinuable.env)
            ans.nextContinuable.env = this.nextContinuable.env;
    }
    return ans;
};

/* I decided to do composition instead of inheritance because it is more
 straightforward in JavaScript. */
function Continuable(subtype, continuation) {
    if (!subtype || !continuation) // todo bl take out after testing
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype = subtype;
    this.continuation = continuation;
    //this.lastContinuable = this.getLastContinuable(); // todo bl caching problems
}

/* The last continuable of a continuable-continuation chain is the first
 continuable c such that c.continuation.nextContinuable is null. */
Continuable.prototype.getLastContinuable = function() {
    if (!this.continuation)
        throw new InternalInterpreterError('invariant violated');
    return this.continuation.nextContinuable
        ? this.continuation.nextContinuable.getLastContinuable()
        : this;
};

// delegate to subtype, passing in the continuation
Continuable.prototype.toString = function() {
    return this.subtype.toString(this.continuation);
};

// For composition; should only be called from newIdShim
function IdShim(payload) {
    this.payload = payload;
}

/* Just for clarity in debugging, the string representation
 of IdShims looks like a procedure call of an "id" procedure. */
IdShim.prototype.toString = function(continuation) {
    return '(id ' + this.payload + ' ' + continuation + ')';
};

IdShim.prototype.evalAndAdvance = function(env, continuation, resultStruct) {

    var ans;

    if (this.payload.isIdentifier())
        ans = env.get(this.payload.payload);
    else if (this.payload.isQuote())
        ans = this.payload.firstChild;
    else
        ans = maybeWrapResult(this.payload.payload, this.payload.type);

    env.addBinding(continuation.lastResultName, ans);

    resultStruct.ans = ans;
    resultStruct.nextContinuable = continuation.nextContinuable;

};

/* If a nonterminal in the grammar has no associated desugar function,
 desugaring it will be a no-op. That is often the right behavior,
 but sometimes we would like to wrap the datum in a Continuable
 object for convenience on the trampoline. For example, the program
 "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]). */
function newIdShim(payload, continuationName) {
    return new Continuable(new IdShim(payload), new Continuation(continuationName));
}

function newBranch(testIdOrLiteral, consequentContinuable, alternateContinuable, continuation) {
    return new Continuable(
        new Branch(testIdOrLiteral, consequentContinuable, alternateContinuable),
        continuation);
}

// For composition; should only be called from newBranch
function Branch(testIdOrLiteral, consequentContinuable, alternateContinuable) {
    this.test = testIdOrLiteral;
    this.consequent = consequentContinuable;
    this.alternate = alternateContinuable;
    this.consequentLastContinuable = consequentContinuable.getLastContinuable();
    this.alternateLastContinuable = alternateContinuable && alternateContinuable.getLastContinuable();
}

Branch.prototype.toString = function(continuation) {
    return '{' + this.test
        + ' ? ' + this.consequent.toString()
        + ' : ' + (this.alternate && this.alternate.toString())
        + ' ' + continuation
        + '}';
};

// For composition; should only be called from newProcCall
function ProcCall(operatorName, firstOperand) {
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
}

function newProcCall(operatorName, firstOperand, continuation) {
    return new Continuable(new ProcCall(operatorName, firstOperand), continuation);
}

ProcCall.prototype.toString = function(continuation) {
    var ans = '(' + this.operatorName + ' ';
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        ans += cur.toString() + ' ';
    return ans + continuation + ')';
};

function TrampolineResultStruct() {
    /*
     this.ans;
     this.nextContinuable;
     this.currentEnv;
     */
}

TrampolineResultStruct.prototype.clear = function() {
    this.currentEnv = null;
    this.ans = null;
};

// This is the main evaluation function.
function trampoline(continuable, env) {

    var curContinuable = continuable;
    var ans;
    var tmp = new TrampolineResultStruct();

    while (curContinuable) {

        tmp.clear();

        // a good first step for debugging: console.log('boing: ' + curContinuable);
        curContinuable.subtype.evalAndAdvance(curContinuable.env || env, curContinuable.continuation, tmp);
        ans = tmp.ans;
        curContinuable = tmp.nextContinuable;
        if (tmp.currentEnv)
            env = tmp.currentEnv;
    }
    return ans;
}

Branch.prototype.resetContinuation = function() {
    this.consequentLastContinuable.continuation.nextContinuable = null;
    if (this.alternateLastContinuable)
        this.alternateLastContinuable.continuation.nextContinuable = null;
};

Branch.prototype.evalAndAdvance = function(env, continuation, resultStruct) {

    this.resetContinuation();

    var testResult = this.test.isIdentifier()
        ? env.get(this.test.payload)
        : maybeWrapResult(this.test, this.test.type);
    if (testResult.payload === false) {
        this.alternateLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.alternate;
    } else {
        this.consequentLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.consequent;
    }
};

ProcCall.prototype.evalAndAdvance = function(env, continuation, resultStruct) {

    var proc = env.getProcedure(this.operatorName);
    var args;
    var ans;

    /* Primitive procedure, represented by JavaScript function:
     (+ x y [ans ...]). We perform the action ("+"), bind the
     result to the continuation's result name ("ans"), and advance
     to the next continuable ("..."). */
    if (typeof proc === 'function') {

        args = evalArgs(this.firstOperand, env);

        /* For call/cc etc: push the current ProcCall, the continuation,
            and the result struct. todo bl: pushing the ProcCall invites trouble
            because it contains the _unevaluated_ arguments. When I'm done
            implementing all the 'magical' functions like apply and call/cc,
            review what support they really need. */
        if (proc.hasSpecialEvalLogic) {
            args.push(this);
            args.push(continuation);
            args.push(resultStruct);
            proc.apply(null, args);
        }

        else {
            ans = proc.apply(null, args);
            if (continuation.nextContinuable && continuation.nextContinuable.env) {
                continuation.nextContinuable.env.addBinding(continuation.lastResultName, ans);
            } else {
                env.addBinding(continuation.lastResultName, ans);
            }
            resultStruct.ans = ans;
            resultStruct.nextContinuable = continuation.nextContinuable && continuation.nextContinuable;
        }
    }

    /* Non-primitive procedure, represented by SchemeProcedure object.
     Example: suppose we have

     (define (foo x y) (+ x (* 2 y)))

     The body of this procedure is desugared as

     (* 2 y [_0 (+ x _0 [_1 ...])])

     Then we have the (nested) procedure call

     (+ 1 (foo 3 4))

     which is desugared as

     (foo 3 4 [foo' (+ 1 foo' [_2 ...])])

     We bind the arguments ("1" and "2") to the formal parameters
     ("x" and "y"), append the ProcCall's continuation to the end of the
     SchemeProcedure's continuation, and advance to the beginning of the
     SchemeProcedure's body. Thus, on the next iteration of the trampoline
     loop, we will have the following:

     (* 2 y [_0 (+ x _0 [foo' (+ 1 foo' [_2 ...])])])
     */
    else if (proc instanceof SchemeProcedure) {
        args = evalArgs(this.firstOperand, env);
        if (continuation.nextContinuable)
            continuation.nextContinuable.env = env;
        // This will be a no-op if tail recursion is detected
        proc.setContinuation(continuation);
        proc.checkNumArgs(args.length);
        proc.bindArgs(args, proc.env);
        resultStruct.nextContinuable = proc.body;
        resultStruct.currentEnv = proc.env;
    }

    else if (proc instanceof Continuation) {
        env.addBinding(proc.lastResultName, this.firstOperand);
        resultStruct.ans = this.firstOperand;
        resultStruct.nextContinuable = proc.nextContinuable;
    }

    else throw new InternalInterpreterError('unrecognized proc '
            + proc
            + ' for name '
            + this.operatorName);
};

function evalArgs(firstOperand, env) {
    var args = [];
    for (var cur = firstOperand; cur; cur = cur.nextSibling) {
        if (cur instanceof Continuation) // todo bl too much special logic for call/cc
            args.push(cur);
        else if (cur.isIdentifier())
            args.push(env.get(cur.payload));
        else if (cur.isQuote())
            args.push(cur.firstChild);
        else if (cur.payload !== undefined)
            args.push(maybeWrapResult(cur.payload, cur.type));
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    return args;
}

