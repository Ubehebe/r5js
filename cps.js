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
        throw new InternalInterpreterError('invariant incorrect');
    return this.continuation.nextContinuable
        ? this.continuation.nextContinuable.getLastContinuable()
        : this;
};

/* One of the drawbacks of using a trampoline for evaluation, instead
    of a stack, is that bindings can get confusing. For example, consider
    the program

    (define (foo x) (* x x x))
    (define x 10)
    (+ x (foo 3))

    The body of the procedure is desugared as

    (* x x x [_0 ...])

    The expression (+ x (foo 3)) is desugared as

    (foo 3 [_1 (+ x _1 [_2 ...])])

    The x should be bound to 10, according to (define x 10). If we didn't
    make any special preparations, when that desugared procedure call
    shows up on the trampoline, it will:

    1. Bind 3 to x
    2. Push [_1 (+ x _1 [_2 ...])] as the procedure body's last continuation
    3. Advance to the procedure body, giving

    (* x x x [_1 (+ x _1 [_2 ...])])

    Using the binding x=3 for all occurrences of x clearly gives the wrong
    answer.

    One solution would be to resolve all the bindings in the continuation
    [_1 (+ x _1 [_2 ...])] before rebinding x. This would require a walk of
    the continuation prior to step 1. In fact, we do oftentimes walk this
    continuation around this time in order to clone it. But there is no cloning
    in tail recursion (the continuation is simply reused), and I did not want
    to sacrifice performance in that important case.

    Here is another solution. We know at definition time that foo uses x
    exactly 3 times. The fourth use of x in

    (* x x x [_1 (+ x _1 [_2 ...])])

    should therefore refer to a different binding. So at procedure
    definition time, we construct a histogram of the uses of the formal
    parameters. We can use the histogram at evaluation time to figure
    out what the correct binding of x should be. (That is what the
    Environment class does.)

    (Actually, this is a simplification. We cannot collect the histogram
    across a branch, because we don't know at definition time which branch
    we're going to take. So what we actually do is collect the histogram down
    to the first branch. Each branch then collects its own histogram down to
    its next branch, and so on. At evaluation time, when the trampoline comes
    across a branch that has a histogram, it uses the histogram to extend the
    "lifetimes" of the bindings accordingly. */
Continuable.prototype.parameterHistogram = function(histogram) {

    this.subtype.parameterHistogram(histogram);

    /* todo bl I don't think a formal parameter can ever occur
     as the lastResultName of a continuation, but this is not enforced
     by the type system. We can remove this once we are sure of this
     invariant. */
    if (histogram[this.continuation.lastResultName] !== undefined)
        ++histogram[this.continuation.lastResultName];

    if (this.continuation.nextContinuable)
        this.continuation.nextContinuable.parameterHistogram(histogram);
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

IdShim.prototype.parameterHistogram = function(histogram) {
    if (this.payload.isIdentifier() && histogram[this.payload.payload] !== undefined)
        ++histogram[this.payload.payload];
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
    if (typeof ans === 'function')
        resultStruct.primitiveName = this.payload.payload;

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
    /* this.consequentFormalHistogram = {};
     this.alternateFormalHistogram = {};
     */
}

Branch.prototype.toString = function(continuation) {
    return '{' + this.test
        + ' ? ' + this.consequent.toString()
        + ' : ' + (this.alternate && this.alternate.toString())
        + ' ' + continuation
        + '}';
};

Branch.prototype.parameterHistogram = function(histogram) {
    if (this.test.isIdentifier() && histogram[this.test.payload] !== undefined)
        ++histogram[this.test.payload];

    /* Branches act as barriers to collecting frequency information
     on formal parameters. The consequent and alternate branches
     might have different frequencies of formal parameters, and there
     is no general way of knowing at definition time which branch will
     be taken. What we can collect is the frequency information on
     each branch up to the next branch node. We store the resulting
     histograms in this branch node, so that at evaluation time, when
     we know which branch is actually taken, we can adopt the correct
     frequency information. */

    var consequentHistogram = {};
    var alternateHistogram = {};

    for (var name in histogram) {
        consequentHistogram[name] = 0;
        alternateHistogram[name] = 0;
    }

    this.consequent.parameterHistogram(consequentHistogram);
    this.alternate.parameterHistogram(alternateHistogram);

    this.consequentFormalHistogram = consequentHistogram;
    this.alternateFormalHistogram = alternateHistogram;

};

// For composition; should only be called from newProcCall
function ProcCall(operatorName, firstOperand) {
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
}

function newProcCall(operatorName, firstOperand, continuation) {
    return new Continuable(new ProcCall(operatorName, firstOperand), continuation);
}

ProcCall.prototype.parameterHistogram = function(histogram) {
    if (histogram[this.operatorName] !== undefined)
        ++histogram[this.operatorName];

    for (var op = this.firstOperand; op; op = op.nextSibling) {
        if (op.isIdentifier() && histogram[op.payload] !== undefined)
            ++histogram[op.payload];
    }
};

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
     this.primitiveName;
     this.currentEnv;
     */
}

// This is the main evaluation function.
function trampoline(continuable, env) {

    console.log('trampoline begins with env ' + env.name);


    var curContinuable = continuable;
    var ans;
    var tmp = new TrampolineResultStruct();

    while (curContinuable) {

        tmp.currentEnv = null;

        console.log('boing: ' + curContinuable);

        curContinuable.subtype.evalAndAdvance(env, curContinuable.continuation, tmp);
        ans = tmp.ans;
        curContinuable = tmp.nextContinuable;
        if (tmp.currentEnv)
            env = tmp.currentEnv;

    }

    /* If we're about to return a JavaScript function, return its name instead.
     (The value of the expression "+" is the primitive function for addition,
     which I wrote in JavaScript, but we should display this value textually
     as "+", not as the text of the function.) */
    return typeof ans === 'function'
        ? newIdOrLiteral(tmp.primitiveName)
        : ans;
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
        if (this.alternateFormalHistogram)
            env.extendBindingLifetimes(this.alternateFormalHistogram);
        this.alternateLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.alternate;
    } else {
        if (this.consequentFormalHistogram)
            env.extendBindingLifetimes(this.consequentFormalHistogram);
        this.consequentLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.consequent;
    }
};

ProcCall.prototype.evalAndAdvance = function(env, continuation, resultStruct) {

    var proc = env.get(this.operatorName);
    var unwrappedProc;
    var args;
    var ans;

    /* Primitive procedure, represented by JavaScript function:
     (+ x y [ans ...]). We perform the action ("+"), bind the
     result to the continuation's result name ("ans"), and advance
     to the next continuable ("..."). */
    if (typeof proc === 'function') {
        args = gatherArgs(this.firstOperand, env);
        ans = proc.apply(null, args);
        env.addBinding(continuation.lastResultName, ans);
        resultStruct.ans = ans;
        resultStruct.nextContinuable = continuation.nextContinuable && continuation.nextContinuable;
        resultStruct.primitiveName = this.operatorName;
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
    else if (proc instanceof Datum && proc.isProcedure()) {
        // todo bl do we need to wrap these in datums anymore?
        unwrappedProc = proc.payload;
        args = gatherArgs(this.firstOperand, env);
        // This will be a no-op if tail recursion is detected
        unwrappedProc.setContinuation(continuation);
        unwrappedProc.checkNumArgs(args.length);
        unwrappedProc.bindArgs(args);
        resultStruct.nextContinuable = unwrappedProc.body;
        resultStruct.currentEnv = unwrappedProc.env;
    }

    else throw new InternalInterpreterError('unrecognized proc '
            + proc
            + ' for name '
            + this.operatorName);
};

function gatherArgs(firstOperand, env) {
    var args = [];
    for (var cur = firstOperand; cur; cur = cur.nextSibling) {
        if (cur.isIdentifier())
            args.push(env.get(cur.payload));
        else if (cur.isQuote())
            args.push(cur.firstChild);
        else if (cur.payload !== undefined)
            args.push(maybeWrapResult(cur.payload, cur.type));
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    return args;
}

