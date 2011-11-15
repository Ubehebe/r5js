function Continuation(name) {

    this.lastResultName = name;

    /* (f x y (lambda (f_ans) (g f_ans z (lambda (g_ans) ...))))
     this.lastProcResultName; // f_ans (an identifier)
     this.nextContinuable; // (g f_ans z ...) (a continuable)
     */
}

// inefficient, should not be used during "run" time
Continuation.prototype.getLastContinuable = function() {
    if (!this.nextContinuable)
        throw new InternalInterpreterError('unexpected ' + this);
    if (!this.nextContinuable.continuation.nextContinuable)
        return this.nextContinuable;
    else return this.nextContinuable.continuation.getLastContinuable();
};

Continuation.prototype.toString = function() {
    return '[' + this.lastResultName + ' ' + this.nextContinuable + ']';
};

Continuation.prototype.cloneAndResolveOperands = function(env) {
    var ans = new Continuation(this.lastResultName);
    if (this.nextContinuable)
        ans.nextContinuable = this.nextContinuable.cloneAndResolveOperands(env);
    return ans;
};

function ContinuationWrapper(payload, continuationName) {
    this.payload = payload;
    this.continuation = new Continuation(continuationName);
    this.savedContinuation = this.continuation;
}

ContinuationWrapper.prototype.setContinuation = function(c) {
    this.continuation = c;
};

ContinuationWrapper.prototype.resetContinuation = function() {
    this.continuation = this.savedContinuation;
};

ContinuationWrapper.prototype.toString = function() {
    return '|' + this.payload + ' ' + this.continuation + '|';
};

ContinuationWrapper.prototype.cloneAndResolveOperands = function(env) {
    var ans = new ContinuationWrapper(this.payload.clone());
    ans.continuation = this.continuation.cloneAndResolveOperands(env);
    ans.savedContinuation = ans.continuation;
    return ans;
};

function Branch(test, consequent, alternate, continuation) {
    this.test = test; // an identifier or literal
    this.consequent = consequent; // a continuable
    this.alternate = alternate; // a continuable
    this.continuation = continuation; // a continuation

    this.consequentLastContinuable = consequent.continuation.nextContinuable
        ? consequent.continuation.getLastContinuable()
        : consequent;
    this.consequentSavedContinuation = this.consequentLastContinuable.continuation;

    this.alternateLastContinuable = alternate && alternate.continuation.nextContinuable
        ? alternate.continuation.getLastContinuable()
        : alternate;
    this.alternateSavedContinuation = alternate && this.alternateLastContinuable.continuation;
}

Branch.prototype.cloneAndResolveOperands = function(env) {

    var ans = new Branch(
        this.test.clone(),
        this.consequent.cloneAndResolveOperands(env),
        this.alternate.cloneAndResolveOperands(env),
        this.continuation.cloneAndResolveOperands(env));

    ans.consequentLastContinuable = ans.consequent.continuation.nextContinuable
        ? ans.consequent.continuation.getLastContinuable()
        : ans.consequent;
    ans.consequentSavedContinuation = ans.consequentLastContinuable.continuation;

    ans.alternateLastContinuable = ans.alternate.continuation.nextContinuable
        ? ans.alternate.continuation.getLastContinuable()
        : ans.alternate;
    ans.alternateSavedContinuation = ans.alternateLastContinuable.continuation;

    return ans;

};

Branch.prototype.setContinuation = function(testDatum, c) {
    if (testDatum.payload === false)
        this.alternateLastContinuable.continuation = c;
    else
        this.consequentLastContinuable.continuation = c;
};

Branch.prototype.resetContinuation = function() {
    this.consequentLastContinuable.continuation = this.consequentSavedContinuation;
    this.alternateLastContinuable.continuation = this.alternateSavedContinuation;
};

Branch.prototype.toString = function() {
    return '{' + this.test
        + ' ? ' + this.consequent.toString()
        + ' : ' + (this.alternate && this.alternate.toString())
        + ' ' + this.continuation
        + '}';
};


function ProcCall(operatorName, firstOperand, continuation, isTailContext) {
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
    this.continuation = continuation;
    if (isTailContext)
        this.isTailContext = isTailContext;
}

ProcCall.prototype.cloneAndResolveOperands = function(env) {
    console.log('resolving operands for ' + this + '\n');

    var clonedFirstOperand = this.firstOperand && this.firstOperand.clone(); // will clone all the siblings too

    for (var cur = clonedFirstOperand; cur; cur = cur.nextSibling) {
        if (cur.isIdentifier()) {
            var maybeFound = env[cur.payload];
            if (maybeFound) {
                cur.type = maybeFound.type;
                if (cur.isQuote())
                    cur.firstChild = maybeFound.firstChild;
                else
                    cur.payload = maybeFound.payload;
            }
        }
    }

    return new ProcCall(this.operatorName, clonedFirstOperand, this.continuation.cloneAndResolveOperands(env));
};

ProcCall.prototype.toString = function() {
    var ans = '(' + this.operatorName + ' ';
    if (this.isTailContext)
        ans += 'TAIL ';
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        ans += cur.toString() + ' ';
    return ans + this.continuation + ')';
};

function trampoline(continuable, env) {

    var cur = continuable;
    var args, ans;
    var prevBuiltinName;

    while (cur) {

        console.log('boing: ' + cur);

        if (cur instanceof ProcCall) {
            var proc = env[cur.operatorName];

            // todo bl shouldn't make it to evaluation?
            if (proc === undefined)
                throw new UnboundVariable(cur.operatorName);

            // Scheme procedure: (foo 32)
            else if (proc instanceof Datum && proc.isProcedure()) {
                // todo bl do we need to wrap these in datums anymore?
                var unwrappedProc = proc.payload; // bl must avoid cloning here
                unwrappedProc.resetContinuation();
                args = gatherArgs(cur.firstOperand, env);
                unwrappedProc.setContinuation(cur.continuation.cloneAndResolveOperands(env));
                unwrappedProc.checkNumArgs(args.length);
                unwrappedProc.bindArgs(args, env);
                cur = unwrappedProc.body;
            }

            // JavaScript procedure: (+ 32)
            else if (typeof proc === 'function') {
                prevBuiltinName = cur.operatorName;
                args = gatherArgs(cur.firstOperand, env);
                ans = proc.apply(null, args);
                if (cur.continuation.lastResultName)
                    env[cur.continuation.lastResultName] = ans;
                console.log('bound ' + ans + ' to ' + cur.continuation.lastResultName);
                cur = cur.continuation.nextContinuable;
            }

            // Lambda literal: ((lambda (x) x) 32)
            else if (proc instanceof ContinuationWrapper) {
                var unwrappedProc = proc.payload.payload; // todo bl too much wrapping
                unwrappedProc.resetContinuation();
                args = gatherArgs(cur.firstOperand, env);
                unwrappedProc.setContinuation(cur.continuation.cloneAndResolveOperands(env));
                unwrappedProc.checkNumArgs(args.length);
                unwrappedProc.bindArgs(args, env);
                cur = unwrappedProc.body;
            }

            else throw new InternalInterpreterError(
                    'unexpected value for operator name '
                        + cur.operatorName
                        + ': '
                        + proc);

        }

        else if (cur instanceof Branch) {

            var testResult = cur.test.isIdentifier()
                ? env[cur.test.payload]
                : maybeWrapResult(cur.test, cur.test.type);
            cur.setContinuation(testResult, cur.continuation.cloneAndResolveOperands(env));
            cur = (testResult.payload === false)
                ? cur.alternate
                : cur.consequent;
        }

        else if (cur instanceof ContinuationWrapper) {
            if (cur.payload.isIdentifier())
                ans = env[cur.payload.payload];
            else if (cur.payload.isQuote())
                ans = cur.payload.firstChild;
            else
                ans = maybeWrapResult(cur.payload.payload, cur.payload.type);

            env[cur.continuation.lastResultName] = ans;

            if (typeof ans === 'function')
                prevBuiltinName = cur.payload.payload;

            cur = cur.continuation.nextContinuable;
        }

        else throw new InternalInterpreterError('unknown continuable: ' + cur.toString());
    }

    /* We use SchemeProcedure objects to represent non-primitive Scheme procedures.
        Such objects have a toString method that returns a representation
        suitable for returning to the REPL. On the other hand, we use JavaScript
        functions to represent primitive Scheme procedures, the string
        representation of which is not suitable for returning to the REPL
        (for example, in Chrome it is the whole text of the function). So, if
        we're at the end of the trampoline and about to return a JavaScript
        function, return the corresponding identifier instead.
     */
    return typeof ans === 'function'
        ? newIdOrLiteral(prevBuiltinName)
        : ans;
}

function gatherArgs(firstOperand, env) {
    var args = [];
    for (var cur = firstOperand; cur; cur = cur.nextSibling) {
        if (cur.isIdentifier())
            args.push(env[cur.payload]);
        else if (cur.isQuote())
            args.push(cur.firstChild);
        else if (cur.payload !== undefined)
            args.push(maybeWrapResult(cur.payload, cur.type));
        else throw new InternalInterpreterError('unexpected datum '+ cur);
    }

    return args;
}

