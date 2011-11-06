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

    this.alternateLastContinuable = alternate.continuation.nextContinuable
        ? alternate.continuation.getLastContinuable()
        : alternate;
    this.alternateSavedContinuation = this.alternateLastContinuable.continuation;
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

    if (testDatum.payload === false) {
        if (this.alternateLastContinuable === c.nextContinuable)
            throw new InternalInterpreterError('cycle in alternate');
        this.alternateLastContinuable.continuation = c;
    }
    else {
        if (this.consequentLastContinuable === c.nextContinuable)
            throw new InternalInterpreterError('cycle in consequent');
        this.consequentLastContinuable.continuation = c;
    }
};

Branch.prototype.resetContinuation = function() {
    this.consequentLastContinuable.continuation = this.consequentSavedContinuation;
    this.alternateLastContinuable.continuation = this.alternateSavedContinuation;
};

Branch.prototype.toString = function() {
    return '{' + this.test
        + ' ? ' + this.consequent.toString(true)
        + ' : ' + this.alternate.toString(true)
        + ' ' + this.continuation
        + '}';
};


function ProcCall(operatorName, firstOperand, continuation) {
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
    this.continuation = continuation;
}

ProcCall.prototype.cloneAndResolveOperands = function(env) {
    console.log('resolving operands for ' + this + '\n');

    var clonedFirstOperand = this.firstOperand.clone(); // will clone all the siblings too

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
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        ans += cur.toString() + ' ';
    return ans + this.continuation + ')';
};

function trampolineNew(procOrBranch, env) {

    var cur = procOrBranch;
    var args, ans;

    while (cur) {

        console.log('boing: ' + cur);

        if (cur instanceof ProcCall) {
            var proc = env[cur.operatorName];

            // todo bl shouldn't make it to evaluation?
            if (proc === undefined)
                throw new UnboundVariable(cur.operatorName);

            // Scheme procedure
            else if (proc instanceof Datum && proc.isProcedure()) {
                // todo bl do we need to wrap these in datums anymore?
                var unwrappedProc = proc.payload; // bl must avoid cloning here
                unwrappedProc.resetContinuation();
                args = gatherArgsNew(cur.firstOperand, env, true);
                console.log('continuation needs fixing: ' + cur.continuation);
                unwrappedProc.setContinuation(cur.continuation.cloneAndResolveOperands(env));
                unwrappedProc.checkNumArgs(args.length);
                unwrappedProc.bindArgs(args, env);
                cur = unwrappedProc.body;
            }

            // JavaScript procedure
            else if (typeof proc === 'function') {
                args = gatherArgsNew(cur.firstOperand, env);
                ans = proc.apply(null, args);
                if (cur.continuation.lastResultName) {
                    env[cur.continuation.lastResultName] = ans;
                }
                console.log('bound ' + ans + ' to ' + cur.continuation.lastResultName);
                cur = cur.continuation.nextContinuable;
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

            cur = cur.continuation.nextContinuable;
        }

        else throw new InternalInterpreterError('neither branch nor proc: ' + cur);
    }

    return ans;
}

function gatherArgsNew(firstOperand, env) {
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

