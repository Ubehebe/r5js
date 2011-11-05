function Continuation(name) {

    this.lastProcResultName = name;

    /* (f x y (lambda (f_ans) (g f_ans z (lambda (g_ans) ...))))
     this.lastProcResultName; // f_ans (an identifier)
     this.nextProc; // (g f_ans z ...) (a Branch or ProcCall object)
     */

}

// todo bl inefficient
Continuation.prototype.appendContinuation = function(c) {
    if (!this.nextProc.continuation)
        this.nextProc.continuation = c;
    else
        this.nextProc.continuation.appendContinuation(c);
};

// todo bl inefficient
Continuation.prototype.getContinuationEndpoint = function() {


};

Continuation.prototype.toString = function() {
  return '[' + this.lastProcResultName + ' ' +this.nextProc + ']';
};

function Branch(test, consequent, alternate, continuation) {
     this.test = test;
     this.consequent = consequent;
     this.alternate = alternate;
     this.continuation = continuation;
}

Branch.prototype.toString = function(omitContinuation) {
    return '{' + this.test
        + ' ? ' + this.consequent.toString(true)
        + ' : ' + this.alternate.toString(true)
        + ' ' + (omitContinuation ? '...' : this.continuation) +'}';
};

function ProcCall(operatorName, firstOperand, continuation) {
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
    this.continuation = continuation;
}

ProcCall.prototype.toString = function(omitContinuation) {
    var ans = '(' + this.operatorName + ' ';
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        ans += cur.toString() + ' ';
    return ans + (omitContinuation ? '...' : this.continuation) + ')';
};


function Proc(formalsArray, isDotted, bodyStart, env, name) {
    this.formals = formalsArray;
    this.definitionEnv = env;
    this.body; // ProcCall object
    // todo bl support isDotted and name
}

Proc.prototype.bindArgs = function (args, env) {
    // todo bl
};

function trampolineNew(procOrBranch, env) {

    var curProcOrBranch = procOrBranch;
    var args, ans;

    while (curProcOrBranch) {

        console.log('boing: ' + curProcOrBranch);

        if (curProcOrBranch instanceof ProcCall) {
            var proc = env[curProcOrBranch.operatorName];

            // todo bl shouldn't make it to evaluation?
            if (proc === undefined)
                throw new UnboundVariable(curProcOrBranch.operatorName);

            // Scheme procedure
            else if (proc instanceof Proc) {
                proc.bindArgs(curProcOrBranch.firstOperand, env);
                proc.body.continuation.appendContinuation(curProcOrBranch.continuation);
                curProcOrBranch = proc.body;
            }

            // JavaScript procedure
            else if (typeof proc === 'function') {
                args = gatherArgsNew(curProcOrBranch.firstOperand, env);
                ans = proc.apply(null, args);
                if (curProcOrBranch.continuation.lastProcResultName) {
                    env[curProcOrBranch.continuation.lastProcResultName] = ans;
                    console.log('bound ' + ans + ' to ' + curProcOrBranch.continuation.lastProcResultName);
                }
                curProcOrBranch = curProcOrBranch.continuation.nextProc;
            }

            else throw new InternalInterpreterError(
                    'unexpected value for operator name '
                        + curProcOrBranch.operatorName
                        + ': '
                        + proc);

        } else if (curProcOrBranch instanceof Branch) {

            var testResult = curProcOrBranch.test.isIdentifier()
                ? env[curProcOrBranch.test.payload]
                : maybeWrapResult(curProcOrBranch.test, curProcOrBranch.test.type);
            var savedContinuation = curProcOrBranch.continuation;
            curProcOrBranch = (testResult === false)
                ? curProcOrBranch.alternate
                : curProcOrBranch.consequent;

            // todo bl should not happen for tail recursion!
            curProcOrBranch.continuation.appendContinuation(savedContinuation);
        }

        else throw new InternalInterpreterError('neither branch nor proc: ' + curProcOrBranch);
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

