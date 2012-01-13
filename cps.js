function Continuation(lastResultName) {

    this.lastResultName = lastResultName;

    /* Example: (g (f x y) z) desugared is
     (f x y [f' (g f' z [g' ...])])
     The continuation c is [f' (g f' z [g' ...])]
     c.lastResultName is f'
     c.nextContinuable is (g f' z ...)
     */
}

Continuation.prototype.toString = function(indentLevel) {
    var ans = '[' + this.lastResultName;

    if (this.nextContinuable) {
        for (var i = 0; i < indentLevel; ++i)
            ans += '\t';
        ans += ' ' + this.nextContinuable.toString(indentLevel + 1);
    }
    return ans + ']';
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

Continuable.prototype.setStartingEnv = function(env, recursive) {
    if (this.subtype instanceof ProcCall)
        this.subtype.setEnv(env, true);

    return recursive && this.continuation.nextContinuable
        ? this.continuation.nextContinuable.setStartingEnv(env, true)
        : this;
};

/* The last continuable of a continuable-continuation chain is the first
 continuable c such that c.continuation.nextContinuable is null. */
Continuable.prototype.getLastContinuable = function() {
    if (!this.continuation)
        throw new InternalInterpreterError('invariant violated');
    return this.continuation.nextContinuable
        ? this.continuation.nextContinuable.getLastContinuable()
        : this;
};

Continuable.prototype.appendContinuable = function(next) {
    this.getLastContinuable().continuation.nextContinuable = next;
    return this;
};

// delegate to subtype, passing in the continuation for debugging
Continuable.prototype.toString = function(indentLevel) {
    return this.subtype.toString(this.continuation, indentLevel || 0);
};

/* If a nonterminal in the grammar has no associated desugar function,
 desugaring it will be a no-op. That is often the right behavior,
 but sometimes we would like to wrap the datum in a Continuable
 object for convenience on the trampoline. For example, the program
 "1 (+ 2 3)" should be desugared as (id 1 [_0 (+ 2 3 [_1 ...])]).

 We represent these id shims as ProcCalls whose operatorNames are null
 and whose firstOperand is the payload text (not enclosed in a datum). */
function newIdShim(payload, continuationName) {
    return newProcCall(null, payload, new Continuation(continuationName));
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

Branch.prototype.toString = function(continuation, indentLevel) {
    var ans = '\n';
    for (var i = 0; i < indentLevel; ++i)
        ans += '\t';
    ans += '{' + this.test
        + ' ? '
        + this.consequent.toString(indentLevel + 1)
        + (this.alternate && this.alternate.toString(indentLevel + 1));
    if (continuation)
        ans += ' ' + continuation.toString(indentLevel + 1);
    return ans + '}';
};

// For composition; should only be called from newProcCall
function ProcCall(operatorName, firstOperand) {
    /* todo bl operatorName is an identifier _datum_...I think
        some call sites might be passing in strings... */
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
    // this.env = null;
}

ProcCall.prototype.setEnv = function(env, override) {
    if (this.env && !override)
        throw new InternalInterpreterError('invariant incorrect');
    this.env = env;
};

ProcCall.prototype.clearEnv = function() {
    this.env = null;
};

function newProcCall(operatorName, firstOperand, continuation) {
    return new Continuable(new ProcCall(operatorName, firstOperand), continuation);
}

ProcCall.prototype.toString = function(continuation, indentLevel) {
    var ans = '\n';
    for (var i = 0; i < indentLevel; ++i)
        ans += '\t';
    ans += '(' + (this.operatorName || 'id');
    if (this.env)
        ans += '|' + this.env;
    if (this.operatorName) {
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        ans += ' ' + cur.toString();
    } else {
        ans += ' ' + this.firstOperand;
    }
    if (continuation)
        ans += ' ' + continuation.toString(indentLevel+1);
    return ans + ')';
};

function TrampolineResultStruct() {
    /*
     this.ans;
     this.nextContinuable;
     */
}

TrampolineResultStruct.prototype.clear = function() {
    this.ans = null;
    this.nextContinuable = null;
};

/* This is the main evaluation function.

    The subtlest part is probably the question "what is the current environment?"
    In general, a Continuable object should have an attached Environment
    object that tells it where to look up identifiers. The code that attaches
    Environments to Continuables is scattered about and may be buggy.

 Here is a worked example. Pen and paper is recommended!

 (define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))

 (fac 3 [_0 ...]) ; create new env A where n = 3

 [jump to procedure body, choose consequent]

 (*{env A} n (fac (- n 1)) [_0 ...]) ; this needs to be CPSified

 (-{env A} n 1 [_1
    (fac{env A} _1 [_2
        (*{env A} n _2 [_0 ...])])]) ; CPSified

 [bind _1 = 2 in env A]

 (fac{env A} _1 [_2
    (*{env A} n _2 [_0 ...])]) ; create new env B where n = 2

 [jump to procedure body, choose consequent]

 (*{env B} n (fac (- n 1)) [_2
    (*{env A} n _2 [_0 ...])]) ; this needs to be CPSified

 (-{env B} n 1 [_3
    (fac{env B} _3 [_4
        (*{env B} n _4 [_2
            (*{env A} n _2 [_0 ...])])]) ; CPSified

 [bind _3 = 1 in env B]

 (fac{env B} _3 [_4
    (*{env B} n _4 [_2
        (*{env A} n _2 [_0 ...])])]) ; create new env C where n = 1

 [jump to procedure body, choose consequent]

 (*{env C} n (fac (- n 1)) [_4
    (*{env B} n _4 [_2
        (*{env A} n _2 [_0 ...])])]) ; this needs to be CPSified

 (-{env C} n 1 [_5
    (fac{env C} _5 [_6
        (*{env C} n _6 [_4
            (*{env B} n _4 [_2
                (*{env A} n _2 [_0 ...])])])])]) ; CPSified

 [bind _5 = 0 in env C]

 (fac{env C} _5 [_6
    (*{env C} n _6 [_4
        (*{env B} n _4 [_2
            (*{env A} n _2 [_0 ...])])])]) ; create new env D where n = 0

 [jump to procedure body, choose alternate]

 (id{env D} 1 [_6
    (*{env C} n _6 [_4
        (*{env B} n _4 [_2
            (*{env A} n _2 [_0 ...])])])]) ; bind _6 = 1 in env C

 (*{env C} n _6 [_4
    (*{env B} n _4 [_2
        (*{env A} n _2 [_0 ...])])]) ; bind _4 = 1 in env B

 (*{env B} n _4 [_2
    (*{env A} n _2 [_0 ...])]) ; bind _2 = 2 in env A

 (*{env A} n _2 [_0 ...]) ; bind _0 = 6 in env whatever
 */
function trampoline(continuable) {

    var curContinuable = continuable;
    var ans;
    var tmp = new TrampolineResultStruct();
    var oldEnv;

    while (curContinuable) {
        // a good first step for debugging: console.log('boing: ' + curContinuable);

        if (curContinuable.subtype instanceof ProcCall) {
            var restoreEmptyEnv;
            if (!curContinuable.subtype.env) {
                restoreEmptyEnv = true;
                curContinuable.subtype.setEnv(oldEnv);
            }
            curContinuable.subtype.evalAndAdvance(curContinuable.continuation, tmp);
            oldEnv = curContinuable.subtype.env;
            if (restoreEmptyEnv)
                curContinuable.subtype.clearEnv();
        } else if (curContinuable.subtype instanceof Branch) {
            curContinuable.subtype.evalAndAdvance(oldEnv, curContinuable.continuation, tmp);
        } else throw new InternalInterpreterError('invariant incorrect');

        ans = tmp.ans;
        curContinuable = tmp.nextContinuable;
        tmp.clear();
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

/* todo bl: this may be a leftover from commit 4762893. Prior to that
 commit we optimistically CPSified apparent procedure calls, so if we
 discovered something was actually a macro use we had to do some
 lifting to reconstruct the datums. */
ProcCall.prototype.reconstructMacroUse = function() {
    var ans = newEmptyList();
    ans.appendChild(newIdOrLiteral(this.operatorName));
    var tail = ans.firstChild;
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling) {
        tail.appendSibling(cur.clone().severSibling());
        tail = tail.nextSibling;
    }
    return ans;
};

ProcCall.prototype.operandsInCpsStyle = function() {

    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        if (cur instanceof Datum && !cur.isLiteral())
            return false;
    return true;

};

ProcCall.prototype.tryIdShim = function(willAlwaysBeNull, continuation, resultStruct) {
    var ans;

    var arg = this.firstOperand;

    /* This is only setup by desugarDefinition() called with a macro.
     todo bl: document! */
    if (arg instanceof SchemeMacro)
        ans = arg;
    else if (typeof arg === 'function' || arg.isProcedure())
        ans = arg;
    else if (arg.isIdentifier())
        ans = this.env.get(arg.payload);
    else if (arg.isQuote())
        ans = arg.firstChild;
    else
        ans = maybeWrapResult(arg.payload, arg.type);

    bindCorrectly(continuation.lastResultName, ans, this.env, continuation.nextContinuable);


    /* If we're at the end of the continuable-continuation chain and we're
     trying to return a macro object off the trampoline, that's an error.
     The input was a bare macro name. */
    if (!continuation.nextContinuable && ans instanceof SchemeMacro)
        throw new MacroError(this.firstOperand, 'bad macro syntax');

    resultStruct.ans = ans;
    resultStruct.nextContinuable = continuation.nextContinuable;
};

/* If the operator resolves as a primitive or non-primitive procedure,
 check that the operands are simple. If they're not, rearrange the flow
 of control to compute them first.

 Example: (+ (* 2 3) (/ 4 5)) will need to be turned into something like

 (* 2 3 [_0 (/ 4 5 [_1 (+ _0 _1 [...])])])

 (We do _not_ do this if the operator resolves as a macro. Macros
 get their arguments as unevaluated datums.)
 */
ProcCall.prototype.cpsify = function(proc, continuation, resultStruct) {
    var localStructure = new LocalStructure(this.operatorName, this.firstOperand);
    var cpsNames = [];
    var maybeSequenced = this.firstOperand
        && this.firstOperand.sequenceOperands(this.env, cpsNames);
    var ans = localStructure.toProcCall(maybeSequenced, cpsNames);
    /* The CPSified procedure call will have the same environment as its
     non-CPSified version. */
    ans.setStartingEnv(this.env, true);
    var lastContinuable = ans.getLastContinuable();
    lastContinuable.continuation = continuation;
    resultStruct.nextContinuable = ans;
};

// todo bl it would be nice to actually return the result struct
ProcCall.prototype.evalAndAdvance = function(continuation, resultStruct) {

    var proc = this.operatorName && this.env.getProcedure(this.operatorName);
    var args = [proc, continuation, resultStruct];

    if (!this.operatorName) {
        this.tryIdShim.apply(this, args);
    } else if (typeof proc === 'function') {
        this.tryPrimitiveProcedure.apply(this, args);
    } else if (proc instanceof SchemeProcedure) {
        this.tryNonPrimitiveProcedure.apply(this, args);
    } else if (proc instanceof SchemeMacro) {
        this.tryMacroUse.apply(this, args);
    } else if (proc instanceof Continuation) {
        this.tryContinuation.apply(this, args);
    } else {
        throw new InternalInterpreterError(
            'procedure application: expected procedure, given '
                + this.operatorName);
    }
};

function bindCorrectly(name, val, curEnv, nextContinuable) {
    if (nextContinuable && nextContinuable.subtype instanceof ProcCall) {
        var maybeEnv = nextContinuable.subtype.env;
        if (maybeEnv) {
            maybeEnv.addBinding(name, val);
        } else {
            curEnv.addBinding(name, val);
            nextContinuable.subtype.setEnv(curEnv);
        }
    } else {
        curEnv.addBinding(name, val);
    }
}

/* Primitive procedure, represented by JavaScript function:
     (+ x y [ans ...]). We perform the action ("+"), bind the
     result to the continuation's result name ("ans"), and advance
     to the next continuable ("..."). */
ProcCall.prototype.tryPrimitiveProcedure = function(proc, continuation, resultStruct) {

    if (!this.operandsInCpsStyle()) {
        this.cpsify(proc, continuation, resultStruct);
    }

    else {

        var args = evalArgs(this.firstOperand, this.env);

        /* For call/cc etc: push the current ProcCall, the continuation,
         and the result struct. todo bl: pushing the ProcCall invites trouble
         because it contains the _unevaluated_ arguments. When I'm done
         implementing all the 'magical' functions like apply and call/cc,
         review what support they really need. */
        if (proc.hasSpecialEvalLogic) {
            args.push(this);
            args.push(this.env);
            args.push(continuation);
            args.push(resultStruct);
            proc.apply(null, args);
        }

        else {
            var ans = proc.apply(null, args);
            bindCorrectly(continuation.lastResultName, ans, this.env, continuation.nextContinuable);
            resultStruct.ans = ans;
            resultStruct.nextContinuable = continuation.nextContinuable;
        }
    }
};

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
ProcCall.prototype.tryNonPrimitiveProcedure = function(proc, continuation, resultStruct) {

    if (!this.operandsInCpsStyle()) {
        this.cpsify(proc, continuation, resultStruct);
    }

    else {

        var args = evalArgs(this.firstOperand, this.env);

        /* We have to allocate a new Environment object for each procedure
         call, since we have to support a since SchemeProcedure having
         more than one active ProcCall at a time. We point it back to the
         procedure's own environment so it can look up procedure-internal
         definitions.

         todo bl: don't allocate Environments in tail contexts. In theory
         this shouldn't prevent an unlimited number of active tail calls
         (because the old Environment objects will get garbage collected),
         but I would imagine it would make tail recursion impracticable. */
        var newEnv = new Environment('tmp-' + proc.name + '-' + (uniqueNodeCounter++), this.env);
        newEnv.addAll(proc.env);


        // todo bl mega hack for left-recursive calls
        if (continuation.nextContinuable
            && continuation.nextContinuable.subtype instanceof ProcCall) {
            var procCall = continuation.nextContinuable.subtype;
            if (procCall.operatorName) {
                // a non-left-recursive proc call
                if (procCall.operatorName.payload
                    && procCall.operatorName.payload.charAt(0) !== '@')
                    procCall.setEnv(this.env, true);
            } else {
                // an id shim!
                procCall.setEnv(this.env, true);
            }
        }

        // This will be a no-op if tail recursion is detected
        proc.setContinuation(continuation, this.env);
        proc.checkNumArgs(args.length);
        proc.bindArgs(args, newEnv);
        /* todo bl is it possible to have a procedure body whose first
         continuable is a branch? hopefully not, and I can remove
         the second check. */
        if (proc.body && proc.body.subtype instanceof ProcCall)
            proc.body.subtype.setEnv(newEnv, true);
        resultStruct.nextContinuable = proc.body;
    }
};

ProcCall.prototype.tryMacroUse = function(macro, continuation, resultStruct) {

    var template = macro.selectTemplate(this.reconstructMacroUse(), this.env);
    if (!template)
        throw new MacroError(this.operatorName.payload, 'no pattern match for input ' + this);
    var newText = template.hygienicTranscription().toString();

    var newEnv = new Environment('macro-' + (uniqueNodeCounter++), this.env);

    /* Set up forwarding addresses for free identifiers inserted by the macro.
     R5RS 4.3: "If a macro transformer inserts a free reference to an
     identifier, the reference refers to the binding that was visible
     where the transformer was specified, regardless of any local bindings
     that may surround the use of the macro." */
    for (var free in template.freeIdsInTemplate)
        newEnv.addBinding(free, macro.definitionEnv);

    // todo bl shouldn't have to go all the way back to the text
    var newContinuable = new Parser(
        new Reader(
            new Scanner(newText)
        ).read()
    ).parse().desugar(newEnv, true).setStartingEnv(newEnv);

    newContinuable.getLastContinuable().continuation = continuation;
    resultStruct.nextContinuable = newContinuable;
};

ProcCall.prototype.tryContinuation = function(proc, continuation, resultStruct) {
    this.env.addBinding(proc.lastResultName, this.firstOperand);
    resultStruct.ans = this.firstOperand;
    resultStruct.nextContinuable = proc.nextContinuable;
};

function evalArgs(firstOperand, env) {
    var args = [];

    /* Special logic for values and call-with-values. Example:

        (call-with-values (lambda () (values 1 2 3)) +)

        The "producer" procedure, (lambda () (values 1 2 3)), will desugar to
        something like

        (values 1 2 3 [_0 ...])

        In this implementation, this will bind the JavaScript array [1, 2, 3]
        to _0. Later on the trampoline, we reach (+ _0). We have to know that
        _0 refers to an array of values, not a single value. */
    if (firstOperand instanceof Datum
        && !firstOperand.nextSibling
        && firstOperand.isIdentifier()) {
        var maybeArray = env.get(firstOperand.payload);
        if (maybeArray instanceof Array)
            return maybeArray;
        // Otherwise, fall through to normal logic.
    }

    for (var cur = firstOperand; cur; cur = cur.nextSibling) {
        if (cur instanceof Continuation)
            args.push(cur);
        else if (cur.isIdentifier())
            args.push(env.get(cur.payload));
        else if (cur.isQuote())
            args.push(cur.firstChild);
        else if (cur.payload !== undefined) {
            args.push(maybeWrapResult(cur.payload, cur.type));
        }
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    return args;
}

