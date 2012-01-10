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
        if (this.nextContinuable.env)
            ans.nextContinuable.setEnv(this.nextContinuable.env);
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

Continuable.prototype.appendContinuable = function(next) {
    this.getLastContinuable().continuation.nextContinuable = next;
    return this;
};

Continuable.prototype.setEnv = function(env, recursive) {
    this.env = env;
    return recursive && this.continuation.nextContinuable
        ? this.continuation.nextContinuable.setEnv(env, true)
        : this;
};

// delegate to subtype, passing in the continuation and environment name for debugging
Continuable.prototype.toString = function() {
    return this.subtype.toString(this.continuation, this.env && this.env.name);
};

// For composition; should only be called from newIdShim
function IdShim(payload) {
    this.payload = payload;
}

/* Just for clarity in debugging, the string representation
 of IdShims looks like a procedure call of an "id" procedure. */
IdShim.prototype.toString = function(continuation, envName) {
    return '(id|' + envName + ' ' + this.payload + ' ' + continuation + ')';
};

IdShim.prototype.evalAndAdvance = function(env, continuation, resultStruct) {

    var ans;

    /* This is only setup by desugarDefinition() called with a macro.
        todo bl: document! */
    if (this.payload instanceof SchemeMacro)
        ans = this.payload;
    else if (typeof this.payload === 'function' || this.payload.isProcedure())
        ans = this.payload;
    else if (this.payload.isIdentifier())
        ans = env.get(this.payload.payload);
    else if (this.payload.isQuote())
        ans = this.payload.firstChild;
    else
        ans = maybeWrapResult(this.payload.payload, this.payload.type);

    if (continuation.nextContinuable) {

        /* If the next Continuable already has an attached environment,
            bind the result in that.

            Example:

            (define (fac n) (if (= n 0) 1 (* n (fac (- n 1)))))
            (fac 3)

            When the recursion terminates, the object on the trampoline will
            look something like

            (id 1 [_0 ; env D: n = 0
                (* n _0 [_1 ; env C: n = 1
                    (* n _1 [_2 ; env B: n = 2
                        (* n _2 ...)])])]) ; env A: n = 3

            We must bind, for example, _1 in environment B, not environment C,
            for the lookup to succeed. */
        if (continuation.nextContinuable.env) {
            continuation.nextContinuable.env.addBinding(continuation.lastResultName, ans);
        }

        /* If the next Continuable does not have an attached environment,
            bind the result in the current environment and attach it. */
        else {
            env.addBinding(continuation.lastResultName, ans);
            continuation.nextContinuable.setEnv(env);
        }
    }

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

Branch.prototype.toString = function(continuation, envName) {
    return '{|'
        + envName
        + this.test
        + ' ? ' + this.consequent.toString()
        + ' : ' + (this.alternate && this.alternate.toString())
        + ' ' + continuation
        + '}';
};

// For composition; should only be called from newProcCall
function ProcCall(operatorName, firstOperand) {
    /* todo bl operatorName is an identifier _datum_...I think
        some call sites might be passing in strings... */
    this.operatorName = operatorName; // an identifier
    this.firstOperand = firstOperand; // identifiers or self-evaluating forms
}

function newProcCall(operatorName, firstOperand, continuation) {
    return new Continuable(new ProcCall(operatorName, firstOperand), continuation);
}

ProcCall.prototype.toString = function(continuation, envName) {
    var ans = '(' + this.operatorName + '|' + envName;
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        ans += ' ' + cur.toString();
    return ans + (continuation ? ' ' + continuation : '') + ')';
};

function TrampolineResultStruct() {
    /*
     this.ans;
     this.nextContinuable;
     */
}

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

    while (curContinuable) {
        // a good first step for debugging: console.log('boing: ' + curContinuable);
        curContinuable.subtype.evalAndAdvance(curContinuable.env, curContinuable.continuation, tmp);
        ans = tmp.ans;
        curContinuable = tmp.nextContinuable;
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
    /* Branches simply forward the current environment to the Continuable
     that is chosen for evaluation. The logic is simpler than for ProcCalls
     or IdShims, so we may be able to eliminate it. */
    resultStruct.nextContinuable.setEnv(env);
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
//    console.log('reconstructed: ' + ans);
    return ans;
};

ProcCall.prototype.operandsInCpsStyle = function() {

    for (var cur = this.firstOperand; cur; cur = cur.nextSibling)
        if (cur instanceof Datum && !cur.isLiteral())
            return false;
    return true;

};

ProcCall.prototype.evalAndAdvance = function(env, continuation, resultStruct) {
    var proc = env.getProcedure(this.operatorName);

    if ((typeof proc === 'function' || proc instanceof SchemeProcedure)
        && !this.operandsInCpsStyle()) {
        var localStructure = new LocalStructure(this.operatorName, this.firstOperand);
        var cpsNames = [];
        var maybeSequenced = this.firstOperand
            && this.firstOperand.sequenceOperands(env, cpsNames);
        var ans = localStructure.toProcCall(maybeSequenced, cpsNames);
        /* Add the continuation to the end of the procedure call.
         This works because the recursive call to setEnv() returns the last
         Continuable in the chain. */
        ans.setEnv(env, true).continuation = continuation;
        resultStruct.nextContinuable = ans;
    }

    else {
        var args = [proc, env, continuation, resultStruct];

        (typeof proc === 'function' && this.tryPrimitiveProcedure.apply(this, args))
            || (proc instanceof SchemeProcedure && this.tryNonPrimitiveProcedure.apply(this, args))
            || (proc instanceof SchemeMacro && this.tryMacroUse.apply(this, args))
            || (proc instanceof Continuation && this.tryContinuation.apply(this, args))
        || this.unrecognizedProc.apply(this, args);
    }
};

ProcCall.prototype.unrecognizedProc = function(proc, env, continuation, resultStruct) {
    throw new InternalInterpreterError(
        'procedure application: expected procedure, given '
        + this.operatorName);
};

/* Primitive procedure, represented by JavaScript function:
     (+ x y [ans ...]). We perform the action ("+"), bind the
     result to the continuation's result name ("ans"), and advance
     to the next continuable ("..."). */
ProcCall.prototype.tryPrimitiveProcedure = function(proc, env, continuation, resultStruct) {

    var args = evalArgs(this.firstOperand, env);

    /* For call/cc etc: push the current ProcCall, the continuation,
     and the result struct. todo bl: pushing the ProcCall invites trouble
     because it contains the _unevaluated_ arguments. When I'm done
     implementing all the 'magical' functions like apply and call/cc,
     review what support they really need. */
    if (proc.hasSpecialEvalLogic) {
        args.push(this);
        args.push(env);
        args.push(continuation);
        args.push(resultStruct);
        proc.apply(null, args);
    }

    else {
        var ans = proc.apply(null, args);
        if (continuation.nextContinuable) {

            /* If the next Continuable already has an attached environment,
                bind the result in that.

                (See logic in IdShim.prototype.evalAndAdvance().)

                We filter out Branches because they always occur in the current
                environment, although their environments are sometimes
                set differently. (todo bl: investigate) */
            if (continuation.nextContinuable.env
                && !(continuation.nextContinuable.subtype instanceof Branch)) {
                continuation.nextContinuable.env.addBinding(continuation.lastResultName, ans);
            }

            /* Otherwise, use the current environment and forward it to the next
             Continuable. */
            else {
                env.addBinding(continuation.lastResultName, ans);
                continuation.nextContinuable.setEnv(env);
            }
        }
        resultStruct.ans = ans;
        resultStruct.nextContinuable = continuation.nextContinuable;
    }

    return true;
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
ProcCall.prototype.tryNonPrimitiveProcedure = function(proc, env, continuation, resultStruct) {

    var args = evalArgs(this.firstOperand, env);

    /* We have to allocate a new Environment object for each procedure
     call, since we have to support a since SchemeProcedure having
     more than one active ProcCall at a time. We point it back to the
     procedure's own environment so it can look up procedure-internal
     definitions.

     todo bl: don't allocate Environments in tail contexts. In theory
     this shouldn't prevent an unlimited number of active tail calls
     (because the old Environment objects will get garbage collected),
     but I would imagine it would make tail recursion impracticable. */
    var newEnv = new Environment('tmp-' + proc.name + '-' + newCpsName(), env);
    newEnv.addAll(proc.env);

    // This will be a no-op if tail recursion is detected
    proc.setContinuation(continuation);
    proc.checkNumArgs(args.length);
    proc.bindArgs(args, newEnv);
    if (proc.body)
        proc.body.setEnv(newEnv);
    resultStruct.nextContinuable = proc.body;
    return true;
};

ProcCall.prototype.tryMacroUse = function(macro, env, continuation, resultStruct) {

    var template = macro.selectTemplate(this.reconstructMacroUse(), env);
    if (!template)
        throw new MacroError(this.operatorName.payload, 'no pattern match for input ' + this);
    var newText = template.hygienicTranscription().toString();

    var newEnv = new MacroUseEnvironment(env, macro.definitionEnv, template);

    // todo bl shouldn't have to go all the way back to the text
    var newContinuable =
        new Parser(
            new Reader(
                new Scanner(newText)
            ).read()
        ).parse('expression')
    .desugar(newEnv, true)
        .setEnv(newEnv);


    newContinuable.getLastContinuable().continuation = continuation;
    resultStruct.nextContinuable = newContinuable;

    /* R5RS 4.3: "If a macro transformer inserts a free reference to an
     identifier, the reference refers to the binding that was visible
     where the transformer was specified, regardless of any local bindings
     that may surround the use of the macro." */

    return true;
};

ProcCall.prototype.tryContinuation = function(proc, env, continuation, resultStruct) {

    env.addBinding(proc.lastResultName, this.firstOperand);
    resultStruct.ans = this.firstOperand;
    resultStruct.nextContinuable = proc.nextContinuable;
    return true;
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

