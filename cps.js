function Continuation(lastResultName) {

    this.lastResultName = lastResultName;

    /* Example: (g (f x y) z) desugared is
     (f x y [f' (g f' z [g' ...])])
     The continuation c is [f' (g f' z [g' ...])]
     c.lastResultName is f'
     c.nextContinuable is (g f' z ...)
     */
}

// Just for debugging
Continuation.prototype.toString = function(indentLevel) {

    if (indentLevel == null) {
        /* If no indent level is given, this function is being used to
         construct an external representation, so we should hide all the
         implementation details. It's legal to return continuations directly,
         as in

         (define x 3)
         (call-with-current-continuation (lambda (c) (set! x c)))
         x
         */
        return '[continuation]';
    } else {

        // Otherwise this is being used for debugging, show all the things.

        var ans = '[' + this.lastResultName;

        if (this.nextContinuable) {
            for (var i = 0; i < indentLevel; ++i)
                ans += '\t';
            ans += ' ' + this.nextContinuable.toString(indentLevel + 1);
        }
        return ans + ']';
    }
};

/* todo bl: Continuable was originally envisioned as the parent
 type of objects on the trampoline. Originally we had three subtypes:
 ProcCall, Branch, and IdShim. But IdShim was turned into a subtype of
 ProcCall in order to take advantage of ProcCall's environment-handling
 logic. Subsequently other "procedure-call-like" entities (assignments,
 for example) have been written as ProcCalls. So it may no longer make
 sense to create both a Continuable and a ProcCall object for most
 things on the trampoline. */
function Continuable(subtype, continuation) {
    if (!subtype || !continuation) // todo bl take out after testing
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype = subtype;
    this.continuation = continuation;
    //this.lastContinuable = this.getLastContinuable(); // todo bl caching problems
}

/* This returns null if the very next Continuable isn't a ProcCall
(in particular, if it is a Branch). */
Continuation.prototype.getAdjacentProcCall = function() {
    return this.nextContinuable
        && this.nextContinuable.subtype instanceof ProcCall
        && this.nextContinuable.subtype;
};

Continuation.prototype.rememberEnv = function(env) {
   /* In general, we need to remember to jump out of the newEnv at
    the end of the procedure body, and this means setting the env on the
    continuable following the current continuation (if if is a procedure call).

    There are two important exceptions. First, if that procedure call already
    has an attached environment, it's already keeping track of a prior
    context, so we should not disturb it. Second, if that procedure call is
    left-recursive, that means the procedure body we're about to jump into
    is going to return something that is going to be used as an operator.
    We must stay in that procedure's environment to use its bindings.
    Example:

    ((foo x) (bar y))

    desugars incrementally as

    (foo x [_0 (_0 (bar y) [_1 ...])])

    When the trampoline is at (foo x ...), it sees the result is
    going to be called as a procedure. Thus we shouldn't set the env
    of (_0 ...) to be the old env; instead, leave it blank, and it will
    eventually be filled in as newEnv (the env that is set up for foo's
    procedure body).

    todo bl: I discovered this logic after the fact, so it is unlikely
    to be watertight. If either of the following two things start going
    wrong, here would be a good place to investigate:

    (1) Site of left recursion is not immediately next to this continuation.
    I think the incremental nature of CPSification makes this impossible,
    but I am not 100% sure. If it were possible to produce

    (foo x [_0 ... (_0 ...)])

    for example, we would probably be in trouble.

    (2) If continuation.nextContinuable is not a ProcCall
    (if it is a Branch), the environment will not be set. I don't observe
    this problem in practice, but perhaps I haven't thought of a good
    example, or perhaps the environment is getting set correctly in a way
    I don't understand. */

    if (this.nextContinuable) {
        var next = this.nextContinuable.subtype;
        if (next instanceof ProcCall) {
            if (!next.isLeftRecursion() && !next.env)
                next.setEnv(env);
        } else if (next instanceof Branch) {
            next.consequent.setStartingEnv(env);
            if (next.alternate)
                next.alternate.setStartingEnv(env);
        } else throw new InternalInterpreterError('invariant incorrect');
    }
};

Continuable.prototype.setStartingEnv = function(env, recursive) {
    if (this.subtype instanceof ProcCall)
        this.subtype.setEnv(env, true);

    return recursive && this.continuation.nextContinuable
        ? this.continuation.nextContinuable.setStartingEnv(env, true)
        : this;
};

Continuable.prototype.setTopLevelAssignment = function() {
    if (!(this.subtype instanceof ProcCall
        && this.subtype.operatorName === ProcCall.prototype.specialOps._set))
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype.isTopLevelAssignment = true;
    return this;
};

Continuable.prototype.setSyntaxAssignment = function() {
    if (!(this.subtype instanceof ProcCall
        && this.subtype.operatorName === ProcCall.prototype.specialOps._set))
        throw new InternalInterpreterError('invariant incorrect');
    this.subtype.isSyntaxAssignment = true;
    return this;
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
 and whose firstOperand is the payload. */
function newIdShim(payload, continuationName) {
    return newProcCall(ProcCall.prototype.specialOps._id, payload, new Continuation(continuationName));
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

// Just for debugging
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

ProcCall.prototype.isIdShim = function() {
    return !this.operatorName;
};

/* If a procedure call is explicitly left-recursive, a CPS-style identifier,
illegal as an identifier in Scheme, will make an appearance as an operator.

Example: ((foo x) (bar y)) => (foo x [_0 (bar y [_1 (_0 _1 [_2 ...])])]) */
ProcCall.prototype.isLeftRecursion = function() {
    return this.operatorName
        && this.operatorName.payload
        && this.operatorName.payload.charAt(0) === cpsPrefix;
};

function newProcCall(operatorName, firstOperand, continuation) {
    return new Continuable(new ProcCall(operatorName, firstOperand), continuation);
}

function newAssignment(dstName, srcName, continuation) {
    var operands = new SiblingBuffer()
        .appendSibling(newIdOrLiteral(dstName))
        .appendSibling(newIdOrLiteral(srcName))
        .toSiblings();

    return newProcCall(ProcCall.prototype.specialOps._set, operands, continuation);
}

// Just for debugging
ProcCall.prototype.toString = function(continuation, indentLevel, suppressEnv) {
    var ans = '\n';
    for (var i = 0; i < indentLevel; ++i)
        ans += '\t';
    ans += '(' + (this.operatorName instanceof Datum
        ? this.operatorName
        : this.specialOps.names[this.operatorName]);
    if (this.env && !suppressEnv)
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

/* Just a pointer to an environment. It's separate from the
 TrampolineResultStruct to make it clear that old environments are only
 reused in a few situations. */
function EnvBuffer() {
    this.env = null;
}

EnvBuffer.prototype.setEnv = function(env) {
    this.env = env;
};

EnvBuffer.prototype.get = function(name) {
    return this.env.get(name);
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

    var cur = continuable;
    var resultStruct = new TrampolineResultStruct();
    var savedEnv = new EnvBuffer();
    var ans;

    while (cur) {
        // a good first step for debugging: console.log('boing: ' + cur);
        resultStruct = cur.subtype.evalAndAdvance(cur.continuation, resultStruct, savedEnv);
        ans = resultStruct.ans;
        cur = resultStruct.nextContinuable;
        resultStruct.clear();
    }

    return ans;
}

Branch.prototype.evalAndAdvance = function(continuation, resultStruct, envBuffer) {

    /* Branches always use the old environment left by the previous action
    on the trampoline. */
    var testResult = this.test.isIdentifier()
        ? envBuffer.get(this.test.payload)
        : maybeWrapResult(this.test, this.test.type);
    if (testResult.payload === false) {
        this.alternateLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.alternate;
    } else {
        this.consequentLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.consequent;
    }

    return resultStruct;
};

ProcCall.prototype.reconstructDatum = function() {
    var op = newIdOrLiteral(this.operatorName);
    op.nextSibling = this.firstOperand;
    var ans = newEmptyList();
    ans.appendChild(op);
    return ans;
};

ProcCall.prototype.operandsInCpsStyle = function() {
    for (var cur = this.firstOperand; cur; cur = cur.nextSibling) {
        if (cur instanceof Datum) {
            if (cur.isEmptyList())
                throw new IllegalEmptyApplication(this.operatorName.payload);
            else if (!cur.isLiteral())
                return false;
        }
    }
    return true;
};

ProcCall.prototype.tryIdShim = function(continuation, resultStruct) {
    var ans;

    var arg = this.firstOperand;

    /* todo bl: id shims have become quite popular for passing through
     disparate objects on the trampoline. The logic could be made clearer. */
    if (arg instanceof SchemeMacro)
        ans = arg;
    else if (typeof arg === 'function' || arg.isProcedure())
        ans = arg;
    else if (arg.isIdentifier())
        ans = this.env.get(arg.payload);
    else if (arg.isQuote()) {
        var env = this.env;
        // Do the appropriate substitutions.
        ans = arg.replaceChildren(
            function(node) {
                return node.shouldUnquote();
            },
            function(node) {
                var ans = env.get(node.payload).clone();
                if (node.shouldUnquoteSplice()) {
                    if (ans.isList()) {
                        if (ans.firstChild) // `(1 ,@(list 2 3) 4) => (1 2 3 4)
                            ans = ans.firstChild;
                        else // `(1 ,@(list) 2) => (1 2)
                            ans = null;
                    } else throw new QuasiquoteError(ans + ' is not a list');
                }
                return ans;
            });
        // Now strip away the quote mark.
        ans = ans.firstChild;
    }
    else if (arg.isQuasiquote()) {
        /* todo bl do I understand how the continuation is being preserved
         properly? We're not passing it in here at all... */
        resultStruct.nextContinuable = arg.processQuasiquote(this.env);
        return;
    } else if (arg.isImproperList()) {
        throw new GeneralSyntaxError(arg);
    } else
        ans = maybeWrapResult(arg.payload, arg.type);

    this.bindResult(continuation, ans);

    /* If we're at the end of the continuable-continuation chain and we're
     trying to return a macro object off the trampoline, that's an error.
     The input was a bare macro name. */
    if (!continuation.nextContinuable && ans instanceof SchemeMacro)
        throw new MacroError(this.firstOperand, 'bad macro syntax');

    resultStruct.ans = ans;
    resultStruct.nextContinuable = continuation.nextContinuable;
};

/* Just a buffer to accumulate siblings without the client having to do
 the pointer arithmetic. */
function SiblingBuffer() {
    //this.first;
    // this.last;
}

SiblingBuffer.prototype.isEmpty = function() {
    return !this.first;
};

SiblingBuffer.prototype.appendSibling = function(node) {
    if (node) {
        if (!this.first) {
            this.first = node;
            this.last = node.lastSibling();
        } else {
            this.last.nextSibling = node;
            this.last = node.lastSibling();
        }
    }
    return this;
};

SiblingBuffer.prototype.toSiblings = function() {
    return this.first;
};

SiblingBuffer.prototype.toList = function() {
  var ans = newEmptyList();
    ans.firstChild = this.first;
    return ans;
};

SiblingBuffer.prototype.toString = function() {
    var tmp = newEmptyList();
    tmp.appendChild(this.first);
    return tmp.toString();
};

/* Just a buffer to accumulate a Continuable-Continuation chain
 without the client having to do the pointer arithmetic. */
function ContinuableHelper() {
    // this.firstContinuable;
    // this.lastContinuable;
}

ContinuableHelper.prototype.appendContinuable = function(continuable) {

    if (!this.firstContinuable) {
        this.firstContinuable = continuable;
        this.lastContinuable = continuable.getLastContinuable();
    } else {
        this.lastContinuable.continuation.nextContinuable = continuable;
        this.lastContinuable = continuable.getLastContinuable();
    }
};

ContinuableHelper.prototype.toContinuable = function() {
    return this.firstContinuable;
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

    var newCallChain = new ContinuableHelper();
    var finalArgs = new SiblingBuffer();
    var maybeContinuable;

    for (var arg = this.firstOperand; arg; arg = arg.nextSibling) {
        arg.resetDesugars();
        if (arg.isQuote())
            finalArgs.appendSibling(arg.clone(true).normalizeInput());
        else if (arg.isQuasiquote()) {
            if ((maybeContinuable = arg.processQuasiquote(this.env)) instanceof Continuable) {
                finalArgs.appendSibling(
                    newIdOrLiteral(maybeContinuable
                        .getLastContinuable()
                        .continuation
                        .lastResultName));
                newCallChain.appendContinuable(maybeContinuable);
            } else {
                /* R5RS 4.2.6: "If no commas appear within the <qq template>,
                 the result of evaluating `<qq template> is equivalent to
                 the result of evaluating '<qq template>." We implement this
                 merely by switching the type of the datum from quasiquote (`)
                 to quote ('). evalArgs will see the quote and evaluate it
                 accordingly. */
                arg.type = "'";
                finalArgs.appendSibling(arg);
            }
        } else if (arg.isProcedure()) {
            finalArgs.appendSibling(newIdOrLiteral(arg.name));
        } else if (arg.isImproperList()) {
            throw new GeneralSyntaxError(arg);
        } else if ((maybeContinuable = arg.desugar(this.env)) instanceof Continuable) {
            /* todo bl is it an invariant violation to be a list
             and not to desugar to a Continuable? */
            finalArgs.appendSibling(newIdOrLiteral(maybeContinuable.getLastContinuable().continuation.lastResultName));
            newCallChain.appendContinuable(maybeContinuable);
        } else {
            finalArgs.appendSibling(newIdOrLiteral(arg.payload, arg.type));
        }
    }

    newCallChain.appendContinuable(
        newProcCall(this.operatorName, finalArgs.toSiblings(), new Continuation(newCpsName()))
    );

    var ans = newCallChain.toContinuable();
    ans.setStartingEnv(this.env, true);
    var lastContinuable = ans.getLastContinuable();
    lastContinuable.continuation = continuation;
    resultStruct.nextContinuable = ans;
};

/* Things like set! are represented as ProcCalls whose
operators are the small integers in ProcCall.prototype.specialOps
rather than Datum objects. */
ProcCall.prototype.isSpecialOperator = function() {
    return !(this.operatorName instanceof Datum);
};

ProcCall.prototype.evalAndAdvance = function(continuation, resultStruct, envBuffer) {

    /* If the procedure call has no attached environment, we use
     the environment left over from the previous action on the trampoline,
     but remember to restore the state of the procedure call when we're done. */
    var restoreEmptyEnv;
    if (!this.env) {
        restoreEmptyEnv = true;
        this.setEnv(envBuffer.env);
    }

    var specialOp = this.isSpecialOperator();
    var proc = specialOp ? this.operatorName : this.env.getProcedure(this.operatorName);
    var args = [proc, continuation, resultStruct];

    if (specialOp) {
        this.specialOps.logic[args.shift()].apply(this, args);
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

    /* Save the environment we used in case the next action on the trampoline
     needs it (for example branches, which have no environment of their own). */
    envBuffer.setEnv(this.env);

    if (restoreEmptyEnv)
        this.clearEnv();

    return resultStruct;
};

ProcCall.prototype.bindResult = function(continuation, val) {

    var name = continuation.lastResultName;
    var nextProcCall = continuation.getAdjacentProcCall();

    if (nextProcCall) {
        var maybeEnv = nextProcCall.env;
        /* If the next procedure call already has an environment,
         bind the result there. Otherwise, bind it in the current
         environment and forward that environment to the
         next procedure call. */
        if (maybeEnv) {

            /* If we're about to return a procedure into another environment,
             we have to remember the current environment. Example:

             (define (compose f g) (lambda (x) (f (g x))))
             (define caar (compose car car))

             caar will return (lambda (x) (f (g x))), but it has to
             remember f and g are both bound to car.

             todo bl: prove that this is sufficient. What if maybeEnv
             isn't set by now? Could this happen? */
            if (val instanceof Datum
                && val.isProcedure()
                && maybeEnv !== this.env)
                val.setClosure(this.env);

            maybeEnv.addBinding(name, val);
        } else {
            this.env.addBinding(name, val);
            nextProcCall.setEnv(this.env);
        }
    }

    /* If the next thing is not a procedure call, it will reuse this procedure
     call's environment, so just bind the result here. */
    else {
        this.env.addBinding(name, val);
    }
};

ProcCall.prototype.tryAssignment = function(continuation, resultStruct) {
    var src = this.env.get(this.firstOperand.nextSibling.payload);
    /* In Scheme, macros can be bound to identifiers but they are not really
     first-class citizens; you cannot say

     (define x let)

     because the text "let" does not parse as an expression
     (at least if it has its normal binding). In this implementation, however,
     SchemeMacros are objects that go into and come out of Environments
     like any other kind of objects. All kinds of assignments -- top-level,
     internal, syntax, non-syntax -- go through this function, so we
     have to make sure we don't accidentally permit some illegal behavior.

     If we're trying to assign a SchemeMacro object but the isSyntaxAssignment
     flag on the ProcCall object hasn't been set, then the programmer is
     requesting this assignment and we ought to signal an error.

     The situation is complicated a bit because internally, we use let and
     letrec to implement let-syntax and letrec-syntax. In other words,
     we as the implementer do exactly what we forbid the programmer to do.
     We tell the difference between the two parties via the isLetOrLetrecSyntax
     flag on the SchemeMacro object, which only the implementation can set. */
    if (src instanceof SchemeMacro
        && !src.isLetOrLetrecSyntax
        && !this.isSyntaxAssignment)
        throw new GeneralSyntaxError(this);
    this.env.mutate(this.firstOperand.payload, this.env.get(this.firstOperand.nextSibling.payload), this.isTopLevelAssignment);
    resultStruct.nextContinuable = continuation.nextContinuable;
};

/* Primitive procedure, represented by JavaScript function:
     (+ x y [ans ...]). We perform the action ("+"), bind the
     result to the continuation's result name ("ans"), and advance
     to the next continuable ("..."). */
ProcCall.prototype.tryPrimitiveProcedure = function(proc, continuation, resultStruct) {

    /* If the operands aren't simple, we'll have to take a detour to
    restructure them. Example:

    (+ (* 1 2) (/ 3 4)) => (* 1 2 [_0 (/ 3 4 [_1 (+ _0 _1 ...)])]) */
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
            args.push(continuation);
            args.push(resultStruct);
            proc.apply(null, args);
        }

        else {
            var ans = proc.apply(null, args);
            this.bindResult(continuation, ans);
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

    /* If the operands aren't simple, we'll have to take a detour to
    restructure them. */
    if (!this.operandsInCpsStyle()) {
        this.cpsify(proc, continuation, resultStruct);
    }

    else {

        var args = evalArgs(this.firstOperand, this.env);

        /* If we're at a tail call we can reuse the existing environment.
            Otherwise create a new environment pointing back to the current one. */
        var newEnv = proc.isTailCall(continuation)
            ? this.env.allowRedefs()
            : new Environment('tmp-'
            + proc.name
            + '-'
            + (uniqueNodeCounter++), this.env).addAll(proc.env);

        /* Remember to discard the new environment
        at the end of the procedure call. */
        continuation.rememberEnv(this.env);

        // Do some bookkeepping to prepare for jumping into the procedure
        proc.setContinuation(continuation);
        proc.checkNumArgs(args.length);
        proc.bindArgs(args, newEnv);
        proc.setEnv(newEnv);

        // And away we go
        resultStruct.nextContinuable = proc.body;
    }
};

ProcCall.prototype.tryMacroUse = function(macro, continuation, resultStruct) {

    var template = macro.selectTemplate(this.reconstructDatum(), this.env);
    if (!template)
        throw new MacroError(this.operatorName.payload, 'no pattern match for input ' + this.toString(null, 0, true));
    var newDatumTree = template.hygienicTranscription(this.env);

    var newEnv = new Environment('macro-' + (uniqueNodeCounter++), this.env);

// useful for debugging
// console.log('transcribed ' + this.reconstructDatum() + ' => ' + newDatumTree);

    var toRename = {};

    /* R5RS 4.3: "If a macro transformer inserts a binding for an identifier
     (variable or keyword), the identifier will in effect be renamed
     throughout its scope to avoid conflicts with other identifiers.

     "If a macro transformer inserts a free reference to an
     identifier, the reference refers to the binding that was visible
     where the transformer was specified, regardless of any local bindings
     that may surround the use of the macro."

     It's easy to collect the set of identifiers inserted by a macro transformer:
     it's the set of identifiers in the template minus the set of identifiers
     in the pattern. But how do we determine which of these are supposed
     to be "free" and which are bindings and thus should be renamed?

     My current heuristic is to do a lookup in the macro's definition
     environment. If we find something, the identifier is probably
     supposed to refer to that. For example, the "+" in the pattern of

     (define-syntax foo (syntax-rules () ((foo x) (+ x x))))

     If we don't find a binding in the macro's definition environment, we
     suppose this is a new binding inserted by the transformer and
     defensively rename it.

     I don't think this is correct, but it works for the letrec macro definition,
     which is the most complex case I've tried so far. */
    for (var free in template.freeIdsInTemplate) {
        if (macro.definitionEnv.hasBindingRecursive(free))
            newEnv.addBinding(free, macro.definitionEnv);
         else
            toRename[free] = newCpsName();
    }

    var newParseTree = new Parser(newDatumTree).parse();

    if (newParseTree) {
        /* We have to embed the new parse tree in a fake shell to do the
         replacement in case the entire newParseTree is an identifier that
         needs to be replaced (Datum.prototype.replaceChildren() only
         looks at a node's children).

         This is a problem that has surfaced more than once, so perhaps
         there is a better way to write replaceChildren.

         todo bl: we should be able to determine the id's in the template
         that will have to be renamed prior to transcription. That would
         save the following tree walk replacing all the identifiers. */
        var fake = newEmptyList();
        fake.appendChild(newParseTree);
        fake.replaceChildren(
            function(node) {
                return node.isIdentifier() && toRename[node.payload];
            },
            function(node) {
                node.payload = toRename[node.payload];
                return node;
            }
        );
    }

    var newContinuable = newParseTree.desugar(newEnv, true).setStartingEnv(newEnv);

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

    // todo bl too much logic
    for (var cur = firstOperand; cur; cur = cur.nextSibling) {
        if (cur instanceof Continuation)
            args.push(cur);
        else if (cur.isIdentifier()) {
            var toPush = env.get(cur.payload);
            /* Macros are not first-class citizens in Scheme; they cannot
             be passed as arguments. Internally, however, we do just that
             for convenience. The isLetOrLetrecSyntax flag discriminates
             between the programmer and the implementation. */
            if (toPush instanceof SchemeMacro
                && !toPush.isLetOrLetrecSyntax)
                throw new MacroError(cur.payload, 'bad syntax');
            args.push(toPush);
        }
        else if (cur.isQuote())
            args.push(cur.firstChild);
        else if (cur.isProcedure()) {
            args.push(cur);
        } else if (cur.payload !== undefined) {
            args.push(maybeWrapResult(cur.payload, cur.type));
        }
        else throw new InternalInterpreterError('unexpected datum ' + cur);
    }

    return args;
}

/* This is my attempt at a JavaScript enum idiom. Is there a better way to
 get guaranteed constant-time lookup given an ordinal? */
ProcCall.prototype.specialOps = {

    _id: 0,
    _set: 1,

    names: ['id', 'set!'],
    logic: [
        ProcCall.prototype.tryIdShim,
        ProcCall.prototype.tryAssignment
    ]
};

