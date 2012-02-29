function Continuation(lastResultName) {

    this.lastResultName = lastResultName;

    /* Example: (g (f x y) z) desugared is
     (f x y [f' (g f' z [g' ...])])
     The continuation c is [f' (g f' z [g' ...])]
     c.lastResultName is f'
     c.nextContinuable is (g f' z ...)
     */
}

/* Just for call/ccs inside dynamic-winds.
 todo bl: document why we don't have to install the "after" thunk.
 (I'm pretty sure the reason is it's already in the continuable chain
 somewhere.) */
Continuation.prototype.installBeforeThunk = function(before) {
    this.beforeThunk = before;
};

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
     the end of the procedure body. See ProcCall.prototype.maybeSetEnv
     for detailed logic (and maybe bugs). */
    if (this.nextContinuable) {
        var next = this.nextContinuable.subtype;
        if (next instanceof ProcCall)
            next.maybeSetEnv(env);
        /* Somewhat tricky. We can't know in advance which branch we'll take,
         so we set the environment on both branches. Later, when we actually
         decide which branch to take, we must clear the environment on the
         non-taken branch to prevent old environments from hanging around.

         todo bl: it would probably be better to remember the environment on
         the Branch directly. Then Branch.prototype.evalAndAdvance can set the
         environment on the taken branch without having to remember to clear
         it off the non-taken branch. I'll save this for the next time I refactor
         ProcCalls and Branches. (The explicit "subtypes" suggest my command of
         prototypal inheritance wasn't great when I wrote this code.) */
        else if (next instanceof Branch) {
            if (next.consequent.subtype instanceof ProcCall)
                next.consequent.subtype.maybeSetEnv(env);
            if (next.alternate.subtype instanceof ProcCall)
                next.alternate.subtype.maybeSetEnv(env);
        } else throw new InternalInterpreterError('invariant incorrect');
    }
};

Continuable.prototype.setStartingEnv = function(env) {
    if (this.subtype instanceof ProcCall)
        this.subtype.setEnv(env, true);

    return this;
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
    /* If there's no alternate given, we create a shim that will return
     an undefined value. Example:

     (display (if #f 42))

     We give a type of "number" for the shim because passing in a null type
     would activate the default type, identifier, which would change the
     semantics. */
    this.alternate = alternateContinuable
        || newIdShim(newIdOrLiteral(null, 'number'), newCpsName());
    this.consequentLastContinuable = this.consequent.getLastContinuable();
    this.alternateLastContinuable = this.alternate.getLastContinuable();
}

// Just for debugging
Branch.prototype.toString = function(continuation, indentLevel) {

    /* Don't print the continuations at the end of the branches;
     these are probably old and will be overwritten on the next
     this.evalAndAdvance, so they produce misleading debugging info. */

    var tmpConsequent = this.consequentLastContinuable.continuation;
    var tmpAlternate = this.alternateLastContinuable.continuation;

    this.consequentLastContinuable.continuation = null;
    this.alternateLastContinuable.continuation = null;

    var ans = '\n';
    for (var i = 0; i < indentLevel; ++i)
        ans += '\t';
    ans += '{' + this.test
        + ' ? '
        + this.consequent.toString(indentLevel + 1)
        + (this.alternate && this.alternate.toString(indentLevel + 1));
    if (continuation) {
        ans += '\n';
        for (i=0; i < indentLevel; ++i)
            ans += '\t';
        ans += continuation.toString(indentLevel + 1);
    }

    this.consequentLastContinuable.continuation = tmpConsequent;
    this.alternateLastContinuable.continuation = tmpAlternate;

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

ProcCall.prototype.maybeSetEnv = function(env) {
    /* If the ProcCall already has an environment, don't overwrite it.
    Exception: if this is a continuation "escape call", we do overwrite it.
    This exception was prompted by things like

     (call-with-current-continuation
        (lambda (exit)
            (for-each
                (lambda (x)
                    (if (negative? x) (exit x)))
            '(54 0 37 -3 245 19)) #t))

     At the end of each invocation of (lambda (x) ...), the environment on
     (exit x) should be updated to reflect the most recent binding of x.
     Otherwise, the trampoline would see that -3 is negative, but the x in
     (exit x) would still be bound to 54.

     This is quite ad-hoc and could contain bugs. */
  if (!this.env
      || this.env.getProcedure(this.operatorName.payload) instanceof Continuation)
      this.env = env;
};

ProcCall.prototype.clearEnv = function() {
    this.env = null;
};

ProcCall.prototype.isIdShim = function() {
    return !this.operatorName;
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
        ? this.operatorName.payload
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
     this.beforeThunk;
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
function trampoline(continuable, debug) {

    var cur = continuable;
    var resultStruct = new TrampolineResultStruct();
    var savedEnv = new EnvBuffer();
    var ans;

    /* The debug check is hoisted out of the while loop because this
     is expected to be hot code. */
    if (debug) {

        while (cur) {
            // a good first step for debugging:
            console.log('boing: ' + cur);
            resultStruct = cur.subtype.evalAndAdvance(cur.continuation, resultStruct, savedEnv);
            ans = resultStruct.ans;
            cur = resultStruct.nextContinuable;
            resultStruct.clear();
        }

    } else {
        while (cur) {
            resultStruct = cur.subtype.evalAndAdvance(cur.continuation, resultStruct, savedEnv);
            ans = resultStruct.ans;
            cur = resultStruct.nextContinuable;
            resultStruct.clear();
        }
    }

    return ans;
}

Branch.prototype.evalAndAdvance = function(continuation, resultStruct, envBuffer) {

    /* Branches always use the old environment left by the previous action
    on the trampoline. */
    var testResult = this.test.isIdentifier()
        ? envBuffer.get(this.test.payload)
        : maybeWrapResult(this.test, this.test.type).payload;
    if (testResult === false) {
        this.alternateLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.alternate;
        /* We must clear the environment off the non-taken branch.
         See comment at Continuation.prototype.rememberEnv. */
        if (this.consequent.subtype instanceof ProcCall)
            this.consequent.subtype.clearEnv();
    } else {
        this.consequentLastContinuable.continuation = continuation;
        resultStruct.nextContinuable = this.consequent;
        /* We must clear the environment off the non-taken branch.
         See comment at Continuation.prototype.rememberEnv. */
        if (this.alternate.subtype instanceof ProcCall)
            this.alternate.subtype.clearEnv();
    }

    return resultStruct;
};

ProcCall.prototype.reconstructDatum = function() {
    var op = newIdOrLiteral(this.operatorName.payload);
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
                var ans = maybeWrapResult(env.get(node.payload)).maybeDeref();
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
        resultStruct.nextContinuable =
            arg.processQuasiquote(this.env, continuation.lastResultName)
                .appendContinuable(continuation.nextContinuable);
        return;
    } else if (arg.isImproperList()) {
        throw new GeneralSyntaxError(arg);
    } else {
        ans = maybeWrapResult(arg.payload, arg.type);
        if (arg.isImmutable())
            ans.setImmutable();
    }

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

SiblingBuffer.prototype.toList = function(type) {
  var ans = newEmptyList();
    ans.firstChild = this.first;
    if (type)
        ans.type = type; // default is (
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
            finalArgs.appendSibling(arg.clone().normalizeInput());
        else if (arg.isQuasiquote()) {
            if ((maybeContinuable
                = arg.processQuasiquote(this.env, continuation.lastResultName))
                instanceof Continuable) {
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
    ans.setStartingEnv(this.env);
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
     the environment left over from the previous action on the trampoline. */
    if (!this.env) {
        this.setEnv(envBuffer.env);
    }

    var specialOp = this.isSpecialOperator();
    var proc = specialOp ? this.operatorName : this.env.getProcedure(this.operatorName.payload);
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
        throw new EvalError(
            'procedure application: expected procedure, given '
                + this.operatorName);
    }

    /* Save the environment we used in case the next action on the trampoline
     needs it (for example branches, which have no environment of their own). */
    envBuffer.setEnv(this.env);

    // We shouldn't leave the environment pointer hanging around.
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
         environment; it will be carried forward by the EnvBuffer. */
        if (maybeEnv) {
            maybeEnv.addBinding(name, val);
        } else {
            this.env.addBinding(name, val);
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
    this.env.mutate(this.firstOperand.payload, src, this.isTopLevelAssignment);
    /* The return value of an assignment is unspecified,
     but this is not the same as no binding. */
    this.bindResult(continuation, null);
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

        var args = evalArgs(this.firstOperand, this.env, true);

        // todo bl document why we're doing this...
        for (var i =0; i< args.length; ++i) {
            if (args[i] instanceof Datum)
                args[i] = args[i].maybeDeref();
        }

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

        // todo bl we should be able to pass false as the last parameter.
        // need to resolve some bugs.
        var args = evalArgs(this.firstOperand, this.env, true);

        /* If we're at a tail call we can reuse the existing environment.
            Otherwise create a new environment pointing back to the current one.

            todo bl: reenable tail recursion! */
        var newEnv = new Environment('tmp-'
            + proc.name
            + '-'
            + (uniqueNodeCounter++), proc.env).addClosuresFrom(proc.env);

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

    var newEnv = new Environment('macro-' + (uniqueNodeCounter++), this.env);
    var newParseTree = macro.transcribe(this.reconstructDatum(), newEnv);

    /* Just like with tryNonPrimitiveProcedures, we have to remember when
     to jump back to the old environment. */
    continuation.rememberEnv(this.env);

// useful for debugging
// console.log('transcribed ' + this.reconstructDatum() + ' => ' + newDatumTree);

    var newContinuable = newParseTree.desugar(newEnv, true).setStartingEnv(newEnv);

    newContinuable.getLastContinuable().continuation = continuation;
    resultStruct.nextContinuable = newContinuable;
};

ProcCall.prototype.tryContinuation = function(proc, continuation, resultStruct) {
    var arg = evalArgs(this.firstOperand, this.env, false)[0]; // there will only be 1 arg
    this.env.addBinding(proc.lastResultName, arg);
    resultStruct.ans = arg;
    resultStruct.nextContinuable = proc.nextContinuable;

    if (proc.beforeThunk) {
        var before = proc.beforeThunk;
        var cur = proc.nextContinuable;
        before.appendContinuable(cur);
        resultStruct.nextContinuable = before;
        // todo bl is it safe to leave proc.beforeThunk defined?
    }

    /* Cut out the current proc call from the continuation chain to
    avoid an infinite loop. Example:

     (define cont #f)
     (display
     (call-with-current-continuation
     (lambda (c)
     (set! cont c)
     "inside continuation")))
     (cont "outside continuation")
     42

     This should display "inside continuation", then "outside continuation",
     then return 42. When the trampoline is at

     (cont "outside continuation")

     proc.nextContinuable will be something like

     (cont "outside continuation" _0 [_0 (id 42 [_1 ...])])

     We clearly have to cut out the first part of this chain to avoid an
     infinite loop. */
    for (var tmp = resultStruct.nextContinuable, prev;
         tmp;
         prev = tmp,tmp = tmp.continuation.nextContinuable) {
        if (tmp.subtype === this) {
            if (prev)
                prev.continuation.nextContinuable = tmp.continuation.nextContinuable;
            else
                resultStruct.nextContinuable = tmp.continuation.nextContinuable;
            break;
        }
    }
};

function evalArgs(firstOperand, env, wrapArgs) {
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
            var toPush = wrapArgs
                ? maybeWrapResult(env.get(cur.payload))
                : env.get(cur.payload);
            /* Macros are not first-class citizens in Scheme; they cannot
             be passed as arguments. Internally, however, we do just that
             for convenience. The isLetOrLetrecSyntax flag discriminates
             between the programmer and the implementation. */
            if (toPush instanceof SchemeMacro
                && !toPush.isLetOrLetrecSyntax)
                throw new MacroError(cur.payload, 'bad syntax');
            args.push(toPush);
        }
        else if (cur.isQuote()) {
            cur.setImmutableOnQuote();
            args.push(cur.firstChild);
        }
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

