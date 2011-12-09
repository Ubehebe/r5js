function Datum() {
    /* No need to set this stuff until it's needed, just here for documentation
     this.firstChild = null;
     this.nextSibling = null;
     this.parent = null; // only for last children
     this.type = null;
     this.payload = null;
     this.nonterminals = [];
     this.desugars = null;
     this.name = null; // only for procedures
     */
}

// todo bl too many utility functions; reduce to minimal set
Datum.prototype.forEach = function(f) {
    /* Quotations are like pseudo-leaves in the datum tree, so they should
     be opaque to this function. */
    if (!this.isQuote()) {
        f(this);
        for (var cur = this.firstChild; cur; cur = cur.nextSibling)
            if (!cur.isQuote())
                f(cur);
    }
};

function newEmptyList() {
    var ans = new Datum();
    ans.type = '(';
    return ans;
}

function newIdOrLiteral(payload, type) {
    var ans = new Datum();
    ans.type = type || 'identifier'; // convenience
    ans.payload = payload;
    return ans;
}

Datum.prototype.isEmptyList = function() {
    return this.isList() && !this.firstChild;
};

Datum.prototype.stripParent = function() {
    this.parent = null;
    return this;
};

Datum.prototype.stripSiblings = function() {
    this.nextSibling = null;
    return this;
};

Datum.prototype.clone = function() {

    var ans = new Datum();

    if (this.type)
        ans.type = this.type;
    if (this.parent)
        ans.parent = this.parent;
    if (this.payload !== undefined) // watch out for 0's and falses
        ans.payload = this.payload;
    if (this.nonterminals)
        ans.nonterminals = shallowArrayCopy(this.nonterminals);
    if (this.firstChild)
        ans.firstChild = this.firstChild.clone();
    if (this.nextSibling)
        ans.nextSibling = this.nextSibling.clone();
    if (this.name)
        ans.name = this.name;

    return ans;
};

Datum.prototype.severSibling = function() {
    this.nextSibling = null;
    return this;
};

Datum.prototype.setParse = function(type) {
    if (!this.nonterminals)
        this.nonterminals = [];
    this.nonterminals.push(type);
};

Datum.prototype.setDesugar = function(desugarFunc) {
    if (!this.desugars)
        this.desugars = [];
    this.desugars.push(desugarFunc);
};

Datum.prototype.unsetParse = function() {
    this.nonterminals = null;
    for (var child = this.firstChild; child; child = child.nextSibling)
        child.unsetParse();
};

Datum.prototype.sanitize = function() {
    this.parent = null;
    this.unsetParse();
    return this;
};

Datum.prototype.peekParse = function() {
    if (this.nonterminals) {
        var len = this.nonterminals.length;
        if (len > 0)
            return this.nonterminals[len - 1];
    }

    return null;
};

Datum.prototype.matchChild = function(predicate) {
    for (var child = this.firstChild; child; child = child.nextSibling)
        if (predicate(child))
            return child;
    return null;
};

Datum.prototype.at = function(type) {
    var ans = this.matchChild(function(datum) {
	/* The first clause is a convenience for things like node.at('(');
	   the second is a convenience for things like node.at('expression') */
        return datum.type === type || datum.peekParse() === type;
    });
    /* If there is no match, we return a fake Datum for convenience. This function
     is often followed by evalSiblings, and calling new Datum().evalSiblings() gives
     []. This is just what we want in the case of an empty list. */
    return ans ? ans : new Datum();
};

Datum.prototype.appendSibling = function(sibling) {
    if (!this.nextSibling) {
        if (this.parent) {
            // Propagate the parent field
            sibling.parent = this.parent;
            // Only the last sibling needs a link back to the parent
            this.parent = null;
        }
        this.nextSibling = sibling;
    }
    else
        this.nextSibling.appendSibling(sibling);
};

/* If we used this to append n children in a row, it would take time O(n^2).
 But we don't actually use it like that. When building a list like (X*), we build
 up the list of X's in linear time, then call appendChild once to append the
 whole list as a child of the list root. We do incur some overhead when building
 a list like (X+ . X): in this case, the X+ list is appended in one go, and then
 we have to re-traverse that list once to append the final X. I expect this to be
 rare enough not to matter in practice, but if necessary we could keep track of
 the root's final child. */
Datum.prototype.appendChild = function(child) {
    if (!this.firstChild)
        this.firstChild = child;
    else this.firstChild.appendSibling(child);
};

// todo bl deprecate in favor of prependSiblings
Datum.prototype.prependChild = function(child) {
    var oldFirstChild = this.firstChild;
    this.firstChild = child;
    child.nextSibling = oldFirstChild;
};

Datum.prototype.prependSiblings = function(firstSibling) {
    var lastSibling = firstSibling.lastSibling();
    var oldFirstChild = this.firstChild;
    this.firstChild = firstSibling;
    lastSibling.nextSibling = oldFirstChild;
};

/* Map isn't the best word, since the function returns an array but the children
 are represented as a linked list. */
Datum.prototype.mapChildren = function(f) {
    var ans = [];
    for (var cur = this.firstChild; cur; cur = cur.nextSibling)
        ans.push(f(cur));
    return ans;
};

// Convenience functions
Datum.prototype.isImproperList = function() {
    return this.type === '.(';
};

Datum.prototype.desugar = function(env, forceContinuationWrapper) {
    var desugarFn = this.desugars && this.desugars.pop();
    var ans = desugarFn ? desugarFn(this, env) : this;
    if (forceContinuationWrapper && !(ans instanceof Continuable))
        ans = newIdShim(ans, newCpsName());
    return ans;
};

function newProcedureDatum(name, procedure) {
    var ans = new Datum();
    ans.type = 'lambda';
    ans.payload = procedure;
    ans.name = name;
    return ans;
}

Datum.prototype.isProcedure = function() {
  return this.type === 'lambda';
};

Datum.prototype.sequenceOperands = function(env, cpsNames) {
    var first, tmp, curEnd;
    for (var cur = this; cur; cur = cur.nextSibling) {
        /* This check is necessary because node.desugar can return null for some
         nodes (when it makes sense for the node to drop off the tree before
         evaluation, e.g. for definitions). */
        if (tmp = cur.desugar(env)) {
            if (cpsNames && tmp instanceof Continuable)
                cpsNames.push(tmp.getLastContinuable().continuation.lastResultName);

            if (tmp instanceof Continuable) {
                if (!first)
                    first = tmp;
                else if (curEnd)
                    curEnd.nextContinuable = tmp;

                curEnd = tmp.getLastContinuable().continuation;
            }
        }
    }

    return first; // can be undefined

};

Datum.prototype.sequence = function(env, isTopLevel) {
    var first, tmp, curEnd;
    for (var cur = this; cur; cur = cur.nextSibling) {
        /* 5.1: "At the top level of a program (begin <form1> ...) is
            equivalent to the sequence of expressions, definitions, and
            syntax definitions that form the body of the begin."

            5.2.2: "Wherever an internal definition may occur
            (begin <definition1> ...) is equivalent to the sequence of
            definitions that form the body of the begin."

            What we do on finding a begin block as a child of node N is
            just read the children of the begin block as children of N.

            Note that the standard gives a couple macro definitions of
            begin, but this confusing sentence made me unsure of whether
            they are adequate definitions:

            7.3: "In any case, note that these rules [=macros?] apply only if
            the body of the begin contains no definitions."
         */
        if (cur.isList() && cur.firstChild.payload === 'begin') {
            var startBeginBlock = cur.firstChild.nextSibling;
            if (startBeginBlock) {
                var endBeginBlock = startBeginBlock.lastSibling();
                endBeginBlock.parent = null;
                endBeginBlock.nextSibling = cur.nextSibling;
                cur = startBeginBlock;
                // fallthrough to the desugar block
            } else continue; // if it's just (begin), do nothing
        }

        // todo bl do we need this check anymore?
        if (tmp = cur.desugar(env)) {

            /* Nodes that have no desugar functions (for example, variables
             and literals) desugar as themselves. Sometimes this is OK
             (for example in Datum.sequenceOperands), but here we need to be
             able to connect the Continuable objects correctly, so we
             wrap them. */
            if (!(tmp instanceof Continuable))
                tmp = newIdShim(tmp, newCpsName());

            /* If there was a DefinitionHelper object attached to this node,
             we need to run some special logic to set up the bindings. */
            if (tmp.definitionHelper) {
                isTopLevel
                    ? constructTopLevelDefs(env, tmp.definitionHelper, cur.nextSibling)
                    : constructInternalDefs(env, tmp.definitionHelper, cur.nextSibling);
                /* This is not a short circuit: the remaining siblings are
                 sequenced recursively by the above function call. */
                return tmp;
            }

            if (!first)
                first = tmp;
            else if (curEnd) {
                curEnd.nextContinuable = tmp;
            }

            curEnd = tmp.getLastContinuable().continuation;
        }
    }

    return first; // can be undefined
};

/* Example:
 (define x (+ 1 2))
 (define y (+ x 4))
 (* x y)

 Such a construction (where y depends on x) is not permitted in
 internal definitions. */
function constructTopLevelDefs(env, definitionHelper, next) {

    var proc = new SchemeProcedure(definitionHelper.formals, false, null, env, definitionHelper.getProcName());
    if (definitionHelper.mustAlreadyBeBound)
        proc.setMustAlreadyBeBound(definitionHelper.mustAlreadyBeBound);
    if (next) {
        var rest = next.sequence(proc.env, true);
        proc.setBody(rest);
    }
    /* Consider what happens when we are at a top-level definition,
        but there is nothing more to sequence, for example, the program

        (define x 1)

        This sets up an empty binding, like

        ((lambda (x) <nothing>) 1)

        and its value is explicitly undefined by the standard. Recognizing that
        such a definition is useless, one option would be to decline to
        execute it. However, if we ever wanted to build in REPL functionality,
        this would become quite useful. So we keep the dummy procedure with
        no body.

        When the trampoline encounters the dummy procedure call, it will
        do some bookkeeping and then jump to the first continuable of the
        procedure's body. This is null, which will halt the trampoline as
        desired; we just need to make sure the trampoline bookkeeping
        doesn't cause any null pointer exceptions. */
    env.addBinding(definitionHelper.getProcName(), proc);
}

/* 5.2.2: "it must be possible to evaluate each <expression> of every
 internal definition in a <body> without assigning or referring to the
 value of any <variable> being defined."

 For example, in a non-top-level context,

 (define x 1)
 (define y 2)
 (+ x y)

 could be interpreted as

 ((lambda (x y) (+ x y)) 1 2)

 but it could NOT be interpreted as

 ((lambda (x) ((lambda (y) (+ x y)) 2)) 1)

 because this would allow something like

 (define x 1)
 (define y x)
 (+ x y)

 But implementations vary. In MIT Scheme it is legal.
 */
function constructInternalDefs(env, definitionHelper, next) {
    var rest = next.sequence(env, false);

    /* If the rest of the sequence contained a definition, we have to aggregate
     the definitions. Example:

     (define x (+ 1 2)) => (+ 1 2 [_0 (proc0 _0 [...])])
     (define y (+ 3 4)) => (+ 3 4 [_1 (proc1 _1 [...])])
     (+ x y)

     The base case is at (define y (+ 3 4)). This will install proc1 with
     body (+ x y) in the environment.

     The recursive case is at (define x (+ 1 2)). What we need to do here is
     combine proc0 and proc1 so that we get

     (+ 1 2 [_0 (+ 3 4 [_1 (proc0 _0 _1 [...])])])

     This involves several updates:
     (a) Prepend a formal parameter x to proc0's parameters
     (b) Insert the argument "_0" in the actual call to proc0
     (c) Replace the end of x's continuable chain with y's continuable chain
     */
    if (rest.definitionHelper) {
        // Takes care of steps (a) and (b)
        rest.definitionHelper.incorporateOuterDef(definitionHelper);
        // Takes care of step (c)
        definitionHelper.setContinuable(rest);
    }

    else {
        var proc = new SchemeProcedure(definitionHelper.formals, false, null, env, definitionHelper.getProcName());
        proc.setBody(rest);
        env.addBinding(definitionHelper.getProcName(), proc);
    }
}

DefinitionHelper.prototype.getSoleCPSName = function() {
    if (this.formals.length !== 1)
        throw new InternalInterpreterError('invariant incorrect');
    return this.precedingContinuable.continuation.lastResultName;
};

DefinitionHelper.prototype.incorporateOuterDef = function(outerDefHelper) {
    this.prependBinding(outerDefHelper.getSoleFormal(), outerDefHelper.getSoleCPSName());
    if (outerDefHelper.mustAlreadyBeBound) {
        if (!this.mustAlreadyBeBound)
            this.mustAlreadyBeBound = {};
        for (var name in outerDefHelper.mustAlreadyBeBound)
            this.mustAlreadyBeBound[name] = true;
    }
};

DefinitionHelper.prototype.setContinuable = function(continuable) {
    this.precedingContinuable.continuation.nextContinuable = continuable;
};


// todo bl once we have hidden these types behind functions, we can
// switch their representations to ints instead of strings

function maybeWrapResult(result, type) {

    if (result instanceof Datum)
        return result; // no-op, strictly for convenience

    var ans = new Datum();
    ans.payload = result;
    if (type)
        ans.type = type;
    // If no type was supplied, we can deduce it in most (not all) cases
    else {
        var inferredType = typeof result;
        switch (inferredType) {
            case 'boolean':
            case 'number':
                ans.type = inferredType;
                break;
            default:
                throw new InternalInterpreterError('cannot deduce type from value '
                    + result + ': noninjective mapping from values to types');
        }
    }
    return ans;
}

Datum.prototype.isList = function() {
    return this.type === '(';
};

Datum.prototype.isVector = function() {
    return this.type === '#(';
};

Datum.prototype.isBoolean = function() {
    return this.type === 'boolean';
};

Datum.prototype.isIdentifier = function() {
    return this.type === 'identifier';
};

Datum.prototype.isCharacter = function() {
    return this.type === 'character';
};

Datum.prototype.isNumber = function() {
    return this.type === 'number';
};

Datum.prototype.isString = function() {
    return this.type === 'string';
};

Datum.prototype.isQuote = function() {
    return this.type === "'"
        || (this.isList()
        && this.firstChild
        && this.firstChild.payload === 'quote'); // todo bl should datums know about this?
};

/* todo bl this could be written in Scheme (as equals?). I wrote it
 in JavaScript because we have to call it when doing macro processing.
 */
Datum.prototype.isEqual = function(other) {
    if (other instanceof Datum
        && this.type === other.type
        && this.payload === other.payload) {
        var thisChild, otherChild;
        for (thisChild = this.firstChild,otherChild = other.firstChild;
             thisChild && otherChild;
             thisChild = thisChild.nextSibling,otherChild = otherChild.nextSibling)
            if (!thisChild.isEqual(otherChild))
                return false;

        return !(thisChild || otherChild);

    } else return false;
};

/* Convenience function for builtin evaluation: unwrap the argument if
    it's "primitive". We never unwrap SchemeProcedures or JavaScript functions
    (though not completely sure why not). */
Datum.prototype.unwrap = function() {
    return this.payload !== undefined && !this.isProcedure() // watch out for 0's and falses
        ? this.payload
        : this;
};

Datum.prototype.startsWith = function(payload) {
    return this.firstChild && this.firstChild.payload === payload;
};

Datum.prototype.transcribe = function(bindings) {
    var prev;
    var first;
    var ellipsisMode = false;
    var curClone;
    var success = false;

    for (var cur = this; cur; prev = cur,cur = cur && cur.nextSibling) {

        /* If we're in ellipsis mode, we'll need to clone the current datum
         before trying to transcribe it, so we can re-transcribe it later. */
        if (ellipsisMode = ellipsisMode
            || (cur.nextSibling && cur.nextSibling.payload === '...')) {
            var savedNextSibling = cur.nextSibling;
            cur.nextSibling = null;
            curClone = cur.clone();
            cur.nextSibling = savedNextSibling;
        }


        // Identifiers: nonrecursive case
        if (cur.payload !== undefined) { // watch out for 0's and falses
            var matches = bindings[cur.payload];
            success = matches ? matches.shift() : cur;
        }

        // Lists etc.: recursive case
        else if (cur.firstChild) {
            success = cur.firstChild.transcribe(bindings);
            if (success !== false) { // watch out: null is an empty list
                cur.firstChild = success;
                success = cur;
            }
        }

        // Don't forget the empty list!
        else {
            success = cur;
        }

        if (success) {

            if (success !== cur) {
                if (cur.parent)
                    success.parent = cur.parent;
                if (prev)
                    prev.nextSibling = success;
            }

            /* If transcription succeeded in ellipsis mode, append the
             datum clone and try to transcribe it again on the next loop. */
            if (ellipsisMode) {
                curClone.nextSibling = cur.nextSibling;
                success.nextSibling = curClone;
            }

            // Otherwise simply proceed.
            else {
                success.nextSibling = cur.nextSibling;
            }
            cur = success;
        }

        /* If transcription failed but we are in ellipsis mode,
         turn off ellipsis mode, get rid of the current (failed) clone
         datum, and proceed. */
        else if (ellipsisMode) {
            ellipsisMode = false;
            var ellipsis = cur.nextSibling;

            if (prev) {
                prev.nextSibling = ellipsis.nextSibling;
                if (ellipsis.parent)
                    prev.parent = ellipsis.parent;
            } else {
                first = null;
            }

            cur.nextSibling = cur.nextSibling && cur.nextSibling.nextSibling;
        }

        /* If transcription failed and we weren't in ellipsis mode,
         that's just plain failure. Report it. */
        else return false; // todo bl better error message

        if (first === undefined)
            first = cur;
    }
    return first;
};

Datum.prototype.filterChildren = function(predicate) {
    var prev;
    for (var cur = this.firstChild; cur; cur = cur.nextSibling) {
        if (predicate(cur)) {
            var newCur = cur.filterChildren(predicate);
            if (prev)
                prev.nextSibling = newCur;
            else
                this.firstChild = newCur;
            prev = newCur;
        } else if (prev) {
            prev.nextSibling = null;
        }
    }

    if (prev)
        prev.parent = this;

    // singleton
    else if (this.firstChild && !predicate(this.firstChild))
        this.firstChild = null;

    return this;
};

Datum.prototype.lastSibling = function() {
    return this.nextSibling ? this.nextSibling.lastSibling() : this;
};

/*
    (x . ()) is equivalent to (x). It is useful to perform this normalization
    prior to evaluation time to simplify the Scheme procedure "list?".
    With normalization, we can merely say, (list? x) iff x.isList().
    Without normalization, we would also have to check if x is an
    improper list, and if so, whether its last element was an empty list. */
Datum.prototype.normalizeInput = function() {

    if (this.firstChild && this.firstChild.payload === 'quote') {
        this.firstChild = this.firstChild.nextSibling;
        this.type = "'";
    }

    var isImproperList = this.isImproperList();

    for (var child = this.firstChild; child; child = child.nextSibling) {
        child.normalizeInput();
        if (isImproperList && child.nextSibling && !child.nextSibling.nextSibling) {
            var maybeEmptyList = child.nextSibling;
            if (maybeEmptyList.isList() && !maybeEmptyList.firstChild) {
                child.parent = child.nextSibling.parent;
                child.nextSibling = null;
                this.type = '(';
            }
        }
    }

    return this;
};

/* Notice that our representation of lists is not recursive: the "second element"
 of (x y z) is y, not (y z). So we provide this function as an aid whenever
 we want that recursive property (which in practice is seldom).

 Why not use car/cdr-style lists? I've gone back and forth (and back) on this, and
 may still change my mind. The current first-child/next-sibling representation
 works; I'm sure I'd break something (especially with the hacky "parent" pointers)
 by changing to car/cdr. Also, car/cdr would double memory usage for lists.
 Consider this list as an example: (x y (1 2)). In first-child/next-sibling, this
 allocates six Datum objects, one for each atom and one for the head of each list.
 In car/cdr, this would allocate five Pair objects and four Atom objects (they would
 have to be a separate datatype).

 Also, first-child/next-sibling is easier to draw by hand, something that's important
 to me.
 */
Datum.prototype.siblingsToList = function(dotted) {
    var ans = new Datum();
    ans.type = dotted ? '.(' : '(';
    ans.firstChild = this;
    this.lastSibling().parent = ans;
    return ans;
};

function newCpsName() {
    return '_' + (uniqueNodeCounter++);
}

function newAnonymousLambdaName() {
    return 'proc' + (anonymousLambdaCounter++);
}

var uniqueNodeCounter = 0; // todo bl any good way to encapsulate this?
var anonymousLambdaCounter = 0;

function LocalStructure(operatorNode, firstOperand) {
    if (!operatorNode.isIdentifier())
        throw new InternalInterpreterError('expected identifier, got ' + operatorNode);

    this.bindings = [];
    this.operatorName = operatorNode.payload;

    for (var cur = firstOperand; cur; cur = cur.nextSibling) {
        if (cur.isQuote()) {
            var tmp = cur.nextSibling;
            cur.severSibling();
            /* Why are we normalizing this datum again? Didn't we already do
                that in the datum's desugar function? Unfortunately, we haven't
                called that desugar function yet -- the reason we create a
                LocalStructure object is to preserve local structure that might
                be destroyed by sequencing the operands (which includes
                desugaring them). This seems like a pretty bad design. */
            this.bindings.push(cur.clone().normalizeInput());
            cur.nextSibling = tmp;
        } else if (cur.isList()) {
            this.bindings.push(null); // a placeholder until we know the name
        } else {
            this.bindings.push(newIdOrLiteral(cur.payload, cur.type));
        }
    }
}

LocalStructure.prototype.toProcCall = function(cpsNames) {

    var idOrLiteralNode;
    var firstArg, lastArg;

    for (var i=0; i<this.bindings.length; ++i) {
        idOrLiteralNode = this.bindings[i] || newIdOrLiteral(cpsNames.shift());
        if (!firstArg)
            firstArg = idOrLiteralNode;
        if (lastArg)
            lastArg.appendSibling(idOrLiteralNode);
        lastArg = idOrLiteralNode;
    }

    return newProcCall(this.operatorName, firstArg, new Continuation(newCpsName()));

};

LocalStructure.prototype.toString = function() {
    var ans = '{' + this.operatorName + ' ';
    for (var i=0; i<this.bindings.length; ++i)
        ans += (this.bindings[i] || '_') + ' ';
    return ans + '}';
};

/*
 Continuation-passing style is
 <cps-expr> -> <cps-procedure-call> | <cps-branch>
 <cps-procedure-call> -> (<operator> <operand>* <continuation>)
 <operator> -> <identifier>
 <operand> -> <identifier> | <self evaluating>
 <continuation> -> (lambda (<identifier>) <cps-expr>?)
 <cps-branch> -> (if <cps-test> <cps-consequent> <cps-alternate>?)
 <cps-test> -> <cps-expr>
 <cps-consequent> -> <cps-expr>
 <cps-alternate> -> <cps-expr>
 (Not in the spec anywhere, I'm just trying to reduce the grammar to simplify
 evaluation.)
 */

function DefinitionHelper(continuableToTarget, precedingContinuable, firstFormal, isAssignment) {
    this.procCallToTarget = continuableToTarget;
    this.precedingContinuable = precedingContinuable;
    this.formals = [firstFormal];
    if (isAssignment) {
        this.mustAlreadyBeBound = {};
        this.mustAlreadyBeBound[firstFormal] = true;
    }
}

DefinitionHelper.prototype.getProcName = function() {
    return this.procCallToTarget.subtype.operatorName;
};

DefinitionHelper.prototype.prependBinding = function(formal, actual) {
    this.formals.unshift(formal);
    var oldFirstOp = this.procCallToTarget.subtype.firstOperand;
    var newFirstOp = newIdOrLiteral(actual);
    this.procCallToTarget.subtype.firstOperand = newFirstOp;
    newFirstOp.nextSibling = oldFirstOp;
};

DefinitionHelper.prototype.getSoleFormal = function() {
    if (this.formals.length !== 1)
        throw new InternalInterpreterError('invariant incorrect');
    return this.formals[0];
};