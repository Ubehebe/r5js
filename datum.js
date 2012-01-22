function Datum() {
    /* No need to set this stuff until it's needed, just here for documentation
     this.firstChild = null;
     this.nextSibling = null;
     this.parent = null; // only for last children
     this.type = null;
     this.payload = null;
     this.nonterminals = [];
     this.desugars = null;
    this.nextDesugar = -1;
     this.name = null; // only for procedures
     */
}

// todo bl too many utility functions; reduce to minimal set
Datum.prototype.forEach = function(callback) {
    /* Quotations are like pseudo-leaves in the datum tree, so they should
     be opaque to this function. */
    if (!this.isQuote()) {
        callback(this);
        for (var cur = this.firstChild; cur; cur = cur.nextSibling)
                cur.forEach(callback);
    }
};

// This penetrates quotations because it's used in quasiquote evaluation.
Datum.prototype.replaceChildren = function(predicate, transform) {

    for (var cur = this.firstChild, prev; cur; prev = cur,cur = cur.nextSibling) {
        if (predicate(cur)) {
            var tmp = cur.nextSibling;
            cur.nextSibling = null;
            /* We have to assign to cur so prev will be set correctly
             in the next iteration. */
            if (cur = transform(cur)) {

                if (prev)
                    prev.nextSibling = cur;
                else
                    this.firstChild = cur;

                /* If cur suddenly has a sibling, it must have been inserted
                by the transform. That is, the transform wants to insert
                multiple siblings in place of the single node. (Use case: in

                `(1 ,@(list 2 3) 4)

                the members of the sublist (2 3), not the sublist itself,
                should be inserted into the main list.)

                In this case we should skip ahead to the last sibling inserted
                by the transform in order to avoid accidentally running the
                transform on those newly-inserted siblings, which would
                presumably not be wanted. */
                if (cur.nextSibling)
                    cur = cur.lastSibling();

                cur.nextSibling = tmp;
            }

            /* If transform returned null, that means the current node
            should be spliced out of the list. */
            else {
                prev.nextSibling = tmp;
                cur = prev;
            }
        } else {
            cur.replaceChildren(predicate, transform);
        }
    }
    return this;
};

function newEmptyList() {
    var ans = new Datum();
    ans.type = '(';
    return ans;
}

function newIdOrLiteral(payload, type) {
    // todo bl: we're sometimes creating these with undefined payloads! Investigate.
    var ans = new Datum();
    ans.type = type || 'identifier'; // convenience
    ans.payload = payload;
    return ans;
}

Datum.prototype.isEmptyList = function() {
    return this.isList() && !this.firstChild;
};

Datum.prototype.sameTypeAs = function(other) {
    return this.type === other.type;
};

Datum.prototype.stripParent = function() {
    this.parent = null;
    return this;
};

Datum.prototype.stripSiblings = function() {
    this.nextSibling = null;
    return this;
};

Datum.prototype.clone = function(ignoreSiblings) {

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
    if (this.nextSibling && !ignoreSiblings)
        ans.nextSibling = this.nextSibling.clone();
    if (this.name)
        ans.name = this.name;

    return ans;
};

Datum.prototype.setParse = function(type) {
    if (!this.nonterminals)
        this.nonterminals = [];
    this.nonterminals.push(type);
};

Datum.prototype.setDesugar = function(desugarFunc) {
    if (!this.desugars) {
        this.desugars = [];
        this.nextDesugar = -1;
    }
    this.desugars.push(desugarFunc);
    ++this.nextDesugar;
};

Datum.prototype.unsetParse = function() {
    this.nonterminals = null;
    for (var child = this.firstChild; child; child = child.nextSibling)
        child.unsetParse();
};

Datum.prototype.peekParse = function() {
    if (this.nonterminals) {
        var len = this.nonterminals.length;
        if (len > 0)
            return this.nonterminals[len - 1];
    }

    return null;
};

Datum.prototype.at = function(type) {
    for (var cur = this.firstChild; cur; cur = cur.nextSibling) {
        /* The first clause is a convenience for things like node.at('(');
         the second is a convenience for things like node.at('expression') */
        if (cur.type === type || cur.peekParse() === type)
            return cur;
    }
    return null;
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

Datum.prototype.resetDesugars = function() {
    if (this.nextDesugar === -1)
        this.nextDesugar += this.desugars.length;
};

Datum.prototype.desugar = function(env, forceContinuationWrapper) {
    var desugarFn = this.desugars
        && this.nextDesugar >= 0
        && this.desugars[this.nextDesugar--];
    var ans;
    if (desugarFn)
        ans = desugarFn(this, env);
    else if (this.firstChild && this.firstChild.payload === 'begin') {
        ans = this.firstChild.nextSibling ? this.firstChild.nextSibling.sequence(env) : null;
    }
    else
        ans = this;

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

Datum.prototype.isEnvironmentSpecifier = function() {
    return this.type === 'environment-specifier';
};

Datum.prototype.sequence = function(env) {
    var first, tmp, curEnd;
    for (var cur = this; cur; cur = cur.nextSibling) {
        // todo bl do we need this check anymore?
        if (tmp = cur.desugar(env)) {

            /* Nodes that have no desugar functions (for example, variables
             and literals) desugar as themselves. Sometimes this is OK
             (for example in Datum.sequenceOperands), but here we need to be
             able to connect the Continuable objects correctly, so we
             wrap them. */
            if (!(tmp instanceof Continuable))
                tmp = newIdShim(tmp, newCpsName());

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

// todo bl once we have hidden these types behind functions, we can
// switch their representations to ints instead of strings

function maybeWrapResult(result, type) {

    if (result === null || result instanceof Datum)
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
            case 'string':
                ans.type = 'identifier';
                break;
            case 'object':
                if (result instanceof SchemeString) {
                    ans.type = 'string';
                    ans.payload = result.s;
                    break;
                }

            default:
                throw new InternalInterpreterError('cannot deduce type from value '
                    + result + ': noninjective mapping from values to types');
        }
    }
    return ans;
}

/* todo bl: numChildren and childAt are for manipulating vectors...
    which are currently represented by linked lists!!! Haha. Let's implement
    vectors by something that actually gives random access. */
Datum.prototype.numChildren = function() {
    for (var cur = this.firstChild, ans = 0; cur; cur = cur.nextSibling,++ans)
        ;
    return ans;
};

Datum.prototype.childAt = function(k) {
    for (var cur = this.firstChild; cur && k; cur = cur.nextSibling, --k)
        ;
    return cur;
};

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

Datum.prototype.isLiteral = function() {
    switch (this.type) {
        case 'boolean':
        case 'identifier':
        case 'character':
        case 'number':
        case 'string':
        case 'lambda':
        case "'":
            return true;
        default:
        return false;
    }
};

Datum.prototype.isQuote = function() {
    return this.type === "'"
        || (this.isList()
        && this.firstChild
        && this.firstChild.payload === 'quote'); // todo bl should datums know about this?
};

Datum.prototype.isQuasiquote = function() {
    return this.type === '`';
};

/* In most situations, we want to detect both unquote (,) and
unquote-splicing (,@) */
Datum.prototype.isUnquote = function() {
    return this.type === ',' || this.type === ',@';
};

Datum.prototype.isUnquoteSplicing = function() {
    return this.type === ',@';
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

Datum.prototype.quote = function() {
    var ans = new Datum();
    ans.type = "'";
    ans.firstChild = this;
    return ans;
};

/* Convenience function for builtin evaluation: unwrap the argument if
    it's "primitive". We never unwrap SchemeProcedures or JavaScript functions
    (though not completely sure why not). */
Datum.prototype.unwrap = function() {
    return (this.payload !== undefined
        && !this.isProcedure()
        && !this.isVector()) // watch out for 0's and falses
        ? this.payload
        : this;
};

Datum.prototype.startsWith = function(payload) {
    return this.firstChild && this.firstChild.payload === payload;
};

Datum.prototype.transcribe = function(templateBindings) {
    var prev;
    var first;
    var ellipsisMode = false;
    var curClone;
    var success = false;

    /* todo bl: this loop is a hornet's nest (though thankfully well-contained).
        It needs some serious simplification, perhaps achievable with the new
        SiblingHelper class. */
    for (var cur = this; cur; prev = cur,cur = cur && cur.nextSibling) {

        /* If we're in ellipsis mode, we'll need to clone the current datum
         before trying to transcribe it, so we can re-transcribe it later. */
        if (ellipsisMode
            || (ellipsisMode = (cur.nextSibling && cur.nextSibling.payload === '...'))) {
            curClone = cur.clone(true);
        }

        // Identifiers: nonrecursive case
        if (cur.payload !== undefined) { // watch out for 0's and falses
            var match = templateBindings.get(cur.payload);

            /* If we found some kind of binding for the name, insert it in
                the transcription. There is a corner case, though: in ellipsis
                mode, the TemplateBindings object has to have some way
                of telling us that there are no remaining bindings for the
                name. When this happens, we just need to move on in the
                transcription process. The current way I do this is by
                returning an empty array. This is not necessarily great style,
                I may change it (return false? but that clashes with null for
                no match; return a special sentinel object? but that requires
                more logic...) */
            if (match)
                success = (match.length === 0) ? false : match;

            /* If there were no bindings for the name, this is not an error,
                it just means insert the datum into the transcription
                unaltered. This is actually the common case: for example,

                (define-syntax foo (syntax-rules () ((foo x) (* x x))))

                we want the * to transcribe as itself. */
            else
                success = cur;
        }

        // Lists etc.: recursive case
        else if (cur.firstChild) {
            success = cur.firstChild.transcribe(templateBindings);
            if (success !== false) { // watch out: null is an empty list
                cur.firstChild = success;
                success = cur;
            }
        }

        // Don't forget the empty list!
        else {
            success = cur;
        }

        if (success !== false) {

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


            // Holy smokes
            var tmp = cur.nextSibling && cur.nextSibling.nextSibling;
            cur = prev;
            if (cur)
                cur.nextSibling = tmp;

        }

        /* If transcription failed and we weren't in ellipsis mode,
         that's just plain failure. Report it. */
        else {
            return false;
        } // todo bl better error message

        if (first === undefined)
            first = cur;
    }
    return first;
};

Datum.prototype.lastSibling = function() {
    return this.nextSibling ? this.nextSibling.lastSibling() : this;
};

/*
    (x . ()) is equivalent to (x). It is useful to perform this normalization
    prior to evaluation time to simplify the Scheme procedure "list?".
    With normalization, we can merely say, (list? x) iff x.isList().
    Without normalization, we would also have to check if x is an
    improper list, and if so, whether its last element was an empty list.

    This is also an opportune time to do these:

    (quote x) -> 'x
    (quasiquote x) -> `x
    (unquote x) -> ,x
    (unquote-splicing x) -> ,@x

    so we don't have to worry about these synonyms during evaluation proper. */
Datum.prototype.normalizeInput = function() {

    if (this.firstChild) {
        switch (this.firstChild.payload) {
            case 'quote':
                this.type = "'";
                this.firstChild = this.firstChild.nextSibling;
                break;
            case 'quasiquote':
                this.type = "`";
                this.firstChild = this.firstChild.nextSibling;
                break;
            case 'unquote':
                this.type = ',';
                this.firstChild = this.firstChild.nextSibling;
                break;
            case 'unquote-splicing':
                this.type = ',@';
                this.firstChild = this.firstChild.nextSibling;
                break;
        }
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

/* Example:
    `(a `(b ,(+ x y) ,(foo ,(+ z w) d) e) f)

    should be decorated as

    `1(a `2(b ,2(+ x y) ,2(foo ,1(+ z w) d) e) f) */
Datum.prototype.decorateQuasiquote = function(qqLevel) {

    if (this.isQuasiquote()) {
        this.qqLevel = qqLevel;
    } else if (this.isUnquote()) {
        this.qqLevel = qqLevel+1;
    }

    for (var cur = this.firstChild; cur; cur = cur.nextSibling) {
        if (cur.isQuasiquote()) {
            cur.decorateQuasiquote(qqLevel+1);
        } else if (cur.isUnquote()) {
            cur.decorateQuasiquote(qqLevel-1);
        } else {
            cur.decorateQuasiquote(qqLevel);
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

Datum.prototype.changeProcToName = function() {
  if (!this.isProcedure())
      throw new InternalInterpreterError('not a procedure: ' + this);
    this.type = 'identifier';
    this.payload = this.name;
};

function newCpsName() {
    return cpsPrefix + (uniqueNodeCounter++);
}

function newAnonymousLambdaName() {
    return 'proc' + (anonymousLambdaCounter++);
}

// todo bl encapsulate these in a global object
var uniqueNodeCounter = 0;
var anonymousLambdaCounter = 0;
// Not a valid identifier prefix so we can easily tell these apart
var cpsPrefix = '@';

// Example: `(1 ,(+ 2 3)) should desugar as (+ 2 3 [_0 (id (1 _0) [_2 ...])])
Datum.prototype.processQuasiquote = function(env) {

    var newCalls = new ContinuableHelper();

    var qqLevel = this.qqLevel;

    this.replaceChildren(
        function(node) {
            return node.isUnquote() && (node.qqLevel === qqLevel);
        },
        function(node) {
            var asContinuable = new Parser(node.firstChild).parse('expression').desugar(env, true);
            var continuation = asContinuable.getLastContinuable().continuation;
            /* Throw out the last result name and replace it with another
             identifier (also illegal in Scheme) that will let us know if it's
             unquotation or unquotation with splicing. */
            continuation.lastResultName = node.type + (uniqueNodeCounter++);
            newCalls.appendContinuable(asContinuable);
            return newIdOrLiteral(continuation.lastResultName);
        });

        this.type = "'";

    newCalls.appendContinuable(newIdShim(this, newCpsName()));
    var ans = newCalls.toContinuable();
    return ans && ans.setStartingEnv(env);
};

Datum.prototype.shouldUnquote = function() {
    return this.isIdentifier() && this.payload.charAt(0) === ',';
};

/* This is a subcase of shouldUnquote, because unquotes
and unquote-splicings have pretty much the same logic. */
Datum.prototype.shouldUnquoteSplice = function() {
    return this.isIdentifier() && this.payload.charAt(1) === '@';
};

/* Munges definitions to get them in a form suitable for let-type
bindings. Example:

(define (foo x y z) ...) => (foo (lambda (x y z) ...))

todo bl: I think there are some bugs with improper lists. Something
I didn't notice until recently is that

(define (foo . xs) ...)

has no legal lambda-form. I'm not quite sure why this isn't causing
my dotted-list tests to fail. */
Datum.prototype.extractDefinition = function() {
    var variable = this.at('variable');
    var list = newEmptyList();
    if (variable) {
        list.prependChild(this.at('expression').clone(true));
    } else {
        var formalsList = this.firstChild.nextSibling;
        variable = formalsList.firstChild;
        var bodyStart = formalsList.nextSibling;
        var lambda = newEmptyList();
        lambda.firstChild = bodyStart;
        var newFormalsList = formalsList.clone(true);
        newFormalsList.firstChild = newFormalsList.firstChild.nextSibling;
        if (newFormalsList.isImproperList() && !newFormalsList.firstChild.nextSibling)
            lambda.prependChild(newIdOrLiteral(newFormalsList.firstChild.payload));
        else
            lambda.prependChild(newFormalsList);
        lambda.prependChild(newIdOrLiteral('lambda'));
        list.prependChild(lambda);
    }
    list.prependChild(variable.clone(true));
    return list;
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

