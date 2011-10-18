function Datum() {
    /* No need to set this stuff until it's needed, just here for documentation
     this.firstChild = null;
     this.nextSibling = null;
     this.parent = null; // only for last children
     this.type = null;
     this.payload = null;
     this.nonterminals = [];
     this.values = []; */
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

// 3.5
Datum.prototype.isTailContext = function() {
    var headOfList = this.parent
        && this.parent.firstChild
        && this.parent.firstChild.payload;

    /* Base case: the last expression within the body of a lambda expression
     occurs in a tail context. */
    if (headOfList === 'lambda')
        return true;

    /* Inductive cases: if (if test consequent alternate) or (if test consequent)
     are in tail context, then consequent and alternate are in tail context. */
    else if ((headOfList === 'if'
        || headOfList === 'let-syntax'
        || headOfList === 'letrec-syntax') && this.parent.isTailContext())
        return true;

    // Ugh, special check for consequent
    else if (
        (headOfList = this.nextSibling
            && this.nextSibling.parent
            && this.nextSibling.parent.firstChild
            && this.nextSibling.parent.firstChild.payload
            )
            && headOfList === 'if'
            && this.nextSibling.parent.isTailContext())
        return true;

    else return false;
};

function newEmptyList() {
    var ans = new Datum();
    ans.type = '(';
    return ans;
}

function newId(name) {
    var ans = new Datum();
    ans.type = 'identifier';
    ans.payload = name;
    return ans;
}

function newProcedureDatum(procedure) {
    var ans = new Datum();
    ans.type = 'lambda';
    ans.payload = procedure;
    return ans;
}

function newContinuationLambda(name) {
    var soleFormal = newEmptyList();
    soleFormal.appendChild(newId(name));
    var ans = new Datum();
    ans.type = '(';
    ans.prependChild(soleFormal);
    ans.prependChild(newId('lambda'));
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
    if (this.values)
        ans.values = shallowArrayCopy(this.values);
    if (this.firstChild)
        ans.firstChild = this.firstChild.clone();
    if (this.nextSibling)
        ans.nextSibling = this.nextSibling.clone();

    return ans;
};

Datum.prototype.setParse = function(type) {
    if (!this.nonterminals)
        this.nonterminals = [];
    this.nonterminals.push(type);
};

Datum.prototype.setValue = function(semanticAction) {
    if (!this.values)
        this.values = [];
    this.values.push(semanticAction);
};

Datum.prototype.unsetParse = function() {
    this.nonterminals = null;
    this.values = null;
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

Datum.prototype.nextSiblingRecursive = function() {
    return this.nextSibling || (this.parent && this.parent.nextSiblingRecursive());
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

Datum.prototype.prependChild = function(child) {
    var oldFirstChild = this.firstChild;
    this.firstChild = child;
    child.nextSibling = oldFirstChild;
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

Datum.prototype.isProcedure = function() {
    return this.type === 'lambda';
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

// Convenience function for builtin evaluation: unwrap the argument if it's "primitive"
Datum.prototype.unwrap = function() {
    return this.payload !== undefined // watch out for 0's and falses
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

var uniqueNodeCounter = 0; // todo bl any good way to encapsulate this?

Datum.prototype.cpsifyLocal = function(rootName, cpsNames) {

    if (this.isList() && this.firstChild) {
        if (this.firstChild.isList())
            throw new InternalInterpreterError('todo bl: unimplemented!');
        var ans = newEmptyList();
        var lastChild = newId(this.firstChild.payload);
        ans.appendChild(lastChild);

        var idNode;

        for (var cur = this.firstChild.nextSibling; cur; cur = cur.nextSibling) {
            idNode = newId(cur.isList() ? cpsNames.shift(): cur.payload);
            lastChild.appendSibling(idNode);
            lastChild = idNode;
        }

        ans.appendChild(newContinuationLambda(rootName));
        return ans;
    } else throw new InternalInterpreterError('unsupported type ' + this.type);
};

Datum.prototype.cpsify = function(rootName, appendTo) {

    var end = appendTo;
    var cpsNames = [];

    if (this.isList() && this.firstChild) {
        for (var cur = this.firstChild.nextSibling; cur; cur = cur.nextSibling) {
            if (cur.isList()) {
                var name = newCpsName();
                end = cur.cpsify(name, end);
                cpsNames.push(name);
            }
        }
        var local = this.cpsifyLocal(rootName, cpsNames);
        end.appendChild(local);
        return local.firstChild.lastSibling();
    } else throw new InternalInterpreterError('not a list');
};