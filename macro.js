function SchemeMacro(literalIdentifiers, rules, definitionEnv) {
    this.literalIdentifiers = {};
    for (var curId = literalIdentifiers; curId; curId = curId.nextSibling)
        this.literalIdentifiers[curId.payload] = true;
    this.rules = rules;
    this.definitionEnv = definitionEnv;
}

SchemeMacro.prototype.allPatternsBeginWith = function(keyword) {

    /* todo bl incorrect -- needs to be recursive
     for (var rule = this.rules; rule; rule = rule.nextSibling) {
     var pattern = rule.at('pattern');
     if (!pattern.isList() || pattern.firstChild.payload !== keyword)
     return false;
     }*/
    return true;

};

SchemeMacro.prototype.ellipsesMatch = function() {

    for (var rule = this.rules; rule; rule = rule.nextSibling)
        if (!ellipsesMatch(rule.at('pattern'), rule.at('template')))
            return false;
    return true;
};

/* 4.3.2
 A subpattern followed by ... can match zero or more elements of the input.
 It is an error for ... to appear in <literal>. Within a pattern the identifier ...
 must follow the last element of a nonempty sequence of subpatterns.

 Pattern variables that occur in subpatterns followed by one or more instances
 of the identifier ... are allowed only in subtemplates that are followed
 by as many instances of .... They are replaced in the output by all of the
 subforms they match in the input, distributed as indicated. It is an error
 if the output cannot be built up as specified.
 */
function ellipsesMatch(patternDatum, templateDatum) {
    // todo bl
    return true;
}

SchemeMacro.prototype.selectTemplate = function(datum, useEnv) {
    for (var rule = this.rules; rule; rule = rule.nextSibling) {
        var bindings = {};
        var pattern = rule.at('pattern');
        if (this.patternMatch(pattern, datum, useEnv, bindings, true))
            return new Template(rule.at('template'), bindings);
    }
    return null;
};

function Template(datum, bindings) {
    /* We must clone the template datum because every macro use will
     deform the template through the hygienic transcription. */
    this.datum = datum.clone().sanitize();
    this.bindings = bindings;
}

Template.prototype.hygienicTranscription = function() {
    return this.datum.transcribe(this.bindings);
    // a good debugging line console.log('hygienicTranscription: ' + before + ' => ' + ans);
};

/* 4.3.2: An input form F matches a pattern P if and only if:
 (1) P is a non-literal identifier; or
 (2) P is a literal identifier and F is an identifier with the same binding; or
 (3) P is a list (P1 ... Pn) and F is a list of n forms that match P1 through Pn, respectively; or
 (4) P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list of n or more forms
 that match P1 through Pn, respectively, and whose nth “cdr” matches Pn+1; or
 (5) P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ...
 and F is a proper list of at least n forms, the first n of which match P1 through Pn,
 respectively, and each remaining element of F matches Pn+1; or
 (6) P is a vector of the form #(P1 ...Pn) and F is a vector of n forms that match P1 through Pn; or
 (7) P is of the form #(P1 . . . Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ...
 and F is a vector of n or more forms the first n of which match P1 through Pn,
 respectively, and each remaining element of F matches Pn+1; or
 (8) P is a datum and F is equal to P in the sense of the equal? procedure.
 */
SchemeMacro.prototype.patternMatch = function(patternDatum, inputDatum, useEnv, bindings, ignoreLeadingKeyword) {
    var args = [patternDatum, inputDatum, useEnv, bindings, ignoreLeadingKeyword];
    return this.matchLiteralId.apply(this, args) // case 2
        || this.matchNonLiteralId.apply(this, args) // case 1
        || this.matchListOrVector.apply(this, args) // cases 3, 5, 6, 7
        || this.matchImproperList.apply(this, args) // case 4
        || this.matchDatum.apply(this, args); // case 8
};


/* (1) P is a non-literal identifier
 Example: (let-syntax ((foo (syntax-rules () ((foo x) "aha!")))) (foo (1 2 3))) => "aha!"
 ((1 2 3) would be a procedure-call error if evaluated, but it is never evaluated.) */
SchemeMacro.prototype.matchNonLiteralId
    = function(patternDatum, inputDatum, useEnv, bindings, ignoreLeadingKeyword) {

    var patternId = patternDatum.isIdentifier() && patternDatum.payload;
    var patternIsLiteralId = patternId && this.literalIdentifiers[patternId];

    if (patternId && !patternIsLiteralId) {

        /* Temporarily slice off the input's siblings
         to save time during cloning. */
        var savedNextSibling = inputDatum.nextSibling;
        inputDatum.nextSibling = null;
        var toInsert = inputDatum.clone().sanitize();
        inputDatum.nextSibling = savedNextSibling;

        var alreadyBound = bindings[patternId];

        // Push the new datum onto the list of bindings
        if (!alreadyBound)
            bindings[patternId] = [toInsert];
        else
            alreadyBound.push(toInsert);

        return true;
    } else return false;
};


/* (2) P is a literal identifier and F is an identifier with the same binding
 4.3.2: A subform in the input matches a literal identifier if and only if
 it is an identifier and either both its occurrence in the macro expression
 and its occurrence in the macro definition have the same lexical binding,
 or the two identifiers are equal and both have no lexical binding.

 Examples:

 ((lambda (x)
 (let-syntax
 ((foo (syntax-rules (x)
 ((foo x) "matched literal")
 ((foo y) "did not match literal"))))
 (foo x)))
 "hello")

 That should evaluate to "matched literal" because x has the same lexical
 binding in the macro expression and the macro definition.

 ((lambda (x)
 (let-syntax
 ((foo (syntax-rules (x)
 ((foo x) "matched literal")
 ((foo y) "did not match literal"))))
 ((lambda (x) (foo x)) "hello")))
 "world")

 That should evaluate to "did not match literal" because x is bound to
 "hello" in the macro expression but bound to "world" in the macro definition.

 (let-syntax
 ((foo (syntax-rules (x)
 ((foo x) "matched literal")
 ((foo y) "did not match literal"))))
 (foo x))

 That should evaluate to "matched literal" because x has no binding in
 either the macro expression or the macro definition.

 (let-syntax
 ((foo (syntax-rules (x)
 ((foo x) "matched literal")
 ((foo y) "did not match literal"))))
 ((lambda (x) (foo x)) "hello"))

 That should evaluate to "did not match literal" because x is bound in the
 macro expression but not in the macro definition.

 Whew! */
SchemeMacro.prototype.matchLiteralId
    = function(patternDatum, inputDatum, useEnv, bindings, ignoreLeadingKeyword) {

    var patternId = patternDatum.isIdentifier() && patternDatum.payload;
    var patternIsLiteralId = patternId && this.literalIdentifiers[patternId];
    var inputIsId = inputDatum.isIdentifier();

    if (patternIsLiteralId && inputIsId) {
        if ((inputDatum.payload === patternId
            && !this.definitionEnv.hasBinding(patternId)
            && !useEnv.hasBinding(patternId)) // both have no lexical binding
            || (this.definitionEnv.get(patternId) === useEnv.get(patternId))) { // both have same lexical binding
            /* If we are here, bindings[patternId] will most likely be undefined.
                The spec does not forbid repeated literals as in
                (define-syntax foo (syntax-rules (x x) ((foo x) "hi!")))
                but such repetition is useless and it is fine to overwrite them. */
            bindings[patternId] = [inputDatum];
            return true;
        }
    }

    return false;
};

/*
 (3) P is a list (P1 ... Pn) and F is a list of n forms that match
 P1 through Pn, respectively

 Example:
 (let-syntax
 ((foo (syntax-rules ()
 ((foo a (b c) d) (+ b d)))))
 (foo "NaN" (100 ()) -1)) => 99

 (5) P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the
 identifier ... and F is a proper list of at least n forms, the first n
 of which match P1 through Pn, respectively, and each remaining element
 of F matches Pn+1

 Example:
 (define-syntax foo
 (syntax-rules ()
 ((foo) 1)
 ((foo x) 2)
 ((foo x xs ...) (+ 1 (foo xs ...)))))

 (foo) => 0
 (foo foo) => 1
 (foo foo foo) => 2
 etc.

 (6) P is a vector of the form #(P1 ...Pn) and F is a vector of n forms that match P1 through Pn
 (7) P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ...
 and F is a vector of n or more forms the first n of which match P1 through Pn,
 respectively, and each remaining element of F matches Pn+1
 */
SchemeMacro.prototype.matchListOrVector
    = function(patternDatum, inputDatum, useEnv, bindings, ignoreLeadingKeyword) {
    if ((patternDatum.isList() && inputDatum.isList())
        || (patternDatum.isVector() && inputDatum.isVector())) {
        var patternElement = patternDatum.firstChild;
        var inputElement = inputDatum.firstChild;

        /* 4.3.2: "The keyword at the beginning of the pattern in a
         <syntax rule> is not involved in the matching and is not considered
         a pattern variable or literal identifier." ignoreLeadingKeyword is a
         convenience parameter to support this. (If we are here, the first
         element in the pattern should already have been verified to be
         the keyword (see allPatternsBeginWith()), and the first element in the
         input will already have been matched to the keyword by the parser.) */
        if (ignoreLeadingKeyword) {
            patternElement = patternElement.nextSibling;
            inputElement = inputElement.nextSibling;
        }

        /* 4.3.2: "A subpattern followed by ... can match zero or more
         elements of the input." Here's how we implement this: when an
         ellipsis is detected in a pattern, the patternDatum pointer gets stuck
         at the element before the ellipsis, while the inputDatum pointer
         advances as normal. This will ensure successive input elements are
         matched to the same pattern element. */
        var stickyEllipsisPattern;

        for (/* already initialized */;
            patternElement && inputElement;
            patternElement = stickyEllipsisPattern || patternElement.nextSibling,
                inputElement = inputElement.nextSibling) {

            /* Turn on "ellipsis matching mode" if it is currently off
             and the next pattern element is an ellipsis. */
            if (!stickyEllipsisPattern
                && patternElement.nextSibling
                && patternElement.nextSibling.payload === '...')
                stickyEllipsisPattern = patternElement;

            if (!this.patternMatch(patternElement, inputElement, useEnv, bindings))
                return false;
        }

        /* In cases like matching input (foo) against pattern (foo x ...),
         the above loop will not execute and "ellipsis matching mode" will
         incorrectly never be turned on. Check for that corner case here.
         In such a case, we have to remember that all the identifiers
         in the datum preceding the ellipsis should have no bindings in the
         corresponding template during transcription. We signal this
         by setting the bindings to empty arrays. */
        var ellipsisMatchedNothing = !stickyEllipsisPattern
            && patternElement
            && patternElement.nextSibling
            && patternElement.nextSibling.payload === '...';
        if (ellipsisMatchedNothing) {
            patternElement.forEach(function(node) {
                if (node.isIdentifier())
                    bindings[node.payload] = [];
            });
        }

        /* If there are no ellipses in the pattern, the pattern and the input
         must both be successfully exhausted for the match to succeed.*/
        var inputAndPatternDone = !(inputElement || patternElement);

        return stickyEllipsisPattern
            || ellipsisMatchedNothing
            || inputAndPatternDone;
    } else return false;
};

/* (4) P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list
 of n or more forms that match P1 through Pn, respectively,
 and whose nth “cdr” matches Pn+1.
 Example: (let-syntax ((foo (syntax-rules () ((foo x . y) y)))) (foo 1 . 2) => 2
 (foo 1 + 2 3) => 5 (because y matches (+ 2 3))
 */

SchemeMacro.prototype.matchImproperList
    = function(patternDatum, inputDatum, useEnv, bindings, ignoreLeadingKeyword) {
    if (patternDatum.isImproperList()
        && (inputDatum.isImproperList() || inputDatum.isList())) {

        var patternElement = patternDatum.firstChild;
        var inputElement = inputDatum.firstChild;
        if (ignoreLeadingKeyword) {
            patternElement = patternElement.nextSibling;
            inputElement = inputElement.nextSibling;
        }

        for (/* already initialized */;
            patternElement && patternElement.nextSibling;
            patternElement = patternElement.nextSibling,
                inputElement = inputElement.nextSibling) {

            if (!inputElement // The input list is shorter than the pattern list. Failure.
                || !this.patternMatch(patternElement, inputElement, useEnv, bindings))
                return false;
        }

        /* Now we have to compare the part of the pattern after the dot with
         the remainder of the input. Note that since our lists aren't recursive,
         we have to explicitly manufacture a list from the pointer. */
        var manufacturedList = inputElement.siblingsToList(inputDatum.isImproperList());
        return inputElement
            && this.patternMatch(
            patternElement,
            manufacturedList,
            useEnv,
            bindings);
    } else return false;
};

// (8) P is a datum and F is equal to P in the sense of the equal? procedure.
// todo bl too permissive? disabled for now
SchemeMacro.prototype.matchDatum
    = function(patternDatum, inputDatum, useEnv, ansDict, ignoreLeadingKeyword) {
    return patternDatum.isEqual(inputDatum);
};
