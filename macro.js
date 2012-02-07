function SchemeMacro(literalIdentifiers, rules, definitionEnv) {
    this.literalIdentifiers = {};
    for (var curId = literalIdentifiers; curId; curId = curId.nextSibling)
        this.literalIdentifiers[curId.payload] = true;
    this.rules = rules.clone();
    this.definitionEnv = definitionEnv;

    // Cache the set of free identifiers per pattern/template pair
    this.freeIds = [];
    for (var cur = rules; cur; cur = cur.nextSibling)
        this.freeIds.push(null);
}

SchemeMacro.prototype.setIsLetOrLetrecSyntax = function() {
    this.isLetOrLetrecSyntax = true;
    return this;
};

/* Should only be used during interpreter bootstrapping. */
SchemeMacro.prototype.clone = function(newDefinitionEnv) {
    return new SchemeMacro(this.literalIdentifiers, this.rules, newDefinitionEnv);
};
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

function TemplateBindings() {
    this.regularBindings = {}; // strings to objects
    this.ellipsisBindings = {}; // strings to arrays of objects
    this.ellipsisIndices = {}; // strings to indices into ellipsisBindings
    this.awaitingFixing = [];
    this.boundIds = {};
}

TemplateBindings.prototype.toString = function() {
  var ans = '[';
    for (var name in this.regularBindings)
        ans += this.regularBindings[name] + ' ';
    return ans + ']';
};

TemplateBindings.prototype.addTemplateBinding = function(name, val) {

    if (val instanceof Datum && val.isIdentifier())
        this.boundIds[val.payload] = true;

    var regularBinding = this.regularBindings[name];
    var ellipsisBinding;

    /* If this name already has a regular binding and we're requesting another
        binding, we must be in an ellipsis pattern, so move the binding from
        the regularBindings to the ellipsisBindings. */
    if (regularBinding) {
        this.regularBindings[name] = null;
        this.ellipsisBindings[name] = [regularBinding, val];
        this.ellipsisIndices[name] = 0;
    }

    // If this name already has an ellipsis binding, just add the new value to it
    else if (ellipsisBinding = this.ellipsisBindings[name]) {
        ellipsisBinding.push(val);
    }

    // If this name has neither type of binding, add a regular binding.
    /* todo bl add this.regularBindings[name] = false to denote moves
        to ellipsisBindings so we don't have to do two lookups in the common case */
    else {
        this.regularBindings[name] = val;
        this.awaitingFixing.push(name);
    }
};

TemplateBindings.prototype.resetAwaitingFixing = function() {
  this.awaitingFixing = [];
};

TemplateBindings.prototype.fixNewBindings = function() {
    var len = this.awaitingFixing.length;
    for (var i=0; i<len; ++i) {
        var name = this.awaitingFixing[i];
        var soleVal = this.regularBindings[name];
        if (soleVal) {
            this.regularBindings[name] = null;
            this.ellipsisBindings[name] = [soleVal];
            this.ellipsisIndices[name] = 0;
        }
    }
    this.awaitingFixing = [];
};

TemplateBindings.prototype.failures = {
  noMoreEllipsisBindings: 0
};

TemplateBindings.prototype.getTemplateBinding = function(name, backdoorEnv) {
    var ans;
    var maybeRegularBinding = this.regularBindings[name];
    if (maybeRegularBinding) {
        ans = maybeRegularBinding.clone();
    } else {
        var maybeEllipsisBindings = this.ellipsisBindings[name];
        if (maybeEllipsisBindings) {
            /* If we're in ellipsis mode and have no more bindings, return
             the special value failures.noMoreEllipsisBindings.
             We have to reset the index into the array because a later part
             of the template could ask for it again. For example:

             (define-syntax foo (syntax-rules () ((foo x ...) (cons (list x ...) (list x ...)))))

             (foo 1 2 3) => ((1 2 3) 1 2 3) */
            if (!maybeEllipsisBindings.length || this.ellipsisIndices[name] === maybeEllipsisBindings.length) {
                this.ellipsisIndices[name] = 0;
                ans = this.failures.noMoreEllipsisBindings;
            } else {
                ans = maybeEllipsisBindings[this.ellipsisIndices[name]++].clone();
            }
        }
    }

    /* Workaround for let-syntax and letrec-syntax.
     This implementation rewrites let-syntax and letrec-syntax
     as let and letrec respectively. For example,

     (let-syntax ((foo (syntax-rules () ((foo) 'hi)))) ...)

     desugars as

     (let ((foo [SchemeMacro object wrapped in a Datum])) ...)

     When this macro use is matched against the definition of let,
     the wrapped SchemeMacro object will be added to the TemplateBindings
     correctly. The problem arises during transcription: we cannot insert
     the wrapped SchemeMacro object directly into the new parse tree,
     because that parse tree will be handed to the parser, which won't know
     what to do with SchemeMacros.

     Indirection comes to the rescue. We insert a new identifier node and
     bind it in the current environment to the SchemeMacro. Later, on
     the trampoline, we will look up the value of that identifier and
     find the SchemeMacro as desired.

     We have to set the isLetOrLetrecSyntax flag on the macro to disallow
     this behavior from the programmer. We cannot allow her to write

     (let ((x let*)) x) */
    if (ans instanceof Datum && ans.isMacro()) {
        var fakeName = newCpsName();
        backdoorEnv.addBinding(fakeName, ans.payload.setIsLetOrLetrecSyntax());
        ans = newIdOrLiteral(fakeName);
    }
    return ans;
};

TemplateBindings.prototype.setEmptyEllipsisMatch = function(name) {
  this.ellipsisBindings[name] = [];
};

SchemeMacro.prototype.selectTemplate = function(datum, useEnv) {
    for (var rule = this.rules, i = 0; rule; rule = rule.nextSibling, ++i) {
        /* During pattern matching, input datums are bound to pattern datums.
            If pattern matching is successful, then during transcription we need
            to remember those bindings. All we need is an object to store the
            bindings between pattern matching and transcription. A JavaScript
            dictionary is perhaps sufficient, but I'm going to use a purpose-built
            object in case we need richer semantics (e.g. for ellipses). */
        var bindings = new TemplateBindings();
        var pattern = rule.at('pattern');
        if (this.patternMatch(pattern, datum, useEnv, bindings, true)) {

            var template = rule.at('template');

            if (!this.freeIds[i])
                this.freeIds[i] = constructFreeIds(pattern, template);

            return new Template(template, bindings, this.freeIds[i]);
        }
    }
    return null;
};

/* R5RS 4.3.2: "Identifiers that appear in the template but are not
 pattern variables or the identifier ... are inserted into the output
 as literal identifiers. If a literal identifier is inserted as a free
 identifier then it refers to the binding of that identifier within whose
 scope the instance of syntax-rules appears. If a literal identifier
 is inserted as a bound identifier then it is in effect renamed to prevent
 inadvertent captures of free identifiers."

 This function implements the free identifier stuff, but I don't understand
 the bound identifier stuff. todo!
 */
function constructFreeIds(patternDatum, templateDatum) {
    var patternIds = {};
    var templateIds = {};
    patternDatum.forEach(function(node) {
        if (node.isIdentifier() && !isParserSensitiveId(node.payload))
            patternIds[node.payload] = true;
    });
    templateDatum.forEach(function(node) {
        if (node.isIdentifier() && !isParserSensitiveId(node.payload) && node.payload !== '...')
            templateIds[node.payload] = true;
    });

    var freeInTemplate = {};

    for (var name in templateIds)
        if (!patternIds[name])
            freeInTemplate[name] = true;

    return freeInTemplate;
}

function Template(datum, templateBindings, freeIdsInTemplate) {
    /* We must clone the template datum because every macro use will
     deform the template through the hygienic transcription. */
    this.datum = datum.clone();
    this.templateBindings = templateBindings;
    this.freeIdsInTemplate = freeIdsInTemplate;
}

Template.prototype.hygienicTranscription = function(env) {
    return this.datum.transcribe(this.templateBindings, env);
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
        // Push the new datum onto the list of bindings
        bindings.addTemplateBinding(patternId, inputDatum.clone(true));
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

        if (inputDatum.payload === patternId
            && (!this.definitionEnv.hasBindingRecursive(patternId)
            && !useEnv.hasBindingRecursive(patternId))) { // both have no lexical binding
            bindings.addTemplateBinding(patternId, inputDatum);
            return true;

        }

        else if (this.definitionEnv.get(patternId) === useEnv.get(patternId)) { // both have same lexical binding
            /* If we are here, bindings[patternId] will most likely be undefined.
                The spec does not forbid repeated literals as in
                (define-syntax foo (syntax-rules (x x) ((foo x) "hi!")))
                but such repetition is useless and it is fine to overwrite them. */

            // todo bl: likely bugs with literal ids in ellipsis patterns! because i see no pushes
            bindings.addTemplateBinding(patternId, inputDatum);
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
                && patternElement.nextSibling.payload === '...') {
                stickyEllipsisPattern = patternElement;
                bindings.resetAwaitingFixing();
            }

            if (!this.patternMatch(patternElement, inputElement, useEnv, bindings))
                return false;

            // todo bl: kludgey and i'm not convinced it works recursively! start here
            if (stickyEllipsisPattern)
                bindings.fixNewBindings();
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
                    bindings.setEmptyEllipsisMatch(node.payload);
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
         we have to explicitly manufacture a list from the pointer.

         todo bl: the construction of manufacturedList is basically a copy
         and paste from the implementation of cdr, and is subtle. Consider
         making cdr available here. */
        var manufacturedList = (inputElement.nextSibling || inputDatum.isList())
            ? inputElement.siblingsToList(inputDatum.isImproperList())
            : inputElement;

        return this.patternMatch(
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
