function SchemeMacro(literalIdentifiers, rules, definitionEnv) {

    this.definitionEnv = definitionEnv;
    this.literalIdentifiers = {};
    for (var curId = literalIdentifiers; curId; curId = curId.nextSibling)
        this.literalIdentifiers[curId.payload] = true;

    this.transformers = [];

    for (var rule = rules; rule; rule = rule.nextSibling) {
        var pattern = rule.at('pattern').desugar(/* no env needed */);
        var template = rule.at('template').desugar(/* no env needed */);
        var transformer = new Transformer(pattern, template);
        this.transformers.push(transformer);
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
SchemeMacro.prototype.setIsLetOrLetrecSyntax = function() {
    this.isLetOrLetrecSyntax = true;
    return this;
};

/* Should only be used during interpreter bootstrapping. */
SchemeMacro.prototype.clone = function(newDefinitionEnv) {
    var ans = new SchemeMacro(this.literalIdentifiers, null, newDefinitionEnv);
    ans.transformers = this.transformers;
    return ans;
};

/* My approach for supporting nested ellipses in macro transcriptions
is to take a single pass through the input and build up a TemplateBindings
object whose tree structure mirrors the ellipsis nesting in the pattern.
For example:

 (define-syntax foo
    (syntax-rules ()
        ((foo ((x y) ...) ...)
        (quote (((x ...) (y ...)) ...)))))

 with input

 (foo ((a b)) ((c d) (e f)))

 produces this TemplateBindings object:

 child 0:
    child 0:
        x = a
        y = b
 child 1:
    child 0:
        x = c
        y = d
    child 1:
        x = e
        y = f

 Then transcription involves a single pass through the template with
 this TemplateBindings object, using the ellipses in the template
 to descend through the TemplateBindings tree. Here's the flow of control
 during transcription:

 1. Transcribe ((x ...) (y ...)) with child0
    2. Transcribe x with child0.child0 => a
    3. Transcribe x with [no more children] => false. Reset cur child.
    4. Transcribe y with child0.child0 => b
    5. Transcribe y with [no more children] => false. Reset cur child.
 [1 completes as ((a) (b))]
 6. Transcribe ((x ...) (y ...)) with child1
    7. Transcribe x with child1.child0 => c
    8. Transcribe x with child1.child1 => e
    9. Transcribe x with [no more children] => false. Reset cur child.
    10. Transcribe y with child1.child0 => d
    11. Transcribe y with chid1.child1 => f
    12. Transcribe y with [no more children] => false. Reset cur child.
 [6 completes as ((c e) (d f))]
 13. Transcribe ((x ...) (y ...)) with [no more children] => false. Reset cur child.
 [13 completes as (((a) (b)) ((c e) (d f)))]

 TODO bl: explain -- or ideally remove -- all the crazy logic dealing
 with "incorporation". Do we even need it?

 */
function TemplateBindings(letSyntaxEnv, patternIds) {
    this.bindings = {};
    this.children = [];
    this.curChild = 0;
    this.letSyntaxEnv = letSyntaxEnv;
    this.patternIds = patternIds;
}

TemplateBindings.prototype.resetCurChild = function() {
    this.curChild = 0;
    return this;
};

TemplateBindings.prototype.addTemplateBinding = function(name, val) {
    if (this.bindings[name])
        throw new InternalInterpreterError('invariant incorrect');
    else if (val.isMacro()) {
        // See comments at SchemeMacro.prototype.setIsLetOrLetrecSyntax
        var fakeName = newCpsName();
        this.letSyntaxEnv.addBinding(fakeName, val.getMacro());
        this.bindings[name] = newIdOrLiteral(fakeName);
    }
    else {
        this.bindings[name] = val;
    }
};

// Purely for debugging.
TemplateBindings.prototype.toString = function(tabs) {
    tabs = tabs || '';
    var ans = '';
    for (var name in this.bindings)
        ans += tabs + name + ' = ' + this.bindings[name].toString() + '\n';
    for (var i=0; i<this.children.length; ++i)
        ans += tabs + 'child ' + i + ':\n' + this.children[i].toString(tabs+'\t');
    return ans;
};

TemplateBindings.prototype.addChildBindings = function(child) {
    this.children.push(child);
    return this;
};

TemplateBindings.prototype.hasNoneOf = function(other) {
    for (var name in other.bindings)
        if (this.bindings[name])
            return false;
    return true;
};

/* Try to incorporate the child's bindings in an existing child if there's room,
 otherwise just tack the child on to the parent. */
TemplateBindings.prototype.addOrIncorporateChild = function(child) {
    return this.incorporateChild(child) || this.addChildBindings(child);
};

TemplateBindings.prototype.incorporateChild = function(child) {

    // We only incorporate flat TemplateBindings objects.
    if (child.children.length > 0)
        return null;

    /* Dump all the child's bindings in the first child that doesn't
     have any of the bindings.

     todo bl: i have no idea why this heuristic seems to work. */
    for (var i = 0; i < this.children.length; ++i) {
        var candidate = this.children[i];
        if (candidate.hasNoneOf(child)) {
            for (var name in child.bindings)
                candidate.addTemplateBinding(name, child.bindings[name]);
            return this;
        }
    }

    return null;
};

TemplateBindings.prototype.getNextChild = function() {
    if (this.curChild < this.children.length) {
        return this.children[this.curChild++];
    } else {
        this.curChild = 0;   // reset for next time
        return null;
    }
};

TemplateBindings.prototype.resolveDatum = function(datum) {
    if (!this.patternIds)
        throw new InternalInterpreterError('invariant incorrect');

    if (datum.isIdentifier()) {
        var name = datum.payload;

        var maybe = this.bindings[name];
        if (maybe) {
            return maybe.clone(true);
        } else if (this.patternIds[name] !== undefined) {
            /* It's important to return false here, instead of some other
             "falsey" value like null. This value is immediately returned by
             IdOrLiteralTransformer.prototype.matchInput. Meanwhile,
             EllipsisTransformer.prototype.matchInput returns
             new SiblingBuffer().toList() when it has successfully matched the
             ellipsis zero times, which is not a failure. And if you look at
             the implementation, you will see there is a good reason that

             new SiblingBuffer().toList() === null.

             So we have to return something different.

             Static types would be useful here. */
            return false;
        } else {
            return datum.clone(true);
        }

    } else {
        return datum.clone(true);
    }
};

TemplateBindings.prototype.getPatternIds = function() {
    return this.patternIds;
};

function Transformer(pattern, template) {
    /* This is an InternalInterpreterError (= sanity check) instead of a
     MacroError because the grammar should make it impossible for
     a programmer to get here. */
    if (!(pattern instanceof ListLikeTransformer))
        throw new InternalInterpreterError(
            'transformer begins with a pattern '
                + pattern.toString()
                + ' that is not a ListLikeTransformer');
    this.pattern = pattern;
    this.template = template;
    this.name = pattern.subtransformers[0].datum.payload;
    this.setupIds();
}

Transformer.prototype.matchInput = function(inputDatum, literalIdentifiers, definitionEnv, useEnv, bindings) {
    return this.pattern.matchInput(inputDatum, literalIdentifiers, definitionEnv, useEnv, bindings);
};

Transformer.prototype.getName = function() {
    return this.name;
};

Transformer.prototype.setupIds = function() {
    var patternIds = {}; // names to nesting levels
    var maybeRename = {};
    var pattern = this.pattern;
    var template = this.template;
    var macroName = this.name;

    var patternFn = function(subtrans, ellipsisLevel) {
        if (subtrans instanceof IdOrLiteralTransformer) {
            if (subtrans.datum.isIdentifier() && subtrans.datum.payload !== macroName) {
                patternIds[subtrans.datum.payload] = ellipsisLevel;
            }
        } else subtrans.forEachSubtransformer(
            patternFn,
            subtrans instanceof EllipsisTransformer ? ellipsisLevel + 1 : ellipsisLevel);
    };

    var templateFn = function(subtrans, ellipsisLevel) {
        if (subtrans instanceof IdOrLiteralTransformer) {
            if (subtrans.datum.isIdentifier()) {
                var name = subtrans.datum.payload;
                var maybeInPattern = patternIds[name];
                if (maybeInPattern === undefined) {
                    maybeRename[name] = true;
                } else if (maybeInPattern !== ellipsisLevel
                    && name !== macroName) {
                    throw new MacroError(
                        macroName,
                        name
                            + ' is at ellipsis level '
                            + maybeInPattern
                            + ' in pattern '
                            + pattern.toString()
                            + ' but at ellipsis level '
                            + ellipsisLevel
                            + ' in template '
                            + template.toString());
                }
            }
        } else subtrans.forEachSubtransformer(
            templateFn,
            subtrans instanceof EllipsisTransformer ? ellipsisLevel + 1 : ellipsisLevel);
    };

    pattern.forEachSubtransformer(patternFn, 0);
    template.forEachSubtransformer(templateFn, 0);

    this.maybeRename = maybeRename;
    this.patternIds = patternIds;

    return this;
};

Transformer.prototype.getPatternIds = function() {
    return this.patternIds;
};

/* ListLikeTransformer, EllipsisTransformer, and IdOrLiteralTransformer
all "implement" the following "interface":

{
    forEachSubtransformer: function(callback, args) { ... },
    matchInput: function(inputDatum, literalIds, definitionEnv, useEnv, bindings) { ... },
    toDatum: function(bindings) { ... }
}

In fact, they all used to have a common prototypal ancestor. But since
they didn't share any implementations of these functions, nor any common state,
all the ancestor did was throw "pure virtual" exceptions if a function
hadn't been overridden. In view of this lack of usefulness, I got rid of the
"superclass". */
function ListLikeTransformer(type) {
    this.type = type;
    this.subtransformers = [];
}

ListLikeTransformer.prototype.addSubtransformer = function(subtransformer) {
    this.subtransformers.push(subtransformer);
    return this;
};

ListLikeTransformer.prototype.forEachSubtransformer = function(callback, args) {
  for (var i=0; i<this.subtransformers.length; ++i)
    callback(this.subtransformers[i], args);
};

ListLikeTransformer.prototype.matchInput = function(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
    var len = this.subtransformers.length;
    var maybeEllipsis = this.subtransformers[len-1] instanceof EllipsisTransformer
        && this.subtransformers[len-1];

    if (!(inputDatum.isList() || inputDatum.isImproperList())) // todo bl vectors
        return false;

    /* R5RS 4.3.2: "an input form F matches a pattern P if and only if [...]
     - P is a list (P1 ... Pn) and F is a list of n forms match P1 through Pn, respectively; or
     - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or
     improper list of n or more forms that match P1 through Pn, respectively,
     and whose nth "cdr" matches Pn+1; or
     - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
     the identifier ... and F is a proper list of at least n forms,
     the first n of which match P1 through Pn, respectively,
     and each remaining element of F matches Pn+1; or
     - P is a vector of the form #(P1 ...Pn) and F is a vector of n forms
     that match P1 through Pn; or (TODO BL)
     - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is
     the identifier ... and F is a vector of n or more forms the first n
     of which match P1 through Pn, respectively, and each remaining element
     of F matches Pn+1" (TODO BL) */
    for (var subinput = inputDatum.firstChild, i=0;
         subinput;
         subinput = subinput.nextSibling, ++i) {

        // If there's an ellipsis in the pattern, break out to deal with it.
        if (i === len - 1 && (maybeEllipsis || this.type === '.('))
            break;

        /* If there's no ellipsis in the pattern and the input is longer
         than the pattern, this is a failure. */
        else if (i >= len)
            return false;

        /* If pattern matching on the subinput and subpattern fails, this is
         a failure. */
        else if (!this.subtransformers[i].matchInput(subinput, literalIds, definitionEnv, useEnv, bindings))
            return false;
    }

    if (maybeEllipsis) {
        return maybeEllipsis.matchInput(subinput, literalIds, definitionEnv, useEnv, bindings);
    }

    // Dotted-list patterns cannot end in ellipses.
    else if (this.type === '.(') {
        var toMatchAgainst;

        if (inputDatum.isList()) {
            toMatchAgainst = subinput.siblingsToList();
        } else if (inputDatum.isImproperList()) {
            if (subinput.nextSibling)
                toMatchAgainst = subinput.siblingsToList(true);
            else
                toMatchAgainst = subinput;
        }

        return this.subtransformers[i].matchInput(toMatchAgainst, literalIds, definitionEnv, useEnv, bindings);
    }

    /* If we matched all of the input without getting through all of
     the pattern, this is a failure. */
    else {
        return i === len;
    }
};

ListLikeTransformer.prototype.toDatum = function (bindings) {

    var buf = new SiblingBuffer();
    var len = this.subtransformers.length;
    var success;

    for (var i = 0; i < len; ++i) {
        success = this.subtransformers[i].toDatum(bindings);
        if (success === false)
            return false;
        else
            buf.appendSibling(success);
    }

    return buf.toList();
};

ListLikeTransformer.prototype.toString = function () {
    var ans = this.type === '#(' ? this.type : '(';
    if (this.subtransformers.length === 0) {
        return ans + ')';
    } else {
        for (var i = 0; i < this.subtransformers.length - 1; ++i)
            ans += this.subtransformers[i].toString() + ' ';
        if (this.type === '.(')
            ans += '. ';
        return ans + this.subtransformers[i].toString() + ')';
    }
};

// See comments at top of ListLikeTransformer.
function EllipsisTransformer(subtransformer) {
    this.subtransformer = subtransformer;
    this.bindings = [];
}

EllipsisTransformer.prototype.toString = function() {
    return this.subtransformer.toString() + ' ...';
};

EllipsisTransformer.prototype.matchInput = function(inputDatum, literalIds, definitionEnv, useEnv, bindings) {

    /* We have to leave some evidence in the TemplateBindings object of
        an empty match. Example:

     (define-syntax foo
     (syntax-rules ()
     ((foo (x ...) ...)
     (+ (* x ...) ...))))

     on input

     (foo () () ())

     should create a TemplateBindings object like

     child 0:
        child 0:
     child 1:
        child 0:
     child 2:
        child 0:

     so that we get the correct transcription

     (+ (*) (*) (*)) => 3.
     */
    if (!inputDatum)
        bindings.addChildBindings(new TemplateBindings(useEnv, bindings.getPatternIds()));

    for (var subinput = inputDatum; subinput; subinput = subinput.nextSibling) {
        var childBindings = new TemplateBindings(useEnv, bindings.getPatternIds());
        var maybeMatched = this.subtransformer.matchInput(subinput, literalIds, definitionEnv, useEnv, childBindings);
        if (maybeMatched)
            bindings.addOrIncorporateChild(childBindings)
        else return false;
    }
    return true;
};

EllipsisTransformer.prototype.toDatum = function(bindings) {
    var buf = new SiblingBuffer();
    var bindingsToUse;
    var success;
    while ((bindingsToUse = bindings.getNextChild())
        && (success = this.subtransformer.toDatum(bindingsToUse)))
        buf.appendSibling(success);
    bindings.resetCurChild();
    return buf.toSiblings();
};

EllipsisTransformer.prototype.forEachSubtransformer = function(callback, args) {
    callback(this.subtransformer, args);
};

// See comments at top of ListLikeTransformer.
function IdOrLiteralTransformer(datum) {
    this.datum = datum.clone(true);
}

IdOrLiteralTransformer.prototype.matchInput = function(inputDatum, literalIds, definitionEnv, useEnv, bindings) {
    if (this.datum.isIdentifier()) {
        /* R5RS 4.3.2: "A subform in the input matches a literal identifier
         if and only if it is an identifier and either both its occurrence
         in the macro expression and its occurrence in the macro definition
         have the same lexical binding, or the two identifiers are equal
         and both have no lexical binding." */
        if (literalIds[this.datum.payload]) {
            if (inputDatum.isIdentifier()) {
                var name = inputDatum.payload;
                // Both have no lexical binding
                if (name === this.datum.payload
                    && (!definitionEnv.hasBindingRecursive(name)
                    && !useEnv.hasBindingRecursive(name))) {
                    bindings.addTemplateBinding(name, inputDatum);
                    return true;
                } else if (definitionEnv.get(name) === useEnv.get(name)) {
                    bindings.addTemplateBinding(name, inputDatum);
                    return true;
                } else return false;
            } else return false;
        }
        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a non-literal identifier [...]".
         That is, non-literal identifiers match anything. */
        else {
            bindings.addTemplateBinding(this.datum.payload, inputDatum);
            return true;
        }
    } else {
        /* R5RS 4.3.2: "An input form F matches a pattern P if and only if
         [...] P is a datum and F is equal to P in the sense of the equal?
         procedure." */
        return inputDatum.isEqual(this.datum);
    }
};

IdOrLiteralTransformer.prototype.toDatum = function(bindings) {
    return bindings.resolveDatum(this.datum);
};

IdOrLiteralTransformer.prototype.toString = function() {
    return this.datum.toString();
};

IdOrLiteralTransformer.prototype.forEachSubtransformer = function(callback, args) {
    callback(this, args);
};

SchemeMacro.prototype.allPatternsBeginWith = function(kw) {
    for (var i = 0; i < this.transformers.length; ++i)
        if (this.transformers[i].getName() !== kw)
            return false;
    return true;
};

SchemeMacro.prototype.transcribe = function(datum, useEnv) {
    var transformer, bindings, newDatumTree;
    for (var i = 0; i < this.transformers.length; ++i) {
        transformer = this.transformers[i];
        bindings = new TemplateBindings(useEnv, transformer.getPatternIds());
        if (transformer.matchInput(datum, this.literalIdentifiers, this.definitionEnv, useEnv, bindings)
            && (newDatumTree = transformer.template.toDatum(bindings))) {
            // this is a good place to see the TemplateBindings object
            // console.log(bindings.toString());

            var newParseTree = new Parser(newDatumTree).parse();

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
            var toRename = {};
            for (var id in transformer.maybeRename) {
                if (this.definitionEnv.hasBindingRecursive(id) || isParserSensitiveId(id))
                    useEnv.addBinding(id, this.definitionEnv);
                else {
                    toRename[id] = newCpsName();
                }
            }

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
                    function (node) {
                        return node.isIdentifier() && toRename[node.payload];
                    },
                    function (node) {
                        node.payload = toRename[node.payload];
                        return node;
                    }
                );
            } else {
                throw new ParseError(newDatumTree);
            }

            return newParseTree;
        }
    }
    throw new MacroError(this.transformers[0].getName(), 'no pattern match for input ' + datum);
};