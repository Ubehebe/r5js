goog.provide('r5js.Macro');

goog.require('r5js.Datum');
goog.require('r5js.ProcCallLike');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TemplateBindings');
goog.require('r5js.Transformer');
goog.require('r5js.ast.Identifier');
goog.require('r5js.ast.List');
goog.require('r5js.error');
goog.require('r5js.RenameUtil');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.runtime.ObjectValue');

r5js.Macro = /** @implements {r5js.runtime.ObjectValue} TODO bl almost certainly wrong */ class {

    /**
     * @param {r5js.Datum} literalIdentifiers
     * @param {r5js.Datum} rules
     * @param {!r5js.IEnvironment} definitionEnv
     * @param {!Array<!r5js.Transformer>=} opt_transformers
     */
    constructor(literalIdentifiers, rules, definitionEnv, opt_transformers) {
        /** @private {!r5js.IEnvironment} */
        this.definitionEnv_ = definitionEnv;

        /** @const @private {!Object<string, boolean>} */
        this.literalIdentifiers_ = {};

        for (var curId = literalIdentifiers; curId; curId = curId.getNextSibling()) {
            this.literalIdentifiers_[(/** @type {!r5js.ast.Identifier} */(
                curId)).getPayload()] = true;
        }

        /** @const @private {!Array<!r5js.Transformer>} */
        this.transformers_ = opt_transformers || [];

        if (!opt_transformers) {
            for (var rule = rules; rule; rule = rule.getNextSibling()) {
                // TODO bl improve
                rule = /** @type {!r5js.ast.CompoundDatum} */ (rule);
                var pattern = /** @type {!r5js.ListLikeTransformer} */(
                    rule.at(r5js.parse.Nonterminals.PATTERN).desugar(definitionEnv));
                var template = /** @type {!r5js.ListLikeTransformer} */ (
                    rule.at(r5js.parse.Nonterminals.TEMPLATE).desugar(definitionEnv));
                var transformer = new r5js.Transformer(pattern, template);
                this.transformers_.push(transformer);
            }
        }

        /** @private {boolean} */
        this.isLetOrLetrecSyntax_ = false;
    }

    /**
     * Workaround for let-syntax and letrec-syntax.
     * This implementation rewrites let-syntax and letrec-syntax
     * as let and letrec respectively. For example,
     *
     * (let-syntax ((foo (syntax-rules () ((foo) 'hi)))) ...)
     *
     * desugars as
     *
     * (let ((foo [SchemeMacro object wrapped in a Datum])) ...)
     *
     * When this macro use is matched against the definition of let,
     * the wrapped SchemeMacro object will be added to the TemplateBindings
     * correctly. The problem arises during transcription: we cannot insert
     * the wrapped SchemeMacro object directly into the new parse tree,
     * because that parse tree will be handed to the parser, which won't know
     * what to do with SchemeMacros.
     *
     * Indirection comes to the rescue. We insert a new identifier node and
     * bind it in the current environment to the SchemeMacro. Later, on
     * the trampoline, we will look up the value of that identifier and
     * find the SchemeMacro as desired.
     *
     * We have to set the isLetOrLetrecSyntax flag on the macro to disallow
     * this behavior from the programmer. We cannot allow her to write
     *
     * (let ((x let*)) x)
     *
     * @return {!r5js.Macro} This object.
     */
    setIsLetOrLetrecSyntax() {
        this.isLetOrLetrecSyntax_ = true;
        return this;
    }

    /** @return {boolean} */
    isLetOrLetrecSyntax() {
        return this.isLetOrLetrecSyntax_;
    }

    /** @param {!r5js.IEnvironment} env */
    setDefinitionEnv(env) {
        this.definitionEnv_ = env;
    }

    /**
     * Should only be used during interpreter bootstrapping.
     * @param {!r5js.IEnvironment} newDefinitionEnv
     * @return {!r5js.Macro} A clone of this macro.
     */
    clone(newDefinitionEnv) {
        return new r5js.Macro(null, null, newDefinitionEnv, this.transformers_);
    }

    /**
     * @param {string} kw Keyword to test for.
     * @return {boolean} True iff all of the macro's patterns begin with kw.
     */
    allPatternsBeginWith(kw) {
        for (var i = 0; i < this.transformers_.length; ++i) {
            if (this.transformers_[i].getName() !== kw) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param {!r5js.Datum} datum Datum to transcribe.
     * @param {!r5js.IEnvironment} useEnv Environment to use for the transcription.
     * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
     * that will return a new Parser for the given Datum. This is a hack to avoid
     * instantiating a Parser directly in this file, which would cause
     * a cyclic dependency between macro.js and parse.js.
     * @return {!r5js.Datum}
     */
    transcribe(datum, useEnv, parserProvider) {
        var transformer, bindings, newDatumTree;
        for (var i = 0; i < this.transformers_.length; ++i) {
            transformer = this.transformers_[i];
            bindings = new r5js.TemplateBindings(
                useEnv,
                transformer.getPatternIds(),
                transformer.getTemplateRenameCandidates());
            if (transformer.matchInput(
                    datum,
                    this.literalIdentifiers_,
                    this.definitionEnv_,
                    useEnv,
                    bindings) &&
                (newDatumTree = transformer.getTemplate().toDatum(bindings))) {
                // this is a good place to see the TemplateBindings object
                // console.log(bindings.toString());

                var newParseTree = parserProvider(newDatumTree).parse();

                /* R5RS 4.3: "If a macro transformer inserts a binding for an identifier
                 (variable or keyword), the identifier will in effect be renamed
                 throughout its scope to avoid conflicts with other identifiers.

                 "If a macro transformer inserts a free reference to an
                 identifier, the reference refers to the binding that was visible
                 where the transformer was specified, regardless of any local bindings
                 that may surround the use of the macro."

                 It's easy to collect the set of identifiers inserted by a macro
                 transformer: it's the set of identifiers in the template minus the set
                 of identifiers in the pattern. But how do we determine which of these
                 are supposed to be "free" and which are bindings and thus should be
                 renamed?

                 My current heuristic is to do a lookup in the macro's definition
                 environment. If we find something, the identifier is probably
                 supposed to refer to that. For example, the "+" in the pattern of

                 (define-syntax foo (syntax-rules () ((foo x) (+ x x))))

                 If we don't find a binding in the macro's definition environment, we
                 suppose this is a new binding inserted by the transformer and
                 defensively rename it.

                 I don't think this is correct, but it works for the letrec
                 macro definition, which is the most complex case I've tried so far. */
                var toRename = {};
                var candidates = transformer.getTemplateRenameCandidates();
                for (var id in candidates) {
                    if (this.definitionEnv_.hasBindingRecursive(id))
                        useEnv.addBinding(id, this.definitionEnv_);
                    else if (!r5js.RenameUtil.isParserSensitiveId(id)) {
                        var tmpName = r5js.RenameUtil.newCpsName();
                        toRename[id] = tmpName;
                        /* If the TemplateBindings object has detected that the same
                         identifier is used in the input and (unrelatedly) in the template,
                         id may be replaced in the template, so we have to manually add
                         the binding here. See the logic at the end of
                         TemplateBindings.prototype.addTemplateBinding. */
                        if (bindings.wasRenamed(id) &&
                            useEnv.hasBindingRecursive(id)) {
                            var binding = useEnv.get(id);
                            if (binding !== null) {
                                useEnv.addBinding(tmpName, binding);
                            }
                        }
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
                    var fake = new r5js.SiblingBuffer().
                        appendSibling(newParseTree).
                        toList(r5js.ast.List);
                    fake.replaceChildren(
                        function (node) {
                            return node instanceof r5js.ast.Identifier &&
                                toRename[node.getPayload()];
                        },
                        function (node) {
                            node = /** @type {!r5js.ast.Identifier} */ (node);
                            node.setPayload(toRename[node.getPayload()]);
                            return node;
                        }
                    );
                } else {
                    throw r5js.error.parse(newDatumTree);
                }

                return newParseTree;
            }
        }
        throw r5js.error.macro(
            this.transformers_[0].getName(), 'no pattern match for input ' + datum);
    }

    /**
     * @param {!r5js.Datum} rawDatum
     * @param {!r5js.ProcCallLike} procCallLike
     * @param {!r5js.TrampolineHelper} resultStruct
     * @param {function(!r5js.Datum):!r5js.Parser} parserProvider
     */
    evaluate(rawDatum, procCallLike, resultStruct, parserProvider) {
        var oldEnv = procCallLike.getEnv();
        var newEnv = oldEnv.child();
        var newParseTree = this.transcribe(rawDatum, newEnv, parserProvider);

        var next = procCallLike.getNext();
        /* Just like with tryNonPrimitiveProcedures, we have to remember when
         to jump back to the old environment. */
        if (oldEnv && next && !next.getEnv()) {
            next.setStartingEnv(oldEnv);
        }

        var newContinuable = /** @type {!r5js.ProcCallLike} */ (
            newParseTree.desugar(newEnv, true));
        newContinuable.setStartingEnv(newEnv);

        var last = r5js.ProcCallLike.getLast(newContinuable);
        if (next) {
            last.setNext(next);
        }
        last.setResultName(procCallLike.getResultName());
        resultStruct.setNext(newContinuable);
    }
};