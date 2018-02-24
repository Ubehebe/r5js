goog.module('r5js.Macro');

const {CompoundDatum} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/compound_datum');
const {Datum, ProcCallLike, ProcCallResult, getLastProcCallLike} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Identifier} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const ListLikeTransformer = goog.require('r5js.ListLikeTransformer');
const {SiblingBuffer} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/sibling_buffer');
const TemplateBindings = goog.require('r5js.TemplateBindings');
const Transformer = goog.require('r5js.Transformer');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {List} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/list');
const {PATTERN, TEMPLATE} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {isParserSensitiveId, newCpsName} = require('/js/parse/rename_util_collect_es6_sources.es6/node_modules/__main__/js/parse/rename_util');

/** @implements {ObjectValue} TODO bl almost certainly wrong */
class Macro {

    /**
     * @param {?Datum} literalIdentifiers
     * @param {?Datum} rules
     * @param {!IEnvironment} definitionEnv
     * @param {!Array<!Transformer>=} transformers
     */
    constructor(literalIdentifiers, rules, definitionEnv, transformers=[]) {
        /** @private {!IEnvironment} */ this.definitionEnv_ = definitionEnv;
        /** @const @private {!Object<string, boolean>} */
        this.literalIdentifiers_ = {};

        for (let curId = literalIdentifiers; curId; curId = curId.getNextSibling()) {
            this.literalIdentifiers_[(/** @type {!Identifier} */(curId)).getPayload()] = true;
        }

        /** @const @private {!Array<!Transformer>} */
        this.transformers_ = transformers;

        if (!transformers.length) {
            for (let rule = rules; rule; rule = rule.getNextSibling()) {
                // TODO bl improve
                rule = /** @type {!CompoundDatum} */ (rule);
                const pattern = /** @type {!ListLikeTransformer} */(
                    rule.at(PATTERN).desugar(definitionEnv));
                const template = /** @type {!ListLikeTransformer} */ (
                    rule.at(TEMPLATE).desugar(definitionEnv));
                const transformer = new Transformer(pattern, template);
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
     * @return {!Macro} This object.
     */
    setIsLetOrLetrecSyntax() {
        this.isLetOrLetrecSyntax_ = true;
        return this;
    }

    /** @return {boolean} */
    isLetOrLetrecSyntax() {
        return this.isLetOrLetrecSyntax_;
    }

    /** @param {!IEnvironment} env */
    setDefinitionEnv(env) {
        this.definitionEnv_ = env;
    }

    /**
     * Should only be used during interpreter bootstrapping.
     * @param {!IEnvironment} newDefinitionEnv
     * @return {!Macro} A clone of this macro.
     */
    clone(newDefinitionEnv) {
        return new Macro(null, null, newDefinitionEnv, this.transformers_);
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
     * @param {!Datum} datum Datum to transcribe.
     * @param {!IEnvironment} useEnv Environment to use for the transcription.
     * @param {function(!Datum):!r5js.Parser} parserProvider Function
     * that will return a new Parser for the given Datum. This is a hack to avoid
     * instantiating a Parser directly in this file, which would cause
     * a cyclic dependency between macro.js and parse.js.
     * @return {!Datum}
     */
    transcribe(datum, useEnv, parserProvider) {
        let transformer, bindings, newDatumTree;
        for (let i = 0; i < this.transformers_.length; ++i) {
            transformer = this.transformers_[i];
            bindings = new TemplateBindings(
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
                const candidates = transformer.getTemplateRenameCandidates();
                for (const id in candidates) {
                    if (this.definitionEnv_.hasBindingRecursive(id)) {
                        // TODO: this is deeply weird. Because of this line,
                        // IEnvironment has to extend ObjectValue. Which it shouldn't.
                        // Find another way to do this.
                        useEnv.addBinding(id, this.definitionEnv_);
                    } else if (!isParserSensitiveId(id)) {
                        var tmpName = newCpsName();
                        toRename[id] = tmpName;
                        /* If the TemplateBindings object has detected that the same
                         identifier is used in the input and (unrelatedly) in the template,
                         id may be replaced in the template, so we have to manually add
                         the binding here. See the logic at the end of
                         TemplateBindings.prototype.addTemplateBinding. */
                        if (bindings.wasRenamed(id) && useEnv.hasBindingRecursive(id)) {
                            const binding = useEnv.get(id);
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
                    const fake = new SiblingBuffer()
                        .appendSibling(newParseTree)
                        .toList(List);
                    fake.replaceChildren(
                        function (node) {
                            return node instanceof Identifier && toRename[node.getPayload()];
                        },
                        function (node) {
                            node = /** @type {!Identifier} */ (node);
                            node.setPayload(toRename[node.getPayload()]);
                            return node;
                        }
                    );
                } else {
                    throw Error.parse(newDatumTree);
                }

                return newParseTree;
            }
        }
        throw Error.macro(
            this.transformers_[0].getName(), 'no pattern match for input ' + datum);
    }

    /**
     * @param {!Datum} rawDatum
     * @param {!ProcCallLike} procCallLike
     * @param {!ProcCallResult} resultStruct
     * @param {function(!Datum):!r5js.Parser} parserProvider
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

        var newContinuable = /** @type {!ProcCallLike} */ (
            newParseTree.desugar(newEnv, true));
        newContinuable.setStartingEnv(newEnv);

        var last = getLastProcCallLike(newContinuable);
        if (next) {
            last.setNext(next);
        }
        last.setResultName(procCallLike.getResultName());
        resultStruct.setNext(newContinuable);
    }
}

exports = Macro;