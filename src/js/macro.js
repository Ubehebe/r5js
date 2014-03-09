/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.Macro');


goog.require('r5js.MacroError');
goog.require('r5js.ParseError');
goog.require('r5js.TemplateBindings');
goog.require('r5js.Transformer');

/**
 * @param {!r5js.Datum} literalIdentifiers
 * @param {!r5js.Datum} rules
 * @param {!r5js.IEnvironment} definitionEnv
 * @constructor
 * @suppress {accessControls} for the raw access of nextSibling_
 */
r5js.Macro = function(literalIdentifiers, rules, definitionEnv) {

    /**
     * @type {!r5js.IEnvironment}
     */
    this.definitionEnv = definitionEnv;

    /**
     * @type {!Object.<string, boolean>}
     */
    this.literalIdentifiers = {};

    for (var curId = literalIdentifiers; curId; curId = curId.nextSibling_) {
        this.literalIdentifiers[curId.payload_] = true;
    }

    /**
     * @type {!Array.<!r5js.Transformer>}
     */
    this.transformers = [];

    for (var rule = rules; rule; rule = rule.getNextSibling()) {
        var pattern = /** @type {!r5js.ListLikeTransformer} */(
            rule.at('pattern').desugar(definitionEnv));
        var template = /** @type {!r5js.ListLikeTransformer} */ (
            rule.at('template').desugar(definitionEnv));
        var transformer = new r5js.Transformer(pattern, template);
        this.transformers.push(transformer);
    }
};

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
r5js.Macro.prototype.setIsLetOrLetrecSyntax = function() {
    this.isLetOrLetrecSyntax = true;
    return this;
};

/**
 * Should only be used during interpreter bootstrapping.
 * @return {!r5js.Macro} A clone of this macro.
 * TODO bl: this is almost certainly a bug. The types of the parameters
 * passed in to the constructor in this method seem to be different than
 * the types of the parameters passed in the only other use of the constructor
 * (in the parser). Yet removing this method and changing its callers to
 * use the uncloned Macro cause tests to fail.
 * @suppress {checkTypes}
 */
r5js.Macro.prototype.clone = function(newDefinitionEnv) {
    var ans = new r5js.Macro(this.literalIdentifiers, null, newDefinitionEnv);
    ans.transformers = this.transformers;
    return ans;
};


/**
 * @param {string} kw Keyword to test for.
 * @return {boolean} True iff all of the macro's patterns begin with kw.
 */
r5js.Macro.prototype.allPatternsBeginWith = function(kw) {
    for (var i = 0; i < this.transformers.length; ++i) {
        if (this.transformers[i].getName() !== kw) {
            return false;
        }
    }
    return true;
};


/**
 * @param {!r5js.Datum} datum Datum to transcribe.
 * @param {!r5js.IEnvironment} useEnv Environment to use for the transcription.
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum. This is a hack to avoid
 * instantiating a Parser directly in this file, which would cause
 * a cyclic dependency between macro.js and parse.js.
 * @return {?} TODO bl
 */
r5js.Macro.prototype.transcribe = function(datum, useEnv, parserProvider) {
    var transformer, bindings, newDatumTree;
    for (var i = 0; i < this.transformers.length; ++i) {
        transformer = this.transformers[i];
        bindings = new r5js.TemplateBindings(useEnv, transformer.getPatternIds(), transformer.getTemplateRenameCandidates());
        if (transformer.matchInput(datum, this.literalIdentifiers, this.definitionEnv, useEnv, bindings)
            && (newDatumTree = transformer.getTemplate().toDatum(bindings))) {
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
            var candidates = transformer.getTemplateRenameCandidates();
            for (var id in candidates) {
                if (this.definitionEnv.hasBindingRecursive(id, false))
                    useEnv.addBinding(id, this.definitionEnv);
                else if (!isParserSensitiveId(id)) {
                    var tmpName = newCpsName();
                    toRename[id] = tmpName;
                    /* If the TemplateBindings object has detected that the same
                     identifier is used in the input and (unrelatedly) in the template, id
                     may be replaced in the template, so we have to manually add
                     the binding here. See the logic at the end of
                     TemplateBindings.prototype.addTemplateBinding. */
                    if (bindings.wasRenamed(id)
                        && useEnv.hasBindingRecursive(id, false)) {
                        useEnv.addBinding(tmpName, useEnv.get(id));
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
                var fake = newEmptyList();
                fake.appendChild(newParseTree);
                fake.replaceChildren(
                    function (node) {
                        return node.isIdentifier() && toRename[node.getPayload()];
                    },
                    function (node) {
                        node.payload_ = toRename[node.getPayload()];
                        return node;
                    }
                );
            } else {
                throw new r5js.ParseError(newDatumTree);
            }

            return newParseTree;
        }
    }
    throw new r5js.MacroError(this.transformers[0].getName(), 'no pattern match for input ' + datum);
};