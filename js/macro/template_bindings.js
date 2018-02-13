goog.module('r5js.TemplateBindings');

const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const Identifier = goog.require('r5js.ast.Identifier');
const Macro = goog.require('r5js.ast.Macro');
const {newCpsName} = goog.require('r5js.RenameUtil');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');

/**
 * My approach for supporting nested ellipses in macro transcriptions
 * is to take a single pass through the input and build up a TemplateBindings
 * object whose tree structure mirrors the ellipsis nesting in the pattern.
 * For example:
 *
 * (define-syntax foo
 *  (syntax-rules ()
 *      ((foo ((x y) ...) ...)
 *      (quote (((x ...) (y ...)) ...)))))
 *
 * with input
 *
 * (foo ((a b)) ((c d) (e f)))
 *
 * produces this TemplateBindings object:
 *
 * child 0:
 *  child 0:
 *      x = a
 *      y = b
 * child 1:
 *  child 0:
 *      x = c
 *      y = d
 *  child 1:
 *      x = e
 *      y = f
 *
 * Then transcription involves a single pass through the template with
 * this TemplateBindings object, using the ellipses in the template
 * to descend through the TemplateBindings tree. Here's the flow of control
 * during transcription:
 *
 * 1. Transcribe ((x ...) (y ...)) with child0
 * 2. Transcribe x with child0.child0 => a
 * 3. Transcribe x with [no more children] => false. Reset cur child.
 * 4. Transcribe y with child0.child0 => b
 * 5. Transcribe y with [no more children] => false. Reset cur child.
 * [1 completes as ((a) (b))]
 * 6. Transcribe ((x ...) (y ...)) with child1
 * 7. Transcribe x with child1.child0 => c
 * 8. Transcribe x with child1.child1 => e
 * 9. Transcribe x with [no more children] => false. Reset cur child.
 * 10. Transcribe y with child1.child0 => d
 * 11. Transcribe y with chid1.child1 => f
 * 12. Transcribe y with [no more children] => false. Reset cur child.
 * [6 completes as ((c e) (d f))]
 * 13. Transcribe ((x ...) (y ...)) with [no more children] => false.
 * Reset cur child.
 * [13 completes as (((a) (b)) ((c e) (d f)))]
 *
 * TODO bl: explain -- or ideally remove -- all the crazy logic dealing
 * with "incorporation". Do we even need it?
 */
class TemplateBindings {
    /**
     * @param {!IEnvironment} letSyntaxEnv TODO bl.
     * @param {!Object<string, number>} patternIds TODO bl.
     * @param {!Object<string, boolean>} templateRenameCandidates TODO bl.
     */
    constructor(letSyntaxEnv, patternIds, templateRenameCandidates) {
        /** @const @private {!Object<string,!Datum>} */
        this.bindings_ = {};

        /** @const @private {!Array<!TemplateBindings>} */
        this.children_ = [];

        /** @private {number} */
        this.curChild_ = 0;

        /** @const @private {!IEnvironment} */
        this.letSyntaxEnv_ = letSyntaxEnv;

        /** @const @private {!Object<string, number>} */
        this.patternIds_ = patternIds;

        /** @const @private {!Object<string, boolean>} */
        this.templateRenameCandidates_ = templateRenameCandidates;

        /** @const @private {!Object<string,*>} */
        this.renameInTemplate_ = {};
    }

    /** @return {!TemplateBindings} This object, for chaining. */
    resetCurChild() {
        this.curChild_ = 0;
        return this;
    }

    /**
     * @param {string} name
     * @param {!Datum} val
     */
    addTemplateBinding(name, val) {
        if (name in this.bindings_) {
            throw Error.internalInterpreterError('invariant incorrect');
        } else if (val instanceof Macro) {
            // See comments at SchemeMacro.prototype.setIsLetOrLetrecSyntax
            const fakeName = newCpsName();
            this.letSyntaxEnv_.addBinding(fakeName, val.getMacro());
            this.bindings_[name] = new Identifier(fakeName);
        } else {
            this.bindings_[name] = val;
        }

        if (val instanceof Identifier) {
            this.maybeRenameId_(val);
        } else if (val instanceof CompoundDatum) {
            val.forEachChild(this.maybeRenameId_, this);
        }
    }

    /**
     * We have to check the datum to be bound for conflicts with identifiers
     * in the template. Example:
     *
     * (define-syntax or
     * (syntax-rules ()
     * (or test1 test2 ...)
     * (let ((x test1)) (if x x (or test2 ...)))))
     *
     * (let ((x 4) (y 3)) (or x y))
     *
     * The identifier x occurs in the template but not the pattern, so it will
     * appear in templateRenameCandidates (see Transformer.prototype.setupIds).
     * Then, during pattern matching, addTemplateBinding(test1, x) will be
     * called. This should signal that, during transcription, any occurrence of
     * the _template's_ x should be safely renamed.
     * @see {r5js.Macro#transcribe}.
     *
     * @param {!Datum} datum
     * @private
     */
    maybeRenameId_(datum) {
        if (datum instanceof Identifier) {
            const id = /** @type {string} */ (datum.getPayload());
            if (this.templateRenameCandidates_[id]) {
                this.renameInTemplate_[id] = true;
            }
        } else if (datum instanceof CompoundDatum) {
            datum.forEachChild(this.maybeRenameId_, this);
        }
    }

    /**
     * @param {!TemplateBindings} child Child bindings.
     * @return {!TemplateBindings} This object, for chaining.
     */
    addChildBindings(child) {
        this.children_.push(child);
        return this;
    }

    /**
     * @param {!TemplateBindings} other Other template bindings.
     * @return {boolean}
     * @private
     */
    hasNoneOf_(other) {
        for (const name in other.bindings_) {
            if (name in this.bindings_) {
                return false;
            }
        }
        return true;
    }

    /**
     * Try to incorporate the child's bindings in an existing child
     * if there's room, otherwise just tack the child on to the parent.
     * @param {!TemplateBindings} child Child bindings.
     * @return {!TemplateBindings}
     */
    addOrIncorporateChild(child) {
        return this.incorporateChild(child) || this.addChildBindings(child);
    }

    /**
     * @param {!TemplateBindings} child Child bindings.
     * @return {?TemplateBindings} This object, or null.
     */
    incorporateChild(child) {
        // We only incorporate flat TemplateBindings objects.
        if (child.children_.length > 0) {
            return null;
        }

        /* Dump all the child's bindings in the first child that doesn't
         have any of the bindings.

         todo bl: i have no idea why this heuristic seems to work. */
        for (let i = 0; i < this.children_.length; ++i) {
            const candidate = this.children_[i];
            if (candidate.hasNoneOf_(child)) {
                for (const name in child.bindings_) {
                    candidate.addTemplateBinding(name, child.bindings_[name]);
                }
                return this;
            }
        }

        return null;
    }

    /** @return {?TemplateBindings} */
    getNextChild() {
        if (this.curChild_ < this.children_.length) {
            return this.children_[this.curChild_++];
        } else {
            this.curChild_ = 0;   // reset for next time
            return null;
        }
    }

    /**
     * @param {!Datum} datum
     * @return {!Datum|boolean}
     * TODO bl document what this does.
     */
    resolveDatum(datum) {
        if (datum instanceof Identifier) {
            const name = /** @type {string} */(datum.getPayload());

            const maybe = this.bindings_[name];
            if (maybe) {
                return maybe.clone(null /* parent */);
            } else if (this.patternIds_[name] !== undefined) {
                /* It's important to return false here, instead of some other
                 "falsey" value like null. This value is immediately returned by
                 IdOrLiteralTransformer.prototype.matchInput. Meanwhile,
                 EllipsisTransformer.prototype.matchInput returns
                 new r5js.SiblingBuffer().toList() when it has successfully matched the
                 ellipsis zero times, which is not a failure. And if you look at
                 the implementation, you will see there is a good reason that

                 new r5js.SiblingBuffer().toList() === null.

                 So we have to return something different.

                 Static types would be useful here. */
                return false;
            } else {
                return datum.clone(null /* parent */);
            }

        } else {
            return datum.clone(null /* parent */);
        }
    }

    /** @return {!Object<string, number>} */
    getPatternIds() {
        return this.patternIds_;
    }

    /** @return {!Object<string, boolean>} */
    getTemplateRenameCandidates() {
        return this.templateRenameCandidates_;
    }

    /**
     * @param {string} id
     * @return {*} TODO bl.
     */
    wasRenamed(id) {
        return this.renameInTemplate_[id];
    }
}

exports = TemplateBindings;
