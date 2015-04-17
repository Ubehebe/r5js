goog.module('r5js.EllipsisTransformer');

const Datum = goog.require('r5js.Datum');
const Subtransformer = goog.require('r5js.Subtransformer');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const TemplateBindings = goog.require('r5js.TemplateBindings');

/** @implements {Subtransformer} */
class EllipsisTransformer {
    /** @param {!Subtransformer} subtransformer */
    constructor(subtransformer) {
        /** @const @private */ this.subtransformer_ = subtransformer;
    }

    /** @override */
    matchInput(inputDatum, literalIds, definitionEnv, useEnv, bindings) {

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
        if (!inputDatum) {
            bindings.addChildBindings(
                new TemplateBindings(
                    useEnv,
                    bindings.getPatternIds(),
                    bindings.getTemplateRenameCandidates()));
        }

        for (var subinput = inputDatum;
             subinput;
             subinput = subinput.getNextSibling()) {
            var childBindings = new TemplateBindings(
                useEnv,
                bindings.getPatternIds(),
                bindings.getTemplateRenameCandidates());
            var maybeMatched = this.subtransformer_.matchInput(
                subinput, literalIds, definitionEnv, useEnv, childBindings);
            if (maybeMatched) {
                bindings.addOrIncorporateChild(childBindings);
            } else {
                return false;
            }
        }
        return true;
    }

    /** @override */
    toDatum(bindings) {
        var buf = new SiblingBuffer();
        var bindingsToUse;
        var success;
        while ((bindingsToUse = bindings.getNextChild()) &&
        (success = this.subtransformer_.toDatum(bindingsToUse))) {
            buf.appendSibling(success);
        }
        bindings.resetCurChild();
        return /** @type {!Datum} */ (buf.toSiblings());
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, renameHelper) {
        this.subtransformer_.collectNestingLevels(ellipsisLevel + 1, renameHelper);
    }
}

exports = EllipsisTransformer;
