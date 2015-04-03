goog.module('r5js.EllipsisTransformer');

const ITransformer = goog.require('r5js.ITransformer');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const TemplateBindings = goog.require('r5js.TemplateBindings');

/** @implements {r5js.ITransformer} */
class EllipsisTransformer {
    /** @param {!r5js.ITransformer} subtransformer Subtransformer. */
    constructor(subtransformer) {
        /** @const @private {!r5js.ITransformer} */
        this.subtransformer_ = subtransformer;
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
        return /** @type {!r5js.Datum} */ (buf.toSiblings());
    }

    /** @override */
    collectNestingLevels(ellipsisLevel, transformer) {
        this.subtransformer_.collectNestingLevels(ellipsisLevel + 1, transformer);
    }
}

exports = EllipsisTransformer;
