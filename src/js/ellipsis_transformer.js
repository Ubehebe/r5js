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


goog.provide('r5js.tmp.ellipsis_transformer');

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
        bindings.addChildBindings(
            new TemplateBindings(
                useEnv,
                bindings.getPatternIds(),
                bindings.getTemplateRenameCandidates()));

    for (var subinput = inputDatum; subinput; subinput = subinput.nextSibling) {
        var childBindings = new TemplateBindings(
            useEnv,
            bindings.getPatternIds(),
            bindings.getTemplateRenameCandidates());
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