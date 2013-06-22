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


goog.provide('r5js.TemplateBindings');

// TODO bl circular dependency! goog.require('r5js.data');
goog.require('r5js.InternalInterpreterError');

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
 * 13. Transcribe ((x ...) (y ...)) with [no more children] => false. Reset cur child.
 * [13 completes as (((a) (b)) ((c e) (d f)))]
 *
 * TODO bl: explain -- or ideally remove -- all the crazy logic dealing
 * with "incorporation". Do we even need it?
 *
 * @param {!r5js.IEnvironment} letSyntaxEnv TODO bl
 * @param {*} patternIds TODO bl
 * @param {*} templateRenameCandidates TODO bl
 * @constructor
 */
r5js.TemplateBindings = function(letSyntaxEnv, patternIds, templateRenameCandidates) {
    /**
     * @type {!Object.<*,*>}
     * TODO bl tighten type and make private
     */
    this.bindings = {};

    /**
     * @type {!Object.<*,*>}
     * TODO bl tighten type and make private
     */
    this.children = [];

    /**
     * @type {number}
     * TODO bl make private
     */
    this.curChild = 0;

    /**
     * @type {!r5js.IEnvironment}
     * TODO bl make private
     */
    this.letSyntaxEnv = letSyntaxEnv;
    this.patternIds = patternIds;
    this.templateRenameCandidates = templateRenameCandidates;

    /**
     * @type {!Object.<*,*>}
     * TODO bl make private
     */
    this.renameInTemplate = {};
};

/**
 * @returns {!r5js.TemplateBindings} This object, for chaining.
 */
r5js.TemplateBindings.prototype.resetCurChild = function() {
    this.curChild = 0;
    return this;
};

r5js.TemplateBindings.prototype.addTemplateBinding = function(name, val) {
    if (this.bindings[name])
        throw new r5js.InternalInterpreterError('invariant incorrect');
    else if (val.isMacro()) {
        // See comments at SchemeMacro.prototype.setIsLetOrLetrecSyntax
        var fakeName = newCpsName();
        this.letSyntaxEnv.addBinding(fakeName, val.getMacro());
        this.bindings[name] = r5js.data.newIdOrLiteral(fakeName);
    }
    else {
        this.bindings[name] = val;
    }

    var self = this;

    /* We have to check the datum to be bound for conflicts with identifiers
    in the template. Example:

    (define-syntax or
        (syntax-rules ()
            (or test1 test2 ...)
            (let ((x test1)) (if x x (or test2 ...)))))

    (let ((x 4) (y 3)) (or x y))

     The identifier x occurs in the template but not the pattern, so it will
     appear in templateRenameCandidates (see Transformer.prototype.setupIds).
     Then, during pattern matching, addTemplateBinding(test1, x) will be
     called. This should signal that, during transcription, any occurrence of
     the _template's_ x should be safely renamed.
     See SchemeMacro.prototype.transcribe. */
    val.forEach(function (datum) {
        if (datum.isIdentifier()
            && self.templateRenameCandidates[datum.payload])
            self.renameInTemplate[datum.payload] = true;
    });
};

/**
 * Purely for debugging.
 * @return {string}
 */
r5js.TemplateBindings.prototype.debugString = function(tabs) {
    tabs = tabs || '';
    var ans = '';
    for (var name in this.bindings) {
        ans += tabs + name + ' = ' + this.bindings[name].toString() + '\n';
    }
    for (var i = 0; i < this.children.length; ++i) {
        ans += tabs + 'child ' + i + ':\n' + this.children[i].debugString(tabs+'\t');
    }
    return ans;
};

/**
 * @param {*} child TODO bl
 * @return {!r5js.TemplateBindings} This object, for chaining.
 */
r5js.TemplateBindings.prototype.addChildBindings = function(child) {
    this.children.push(child);
    return this;
};

/**
 * @param {*} other TODO bl
 * @return {boolean} TODO bl
 */
r5js.TemplateBindings.prototype.hasNoneOf = function(other) {
    for (var name in other.bindings) {
        if (this.bindings[name]) {
            return false;
        }
    }
    return true;
};

/**
 * Try to incorporate the child's bindings in an existing child
 * if there's room, otherwise just tack the child on to the parent.
 * @param {*} child TODO bl
 */
r5js.TemplateBindings.prototype.addOrIncorporateChild = function(child) {
    return this.incorporateChild(child) || this.addChildBindings(child);
};

/**
 * @param {*} child TODO bl
 * @return {r5js.TemplateBindings} This object, or null.
 * TODO bl: what is the reason behind the odd return type variance?
 */
r5js.TemplateBindings.prototype.incorporateChild = function(child) {

    // We only incorporate flat TemplateBindings objects.
    if (child.children.length > 0) {
        return null;
    }

    /* Dump all the child's bindings in the first child that doesn't
     have any of the bindings.

     todo bl: i have no idea why this heuristic seems to work. */
    for (var i = 0; i < this.children.length; ++i) {
        var candidate = this.children[i];
        if (candidate.hasNoneOf(child)) {
            for (var name in child.bindings) {
                candidate.addTemplateBinding(name, child.bindings[name]);
            }
            return this;
        }
    }

    return null;
};

/**
 * @return {*} TODO bl
 */
r5js.TemplateBindings.prototype.getNextChild = function() {
    if (this.curChild < this.children.length) {
        return this.children[this.curChild++];
    } else {
        this.curChild = 0;   // reset for next time
        return null;
    }
};

r5js.TemplateBindings.prototype.resolveDatum = function(datum) {
    if (!this.patternIds)
        throw new r5js.InternalInterpreterError('invariant incorrect');

    if (datum.isIdentifier()) {
        var name = datum.payload;

        var maybe = this.bindings[name];
        if (maybe) {
            return maybe.clone();
        } else if (this.patternIds[name] !== undefined) {
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
            return datum.clone();
        }

    } else {
        return datum.clone();
    }
};

r5js.TemplateBindings.prototype.getPatternIds = function() {
    return this.patternIds;
};

r5js.TemplateBindings.prototype.getTemplateRenameCandidates = function() {
    return this.templateRenameCandidates;
};

r5js.TemplateBindings.prototype.wasRenamed = function(id) {
    return this.renameInTemplate[id];
};