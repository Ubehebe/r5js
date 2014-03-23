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

goog.require('r5js.InternalInterpreterError');
// TODO bl circular dependency goog.require('r5js.MacroDatum');

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
 * @param {!Object.<string, number>} patternIds TODO bl
 * @param {!Object.<string, boolean>} templateRenameCandidates TODO bl
 * @struct
 * @constructor
 */
r5js.TemplateBindings = function(letSyntaxEnv, patternIds, templateRenameCandidates) {
    /** @const @private {!Object.<string,!r5js.Datum>} */
    this.bindings_ = {};

    /** @const @private {!Array.<!r5js.TemplateBindings>} */
    this.children_ = [];

    /** @private {number} */
    this.curChild_ = 0;

    /** @const @private {!r5js.IEnvironment} */
    this.letSyntaxEnv_ = letSyntaxEnv;

    /** @const @private {!Object.<string, number>} */
    this.patternIds_ = patternIds;

    /** @const {!Object.<string, boolean>} */
    this.templateRenameCandidates_ = templateRenameCandidates;

    /** @const @private {!Object.<*,*>} */
    this.renameInTemplate_ = {};
};

/** @return {!r5js.TemplateBindings} This object, for chaining. */
r5js.TemplateBindings.prototype.resetCurChild = function() {
    this.curChild_ = 0;
    return this;
};


/**
 * @param {string} name
 * @param {!r5js.Datum} val
 */
r5js.TemplateBindings.prototype.addTemplateBinding = function(name, val) {
    if (name in this.bindings_) {
        throw new r5js.InternalInterpreterError('invariant incorrect');
    } else if (val instanceof r5js.MacroDatum) {
        // See comments at SchemeMacro.prototype.setIsLetOrLetrecSyntax
        var fakeName = newCpsName();
        this.letSyntaxEnv_.addBinding(fakeName, val.getMacro());
        this.bindings_[name] = new r5js.ast.Identifier(fakeName);
    } else {
        this.bindings_[name] = val;
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
        if (datum instanceof r5js.ast.Identifier &&
            self.templateRenameCandidates_[/** @type {string} */(
            datum.getPayload())])
            self.renameInTemplate_[datum.getPayload()] = true;
    });
};

/**
 * Purely for debugging.
 * @return {string}
 */
r5js.TemplateBindings.prototype.debugString = function(tabs) {
    tabs = tabs || '';
    var ans = '';
    for (var name in this.bindings_) {
        ans += tabs + name + ' = ' + this.bindings_[name].toString() + '\n';
    }
    for (var i = 0; i < this.children_.length; ++i) {
        ans += tabs + 'child ' + i + ':\n' + this.children_[i].debugString(tabs+'\t');
    }
    return ans;
};

/**
 * @param {!r5js.TemplateBindings} child Child bindings.
 * @return {!r5js.TemplateBindings} This object, for chaining.
 */
r5js.TemplateBindings.prototype.addChildBindings = function(child) {
    this.children_.push(child);
    return this;
};

/**
 * @param {!r5js.TemplateBindings} other Other template bindings.
 * @return {boolean}
 * @private
 */
r5js.TemplateBindings.prototype.hasNoneOf_ = function(other) {
    for (var name in other.bindings_) {
        if (name in this.bindings_) {
            return false;
        }
    }
    return true;
};

/**
 * Try to incorporate the child's bindings in an existing child
 * if there's room, otherwise just tack the child on to the parent.
 * @param {!r5js.TemplateBindings} child Child bindings.
 * @return {r5js.TemplateBindings}
 */
r5js.TemplateBindings.prototype.addOrIncorporateChild = function(child) {
    return this.incorporateChild(child) || this.addChildBindings(child);
};

/**
 * @param {!r5js.TemplateBindings} child Child bindings.
 * @return {r5js.TemplateBindings} This object, or null.
 */
r5js.TemplateBindings.prototype.incorporateChild = function(child) {

    // We only incorporate flat TemplateBindings objects.
    if (child.children_.length > 0) {
        return null;
    }

    /* Dump all the child's bindings in the first child that doesn't
     have any of the bindings.

     todo bl: i have no idea why this heuristic seems to work. */
    for (var i = 0; i < this.children_.length; ++i) {
        var candidate = this.children_[i];
        if (candidate.hasNoneOf_(child)) {
            for (var name in child.bindings_) {
                candidate.addTemplateBinding(name, child.bindings_[name]);
            }
            return this;
        }
    }

    return null;
};

/** @return {r5js.TemplateBindings} */
r5js.TemplateBindings.prototype.getNextChild = function() {
    if (this.curChild_ < this.children_.length) {
        return this.children_[this.curChild_++];
    } else {
        this.curChild_ = 0;   // reset for next time
        return null;
    }
};


/**
 * @param {!r5js.Datum} datum
 * @return {!r5js.Datum|boolean}
 * TODO bl document what this does.
 */
r5js.TemplateBindings.prototype.resolveDatum = function(datum) {
    if (!this.patternIds_)
        throw new r5js.InternalInterpreterError('invariant incorrect');

    if (datum instanceof r5js.ast.Identifier) {
        var name = /** @type {string} */(datum.getPayload());

        var maybe = this.bindings_[name];
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
};

/** @return {!Object.<string, number>} */
r5js.TemplateBindings.prototype.getPatternIds = function() {
    return this.patternIds_;
};

/** @return {!Object.<string, boolean>} */
r5js.TemplateBindings.prototype.getTemplateRenameCandidates = function() {
    return this.templateRenameCandidates_;
};

/**
 * @param {string} id
 * @return {*} TODO bl
 */
r5js.TemplateBindings.prototype.wasRenamed = function(id) {
    return this.renameInTemplate_[id];
};