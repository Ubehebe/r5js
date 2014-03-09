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


goog.provide('r5js.EllipsisTransformer');


goog.require('r5js.SiblingBuffer');
goog.require('r5js.TemplateBindings');



/**
 * @param {!r5js.ITransformer} subtransformer Subtransformer.
 * @implements {r5js.ITransformer}
 * @struct
 * @constructor
 */
r5js.EllipsisTransformer = function(subtransformer) {
  /** @const @private {!r5js.ITransformer} */
  this.subtransformer_ = subtransformer;
};


/** @override */
r5js.EllipsisTransformer.prototype.matchInput = function(
    inputDatum, literalIds, definitionEnv, useEnv, bindings) {

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
        new r5js.TemplateBindings(
        useEnv,
        bindings.getPatternIds(),
        bindings.getTemplateRenameCandidates()));
  }

  for (var subinput = inputDatum;
       subinput;
       subinput = subinput.getNextSibling()) {
    var childBindings = new r5js.TemplateBindings(
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
};


/** @override */
r5js.EllipsisTransformer.prototype.toDatum = function(bindings) {
  var buf = new r5js.SiblingBuffer();
  var bindingsToUse;
  var success;
  while ((bindingsToUse = bindings.getNextChild()) &&
      (success = this.subtransformer_.toDatum(bindingsToUse))) {
    buf.appendSibling(success);
  }
  bindings.resetCurChild();
  return buf.toSiblings();
};


/**
 * @override
 * @suppress {checkTypes} TODO bl the compiler complains about an incorrect
 * override from the interface definition. Why?
 */
r5js.EllipsisTransformer.prototype.forEachSubtransformer = function(
    callback, args, opt_context) {
  callback.call(opt_context, this.subtransformer_, args);
};
