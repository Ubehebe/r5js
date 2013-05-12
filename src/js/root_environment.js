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


goog.provide('r5js.tmp.root_environment');


goog.require('r5js.UnboundVariable');

/**
 * @param {!r5js.IEnvironment} delegate Environment used for most lookups.
 * @constructor
 * @implements {r5js.IEnvironment}
 */
function RootEnvironment(delegate) {
    /**
     * @type {!r5js.IEnvironment}
     * @private
     */
    this.delegate_ = delegate;
}

/** @override */
RootEnvironment.prototype.toString = function() {
    return this.delegate_.toString();
};

/** @override */
RootEnvironment.prototype.get = function(name) {
    if (this.delegate_.hasBindingRecursive(name, true))
        return this.delegate_.get(name);
    else if (this.lookaside.hasBindingRecursive(name, true))
        return this.lookaside.get(name);
    else throw new r5js.UnboundVariable(name + ' in env ' + this.toString());
};

/** @override */
RootEnvironment.prototype.getProcedure = function(name) {
    if (this.delegate_.hasBinding(name))
        return this.delegate_.getProcedure(name);
    else if (this.lookaside.hasBinding(name))
        return this.lookaside.getProcedure(name);
    else return null;
};

/** @override */
RootEnvironment.prototype.addClosure = function(name, proc) {
    this.delegate_.addClosure(name, proc);
};

/** @override */
RootEnvironment.prototype.addBinding = function(name, val) {
    this.delegate_.addBinding(name, val);
};

/** @override */
RootEnvironment.prototype.mutate = function(name, newVal, isTopLevel) {
    this.delegate_.mutate(name, newVal, isTopLevel);
};

/**
 * @param {!r5js.Environment} lookaside Lookaside environment.
 */
RootEnvironment.prototype.setLookaside = function(lookaside) {
    this.lookaside = lookaside;
};

/** @override */
RootEnvironment.prototype.seal = function() {
    this.delegate_.seal();
};

/** @override */
RootEnvironment.prototype.hasBinding = function(name) {
    return this.delegate_.hasBinding(name);
};

/** @override */
RootEnvironment.prototype.hasBindingRecursive = function(
    name, searchClosures) {
    return this.delegate_.hasBindingRecursive(name, false);
};