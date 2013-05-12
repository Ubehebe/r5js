/* Copyright 2011-2013 Brendan Linn

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


goog.provide('r5js.IEnvironment');

/**
 * Interface abstracted from {@link Environment} and {@link RootEnvironment}.
 * @interface
 */
r5js.IEnvironment = function() {};


/**
 * @param {string} name Name of the binding.
 * @param {*} val Value of the binding.
 * TODO bl: tighten the type of the value.
 */
r5js.IEnvironment.prototype.addBinding = function(name, val) {};


/**
 * @param {string} name Name of the binding to install this closure under.
 * @param {!SchemeProcedure} proc Closure to install.
 * TODO bl: consider renaming to addSchemeProcedure?
 */
r5js.IEnvironment.prototype.addClosure = function(name, proc) {};


/**
 * @param {string} name Name of binding to get.
 * @return {*} Value of binding, if any.
 * TODO bl: tighten the return type.
 */
r5js.IEnvironment.prototype.get = function(name) {};


/**
 * @param {string} name Name of the procedure to get.
 */
r5js.IEnvironment.prototype.getProcedure = function(name) {};


/**
 * @param {string} name Name of the binding to look up.
 * @return {boolean} True iff the environment has a binding for the name.
 */
r5js.IEnvironment.prototype.hasBinding = function(name) {};


/**
 * @param {string} name Name of the binding to look up.
 * @param {boolean} searchClosures True iff closures should be searched.
 * @return {boolean} True iff the environment, or any of its enclosing
 * environments, has a binding for the name.
 */
r5js.IEnvironment.prototype.hasBindingRecursive = function(
    name, searchClosures) {};


/**
 * @param {string} name Name of the binding.
 * @param {*} newVal New value of the binding.
 * @param {boolean} isTopLevel True iff the binding should be top-level.
 */
r5js.IEnvironment.prototype.mutate = function(name, newVal, isTopLevel) {};


r5js.IEnvironment.prototype.seal = function() {};

