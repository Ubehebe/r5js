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

function RootEnvironment(delegate) {
    this.delegate = delegate;
}

RootEnvironment.prototype.toString = function() {
    return this.delegate.toString();
};

RootEnvironment.prototype.get = function(name) {
    if (this.delegate.hasBindingRecursive(name, true))
        return this.delegate.get(name);
    else if (this.lookaside.hasBindingRecursive(name, true))
        return this.lookaside.get(name);
    else throw new UnboundVariable(name + ' in env ' + this.toString());
};

RootEnvironment.prototype.getProcedure = function(name) {
    if (this.delegate.hasBinding(name))
        return this.delegate.getProcedure(name);
    else if (this.lookaside.hasBinding(name))
        return this.lookaside.getProcedure(name);
    else return null;
};

RootEnvironment.prototype.addClosure = function(name, proc) {
    this.delegate.addClosure(name, proc);
};

RootEnvironment.prototype.addBinding = function(name, val) {
    this.delegate.addBinding(name, val);
};

RootEnvironment.prototype.mutate = function(name, newVal, isTopLevel) {
    this.delegate.mutate(name, newVal, isTopLevel);
};

RootEnvironment.prototype.setLookaside = function(lookaside) {
    this.lookaside = lookaside;
    return this;
};

RootEnvironment.prototype.seal = function() {
    this.delegate.seal();
};

RootEnvironment.prototype.hasBinding = function(name) {
    return this.delegate.hasBinding(name);
};

RootEnvironment.prototype.hasBindingRecursive = function(name) {
    return this.delegate.hasBindingRecursive(name);
};