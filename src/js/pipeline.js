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

function Pipeline() {}

Pipeline.prototype.setRootEnv = function(rootEnv) {
    this.rootEnv = rootEnv;
    this.env = new Environment('global', rootEnv);
};

Pipeline.prototype.scan = function(string) {
    return new Scanner(string);
};

Pipeline.prototype.read = function(scanner) {
    return new Reader(scanner).read();
};

Pipeline.prototype.parse = function(root, lhs) {
    var ans = new Parser(root).parse(lhs);
    if (ans)
        return ans;
    else throw new ParseError(root);
};

Pipeline.prototype.desugar = function(root, replMode) {
    if (!replMode) {
        this.env = new Environment('global', this.rootEnv);
    }
    return root.desugar(this.env).setStartingEnv(this.env);
};

Pipeline.prototype.eval = function(continuable, sideEffectHandler) {
    return trampoline(continuable, null, sideEffectHandler, debug);
};