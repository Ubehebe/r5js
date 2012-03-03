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

var GayLisp = (function() {

    /* The build process inserts src/js/* and src/scm/* here.
     Without these insertions, this file will be useless. */

    // globals.js here
    // Scheme libraries and syntax here
    // All other js here
    // boot.js here

    /* The pipeline is not public because it inputs and outputs
        internal data structures. */
    var pipeline = {

        scan: function(string) {
            return new Scanner(string);
        },

        read: function(scanner) {
            return new Reader(scanner).read();
        },

        parse: function(root, lhs) {
            var ans = new Parser(root).parse(lhs);
            if (ans)
                return ans;
            else throw new ParseError(root);
        },

        desugar: function(root, env) {
            if (!env)
                env = new Environment('global', r5RSEnv);
            return root.desugar(env).setStartingEnv(env);
        },

        eval: function(continuable) {
            return trampoline(continuable, debug);
        }



    };

    /* This is the public API. It runs the above non-public
     pipeline from string input to the relevant stop point.
     The exception is tokenize, which is never really used during
     the normal operation of the interpreter; normally the
     scanner and reader interact on a token-by-token basis,
     rather than scanning the whole input at once. */
    return {
        tokenize: function(string) {
            return new Scanner(string).tokenize();
        },

        read: function(string) {
            var ans =
                pipeline.read(
                    pipeline.scan(string));
            return ans;
        },

        parse: function(string) {
            var ans =
                pipeline.parse(
                    pipeline.read(
                        pipeline.scan(string)));
            return ans;
        },

        eval: function(string) {
            var ans =
                pipeline.eval(
                    pipeline.desugar(
                        pipeline.parse(
                            pipeline.read(
                                pipeline.scan(string)))));
            return ans ? ans.toString() : 'undefined';
        }
    };

})();