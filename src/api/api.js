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

    /* The build process inserts src/js/* here (after the first opening brace
     in this file).. It also inserts src/scm/* here, embedded as JavaScript string
     literals and appropriately escaped. See the Makefile for details. */

    /* The pipeline is not public because it inputs and outputs
        internal data structures. */
    var pipeline = new LazyBoot(new Pipeline(), function() {
        bootstrap(syntax, procedures);
        pipeline.delegate.setRootEnv(r5RSEnv);
    });

    /* This is the public API. It mainly runs the above non-public
     pipeline from string input to the relevant stop point.

     The names are quoted in order to prevent the Google Closure Compiler
     from renaming the public API methods. */
    var publicApi = {
        'test': function(unitTestUrl) {
            testScanner();
            testParser();
            if (unitTestUrl)
                publicApi['evalUrl'](unitTestUrl);
        },

        'tokenize': function(string) {
            return new Scanner(string).tokenize();
        },

        'read': function(string) {
            var ans =
                pipeline.read(
                    pipeline.scan(string));
            return ans;
        },

        'parse': function(string) {
            var ans =
                pipeline.parse(
                    pipeline.read(
                        pipeline.scan(string)));
            return ans;
        },

        'eval': function(string) {
            var ans =
                pipeline.eval(
                    pipeline.desugar(
                        pipeline.parse(
                            pipeline.read(
                                pipeline.scan(string)))));
            return ans == null ? 'undefined' : ans.toString();
        },

        // Just like eval, but we reuse the old environment
        'repl': function(string) {
            var ans =
                pipeline.eval(
                    pipeline.desugar(
                        pipeline.parse(
                            pipeline.read(
                                pipeline.scan(string))), true));
            return ans == null ? 'undefined' : ans.toString();
        },

        'evalUrl': function(url) {
            var req = new XMLHttpRequest();
            req.open('GET', url);
            req.onreadystatechange = function() {
                if (req.readyState === 4 && req.status === 200)
                    publicApi['eval'](req.responseText);
            };
            req.send();
        },

        'getMetadata': function() {
            return {'banner': banner};
        }
    };

    return publicApi;
})();