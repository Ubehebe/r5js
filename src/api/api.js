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


goog.provide('r5js.main');

goog.require('r5js.tmp.boot');
goog.require('r5js.tmp.branch');
goog.require('r5js.tmp.callback_backed_port');
goog.require('r5js.tmp.cdr_helper');
goog.require('r5js.tmp.continuable');
goog.require('r5js.tmp.continuable_helper');
goog.require('r5js.tmp.continuation');
goog.require('r5js.tmp.datum');
goog.require('r5js.tmp.ellipsis_transformer');
goog.require('r5js.tmp.env_buffer');
goog.require('r5js.tmp.environment');
goog.require('r5js.tmp.errors');
goog.require('r5js.tmp.ffi');
goog.require('r5js.tmp.globals');
goog.require('r5js.tmp.id_or_literal_transformer');
goog.require('r5js.tmp.js_obj_or_method');
goog.require('r5js.tmp.lazy_boot');
goog.require('r5js.tmp.list_like_transformer');
goog.require('r5js.tmp.node_backed_port');
goog.require('r5js.tmp.output_modes');
goog.require('r5js.tmp.parse');
goog.require('r5js.tmp.pipeline');
goog.require('r5js.tmp.port');
goog.require('r5js.tmp.proc_call');
goog.require('r5js.tmp.read');
goog.require('r5js.tmp.rename_helper');
goog.require('r5js.tmp.root_environment');
goog.require('r5js.tmp.scanner');
goog.require('r5js.tmp.scheme_macro');
goog.require('r5js.tmp.scheme_procedure');
goog.require('r5js.tmp.sibling_buffer');
goog.require('r5js.tmp.stdproc');
goog.require('r5js.tmp.template_bindings');
goog.require('r5js.tmp.timer');
goog.require('r5js.tmp.trampoline');
goog.require('r5js.tmp.trampoline_helper');
goog.require('r5js.tmp.transformer');
goog.require('r5js.tmp.unit_test');


var GayLisp = (function() {

    /* The build process inserts src/js/* here (after the first opening brace
     in this file).. It also inserts src/scm/* here, embedded as JavaScript string
     literals and appropriately escaped. See the Makefile for details. */

    /* The pipeline is not public because it inputs and outputs
        internal data structures. */


    var pipeline = new LazyBoot(new Pipeline(),
        /**
         * @suppress {undefinedVars} For syntax and procedures
         * TODO bl: remove @suppress when we have a better build procedure.
         */
        function() {
            bootstrap(syntax, procedures);
            pipeline.delegate.setRootEnv(r5RSEnv);
        }
    );

    /* This is the public API. It mainly runs the above non-public
     pipeline from string input to the relevant stop point.

     The names are quoted in order to prevent the Google Closure Compiler
     from renaming the public API methods. */
    var publicApi = {
        'test': function(unitTestUrl, sideEffectHandler) {
            testScanner();
            testParser();
            if (unitTestUrl)
                publicApi['evalUrl'](unitTestUrl, sideEffectHandler);
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

        /* Mainly intended for multiline input on a terminal:
         when the programmer presses enter, the terminal needs to know
         whether to send the line out for evaluation or wait for another
         line to complete the input. */
        'willParse': function(logicalLine) {
            try {
                publicApi['parse'](logicalLine);
                return true;
            } catch (x) {
                /* If parsing failed, we usually want to wait for another line
                 of input. There's one common exception: unquoted empty lists
                 () and nested versions of the same. If a programmer types ()
                 at the terminal and presses enter, she will be stuck forever:
                 nothing she later types in will make the line buffer parse, and
                 so the terminal will never send the line buffer off for
                 evaluation. As a heuristic, if the parse has not succeeded,
                 we return false unless the number of opening and closing parens
                 is the same. This might not be the right heuristic,
                 but I haven't found a counterexample yet. Note that it's
                 fine to type unquoted empty lists as their own lines as long
                 as they are not the first line: for example the following is
                 fine:

                 >> (define-syntax
                 >> foo
                 >> (syntax-rules
                 >> ()
                 >> ((foo f) 'hi)))
                 >> (foo ())

                 If we find more of these situations where parsing fails but
                 we should not wait for more input, it might be a better idea
                 to equip the programmer with a button or key to flush the
                 line buffer. */
                var lparens = logicalLine.match(/\(/g);
                var rparens = logicalLine.match(/\)/g);
                return lparens
                    && rparens
                    && lparens.length === rparens.length;
            }
        },

        'eval': function(string, sideEffectHandler) {
            var ans =
                pipeline.eval(
                    pipeline.desugar(
                        pipeline.parse(
                            pipeline.read(
                                pipeline.scan(string)))), sideEffectHandler);
            return ans ? ans.toString() : '';
        },

        // Just like eval, but we reuse the old environment
        'repl': function(string, sideEffectHandler) {
            var ans =
                pipeline.eval(
                    pipeline.desugar(
                        pipeline.parse(
                            pipeline.read(
                                pipeline.scan(string))), true), sideEffectHandler);
            return ans ? ans.toString() : '';
        },

        'evalUrl': function(url, sideEffectHandler) {
            var req = new XMLHttpRequest();
            req.open('GET', url);
            req.onreadystatechange = function() {
                if (req.readyState === 4 && req.status === 200)
                    publicApi['eval'](req.responseText, sideEffectHandler);
            };
            req.send();
        },

        'getMetadata':
            /**
             * @suppress {undefinedVars} For the banner.
             * TODO bl: remove @suppress when we have a better build process.
             */
            function() {
                return {'banner': banner};
            }
    };

    return publicApi;
})();