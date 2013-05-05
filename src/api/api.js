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

goog.require('r5js.PublicApi');
goog.require('r5js.test.parser');
goog.require('r5js.test.scanner');



var GayLisp = (function () {

    /* The build process inserts src/js/* here (after the first opening brace
     in this file).. It also inserts src/scm/* here, embedded as JavaScript string
     literals and appropriately escaped. See the Makefile for details. */

    var pipeline = new LazyBoot(new Pipeline(),
        /**
         * @suppress {undefinedVars} For syntax and procedures
         * TODO bl: remove @suppress when we have a better build procedure.
         */
        function() {
            bootstrap(syntax, procedures);
            pipeline.setRootEnv(r5RSEnv);
        }
    );
    return new r5js.PublicApi(pipeline);
})();