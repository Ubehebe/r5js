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


goog.provide('r5js.PublicApi');


goog.require('r5js.OutputMode');
goog.require('r5js.ScannerImpl');
goog.require('r5js.util.Logger');

/**
 *
 * @param {!r5js.IPipeline} pipeline A pipeline object.
 * @constructor
 */
r5js.PublicApi = function(pipeline) {

    /**
     * @type {!r5js.IPipeline}
     * @private
     */
    this.pipeline_ = pipeline;
};

/**
 * @param {?string} unitTestUrl The URL for the unit tests.
 * @param {function()} sideEffectHandler A side effect handler.
 * @param {r5js.util.Logger} logger Logger, for test output.
 * The logger is nullable because this method is called from node_exports.js,
 * which lives outside the JavaScript root managed by the Closure build system
 * and cannot conveniently instantiate a logger.
 * TODO bl: tighten the type of sideEffectHandler.
 */
r5js.PublicApi.prototype.test = function(
    unitTestUrl, sideEffectHandler, logger) {
    logger = logger || r5js.util.Logger.getLogger('r5js');
    if (unitTestUrl) {
        this.evalUrl(unitTestUrl, sideEffectHandler, logger);
    }
};


/**
 * @param {string} string The string to read.
 */
r5js.PublicApi.prototype.read = function(string) {
    return this.pipeline_.read(
        this.pipeline_.scan(string)
    );
};

r5js.PublicApi.prototype.parse = function(string) {
    return this.pipeline_.parse(
        this.pipeline_.read(
            this.pipeline_.scan(string)
        ),
        null
    );
};


/**
 * Mainly intended for multiline input on a terminal:
 * when the programmer presses enter, the terminal needs to know
 * whether to send the line out for evaluation or wait for another
 * line to complete the input.
 * @param {string} logicalLine The logical line.
 * @return {boolean} True iff the logical line has a parse.
 */
r5js.PublicApi.prototype.willParse = function(logicalLine) {
    try {
        this.parse(logicalLine);
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
        return /** @type {boolean}*/ (
            lparens
                && rparens
                && lparens.length === rparens.length
            );
    }
};


/**
 * @param {string} string The source text to evaluate.
 * @param {function()} sideEffectHandler A side effect handler.
 * @param {!r5js.util.Logger} logger Logger.
 * @return {string} A string representation of the value of the evaluation.
 * TODO bl: narrow the type of sideEffectHandler.
 */
r5js.PublicApi.prototype.Eval = function(string, sideEffectHandler, logger) {
    var ans =
        this.pipeline_.Eval(
            this.pipeline_.desugar(
                this.pipeline_.parse(
                    this.pipeline_.read(
                        this.pipeline_.scan(string)
                    ),
                    null
                ),
                false
            ),
            sideEffectHandler,
            logger
        );
    return ans instanceof r5js.Datum ?
        (/** @type {!r5js.Datum} */ (ans)).stringForOutputMode(
            r5js.OutputMode.DISPLAY) :
        (ans ? ans.toString() : '');
};

/**
 * Just like {@link r5js.PublicApi.eval}, but reuses the old environment.
 * @param {string} string The source text to evaluate.
 * @param {*} sideEffectHandler A side effect handler.
 * @param {!r5js.util.Logger} logger Logger.
 * TODO bl: tighten the type of sideEffectHandler.
 */
r5js.PublicApi.prototype.repl = function (string, sideEffectHandler, logger) {
    var ans =
        this.pipeline_.Eval(
            this.pipeline_.desugar(
                this.pipeline_.parse(
                    this.pipeline_.read(
                        this.pipeline_.scan(string)
                    ),
                    null
                ),
                true
            ),
            goog.nullFunction,
            logger
        );
    return ans instanceof r5js.Datum ?
        (/** @type {!r5js.Datum} */ (ans)).stringForOutputMode(
            r5js.OutputMode.DISPLAY) :
        (ans ? ans.toString() : '');
};


/**
 * @param {string} url URL to open.
 * @param {function()} sideEffectHandler A side effect handler.
 * @param {!r5js.util.Logger} logger Logger.
 * TODO bl: narrow the type of sideEffectHandler.
 */
r5js.PublicApi.prototype.evalUrl = function(url, sideEffectHandler, logger) {
    var req = new XMLHttpRequest();
    req.open('GET', url);
    var self = this;
    req.onreadystatechange = function() {
      if (req.readyState === 4 && req.status === 200) {
          self.Eval(req.responseText, sideEffectHandler, logger);
      }
    };
    req.send();
};


/**
 * @return {{banner: string}}
 */
r5js.PublicApi.prototype.getMetadata = function() {
    return {
        'banner': '[GAY LISP BANNER HERE]'
    };
};