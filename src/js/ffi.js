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


goog.provide('r5js.ffi');
goog.provide('r5js.ffiutil');


goog.require('r5js.data');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.FFIError');
goog.require('r5js.JsObjOrMethod');
goog.require('r5js.ProcCall');


/**
 * @param {!r5js.JsObjOrMethod} jsObjOrMethod
 * @param {!r5js.Continuation} continuation A continuation.
 * @param {!r5js.TrampolineHelper} resultStruct
 * @param {function(!r5js.Datum):!r5js.Parser} parserProvider Function
 * that will return a new Parser for the given Datum.
 */
r5js.ProcCall.prototype.tryFFI = function(
    jsObjOrMethod, continuation, resultStruct, parserProvider) {
    if (!this.operandsInCpsStyle()) {
        this.cpsify(continuation, resultStruct, parserProvider);
    } else {

        var property;
        if (jsObjOrMethod.isBoundMethod()) {
            property = jsObjOrMethod.callWith(this.evalArgs(false));
        } else if (this.firstOperand
            && !this.firstOperand.nextSibling
            && this.firstOperand.isQuote()
            && this.firstOperand.firstChild.isIdentifier()) {
            property = jsObjOrMethod.getObject()[this.firstOperand.firstChild.payload];
        } else throw new r5js.FFIError();

        var ans;

        switch (typeof property) {
            case 'function':
                ans = r5js.ffiutil.newFFIDatum(
                    new r5js.JsObjOrMethod(
                        jsObjOrMethod.getObject(),
                        property
                    )
                );
                break;
            case 'object':
                ans = r5js.ffiutil.newFFIDatum(new r5js.JsObjOrMethod(property));
                break;
            case 'undefined':
                ans = null;
                break;
            case 'string':
                ans = r5js.data.maybeWrapResult(property, r5js.DatumType.STRING);
                break;
            case 'number':
                ans = r5js.data.maybeWrapResult(property, r5js.DatumType.NUMBER);
                break;
            case 'boolean':
                ans = r5js.data.maybeWrapResult(property, r5js.DatumType.BOOLEAN);
                break;
        }

        this.bindResult(continuation, ans);
        resultStruct.ans = ans;
        resultStruct.nextContinuable = continuation.nextContinuable;
    }
};


/**
 * Convenience functions for working with the foreign-function interface.
 */
r5js.ffiutil = {};


/**
 * @param {!Object} jsObj A JavaScript object.
 * @return {!r5js.Datum} A new datum representing the given JavaScript object.
 */
r5js.ffiutil.newFFIDatum = function(jsObj) {
    var ans = new r5js.Datum();
    ans.type = r5js.DatumType.FFI;
    ans.payload = jsObj;
    return ans;
};


/**
 * Warning: experimental. The intent is to allows things like
 * (js-set! ((((window 'document) 'querySelector) "body") 'style) 'background-color "red")
 * ((window 'alert) "Hello, world!")
 * @type {!Object}
 */
r5js.ffi = {
    'js?': {
        argc: 1,
        proc: function(obj) {
            return obj instanceof r5js.JsObjOrMethod;
        }
    },

    'js-set!': {
        argc: 3,
        argtypes: ['js', 'symbol'],
        proc: function(jsObjOrMethod, property, value) {
            if (jsObjOrMethod.isBoundMethod())
                throw new r5js.FFIError();
            jsObjOrMethod.getObject()[property] = value;
            return null; // I like Scheme's assignment semantics better than JavaScript's
        }
    }
};



