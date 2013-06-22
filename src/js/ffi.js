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


goog.provide('r5js.tmp.ffi');

goog.require('r5js.Datum');
goog.require('r5js.ProcCall');

/* Warning: experimental. The intent is to allows things like
 (js-set! ((((window 'document) 'querySelector) "body") 'style) 'background-color "red")
 ((window 'alert) "Hello, world!")
 */

/**
 * @constructor
 */
function FFIError() {
    this.toString = function() {
        return 'JS interop is an experimental feature. Sorry!';
    };
}

/**
 * @param {!Object} jsObj A JavaScript object.
 * @return {!r5js.Datum} A new datum representing the given JavaScript object.
 */
function newFFIDatum(jsObj) {
    var ans = new r5js.Datum();
    ans.type = 'ffi';
    ans.payload = jsObj;
    return ans;
}

/**
 * TODO bl: Why is this method not in proc_call.js?
 * @param {?} jsObjOrMethod
 * @param {!Continuation} continuation A continuation.
 * @param {?} resultStruct
 */
r5js.ProcCall.prototype.tryFFI = function(jsObjOrMethod, continuation, resultStruct) {
    if (!this.operandsInCpsStyle()) {
        this.cpsify(jsObjOrMethod, continuation, resultStruct);
    } else {

        var property;
        if (jsObjOrMethod.isBoundMethod()) {
            property = jsObjOrMethod.callWith(this.evalArgs(false));
        } else if (this.firstOperand
            && !this.firstOperand.nextSibling
            && this.firstOperand.isQuote()
            && this.firstOperand.firstChild.isIdentifier()) {
            property = jsObjOrMethod.getObject()[this.firstOperand.firstChild.payload];
        } else throw new FFIError();

        var ans;

        switch (typeof property) {
            case 'function':
                ans = newFFIDatum(new JsObjOrMethod(jsObjOrMethod.getObject(), property));
                break;
            case 'object':
                ans = newFFIDatum(new JsObjOrMethod(property));
                break;
            case 'undefined':
                ans = null;
                break;
            default:
                ans = maybeWrapResult(property, typeof property);
                break;
        }

        this.bindResult(continuation, ans);
        resultStruct.ans = ans;
        resultStruct.nextContinuable = continuation.nextContinuable;
    }
};

var FFI = {
    'js?': {
        argc: 1,
        proc: function(obj) {
            return obj instanceof JsObjOrMethod;
        }
    },

    'js-set!': {
        argc: 3,
        argtypes: ['js', 'symbol'],
        proc: function(jsObjOrMethod, property, value) {
            if (jsObjOrMethod.isBoundMethod())
                throw new FFIError();
            jsObjOrMethod.getObject()[property] = value;
            return null; // I like Scheme's assignment semantics better than JavaScript's
        }
    }


};



