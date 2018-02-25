goog.module('r5js.VarargsUserDefinedProcedure');

const {Datum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {SiblingBuffer} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/sibling_buffer');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const {wrapValue} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum_util');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {List} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/list');

class VarargsUserDefinedProcedure extends UserDefinedProcedure {
    /**
     * @param {!Array<string>} formalsArray The procedure's formal parameters, in order.
     * @param {?Datum} bodyStart
     * @param {!IEnvironment} env
     * @param {string=} name The procedure's name, for pretty-printing and
     * error messages. If not given, one will be created.
     */
    constructor(formalsArray, bodyStart, env, name=undefined) {
        super(formalsArray, bodyStart, env, name);
    }

    /** @override */
    checkNumArgs(numActuals) {
        const minNumArgs = this.formalsArray.length - 1;
        if (numActuals < minNumArgs) {
            throw Error.tooFewVarargs(this.toString(), minNumArgs, numActuals);
        }
    }

    /** @override */
    bindArgs(args, env) {
        let name, i;

        for (i = 0; i < this.formalsArray.length - 1; ++i) {
            name = this.formalsArray[i];
            env.addBinding(name, args[i]);
        }

        if (this.formalsArray.length > 0) {
            name = this.formalsArray[i];
            // Roll up the remaining arguments into a list
            let siblingBuffer = new SiblingBuffer();
            for (let j = i; j < args.length; ++j) {
                siblingBuffer.appendSibling(wrapValue(args[j]));
            }
            env.addBinding(name, siblingBuffer.toList(List));
        }
    }
}

exports = VarargsUserDefinedProcedure;
