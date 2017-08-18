goog.module('r5js.VarargsUserDefinedProcedure');

const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const IEnvironment = goog.require('r5js.IEnvironment');
const List = goog.require('r5js.ast.List');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const datumutil = goog.require('r5js.datumutil');

class VarargsUserDefinedProcedure extends UserDefinedProcedure {
    /**
     * @param {!Array<string>} formalsArray The procedure's formal parameters, in order.
     * @param {Datum} bodyStart
     * @param {!IEnvironment} env
     * @param {string=} opt_name The procedure's name, for pretty-printing and
     * error messages. If not given, one will be created.
     */
    constructor(formalsArray, bodyStart, env, opt_name) {
        super(formalsArray, bodyStart, env, opt_name);
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
                siblingBuffer.appendSibling(datumutil.wrapValue(args[j]));
            }
            env.addBinding(name, siblingBuffer.toList(List));
        }
    }
}

exports = VarargsUserDefinedProcedure;
