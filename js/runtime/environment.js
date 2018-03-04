goog.module('r5js.Environment');

const {Continuation} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/continuation');
const {Datum, UNSPECIFIED_VALUE} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {Lambda} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/lambda');
const {Macro} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/macro');
const {Procedure} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/procedure');
const {Ref} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/ref');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const {CPS_PREFIX} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/rename_util');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {notAProcedure} = require('/js/runtime/shim_collect_es6_sources.es6/node_modules/__main__/js/runtime/errors');

/** @implements {IEnvironment} */
class Environment {
    /** @param {?IEnvironment} enclosingEnv The enclosing environment, if any. */
    constructor(enclosingEnv) {
        /** @const @private */ this.enclosingEnv_ = enclosingEnv;
        /** @const @private {!Object<string,!Value>} */ this.bindings_ = {};
        /** @private {!Object<string, !UserDefinedProcedure>} */ this.closures_ = {};
        /** @private */ this.redefsOk_ = false;
        /** @private */ this.sealed_ = false;
    }

    /** @override */
    seal() {
        this.sealed_ = true;
    }

    /** @override */
    allowRedefs() {
        this.redefsOk_ = true;
        return this;
    }

    /** @return {!Environment} A clone of this environment. */
    clone() {
        if (this.enclosingEnv_) {
            throw Error.internalInterpreterError(
                'clone should only be used during ' +
                'interpreter bootstrapping');
        }

        const cloned = new Environment(null /* enclosingEnv */);

        for (let name_ in this.bindings_) {
            const val = this.bindings_[name_];
            cloned.bindings_[name_] = val instanceof Macro
                ? val.clone(cloned)
                : val;
        }

        return cloned;
    }

    /** @override */
    hasBindingRecursive(name) {
        return name in this.bindings_ ||
            (!!this.enclosingEnv_ && this.enclosingEnv_.hasBindingRecursive(name));
    }

    /** @override */
    get(name) {
        if (name in this.bindings_) {
            const binding = this.bindings_[name];
            if (binding instanceof Environment
                && binding.hasBindingRecursive(name)) {
                // Redirects for free ids in macro transcriptions
                return binding.get(name);
            } else if (binding instanceof Macro) {
                return binding;
            } else if (binding instanceof Continuation
                || binding instanceof Procedure) {
                /* We store primitive and non-primitive procedures unwrapped,
                 but wrap them in a Datum if they are requested through get.
                 (getProcedure, which is intended just for evaluating the operator
                 on the trampoline, will return the unwrapped procedures.) */
                return new Lambda(name, /** @type {!Procedure} */ (binding));
            } else if (binding === UNSPECIFIED_VALUE) {
                return binding;
            } else if (binding instanceof Datum) {
                return new Ref(binding);
            } else {
                return binding;
            }
        } else if (name in this.closures_) {
            /* I think this is only used for ProcCall.prototype.cpsify, where
             identifiers are used to keep track of things while the structure
             is changed. Semantic use of procedures should be gated by
             Environment.prototype.getProcedure, and since that doesn't check
             the closures map, there should be no danger of accidentally
             returning a closure. */
            return this.closures_[name];
        } else if (this.enclosingEnv_) {
            // If the current environment has no binding for the name, look one level up
            return this.enclosingEnv_.get(name);
        } else {
            throw Error.unboundVariable(name);
        }
    }

    /** @override */
    getProcedure(name) {
        if (name in this.bindings_) {
            const binding = this.bindings_[name];
            if (binding instanceof Environment) {
                return (/** @type {!Environment} */ (binding)).getProcedure(name);
            } else if (binding instanceof Continuation
                || binding instanceof Macro
                || binding instanceof Procedure) {
                return binding;
            } else if (binding instanceof Datum) {
                throw notAProcedure(name);
            } else {
                throw Error.internalInterpreterError(
                    "getProcedure: don't know what to do with binding " + name);
            }

        } else if (this.enclosingEnv_) {
            return this.enclosingEnv_.getProcedure(name);
        } else {
            return null;
        }
    }

    /** @override */
    addClosuresFrom(other) {
        const otherEnv = /** @type {!Environment} */ (other);
        /* todo bl: we have to clone the SchemeProcedures to prevent
         some kind of infinite loop. I'm not entirely clear about what loop, though,
         since SchemeProcedure.prototype.cloneWithEnv itself does not do a lot
         of copying. */
        for (const name in otherEnv.closures_) {
            this.addBinding(name, otherEnv.closures_[name].cloneWithEnv(this));
        }
        return this;
    }

    /** @override */
    addClosure(name, proc) {
        if (this.sealed_) {
            throw Error.internalInterpreterError('tried to bind ' +
            name +
            ' in sealed environment');
        } else if (this.closures_[name]) {
            throw Error.internalInterpreterError('invariant incorrect');
        } else {
            this.closures_[name] = proc;
        }
    }

    /**
     * @param {string} name
     * @return {boolean}
     * @private
     */
    bindingIsAcceptable_(name) {
        return !(name in this.bindings_)
            || this.redefsOk_
            || name.charAt(0) === CPS_PREFIX;
    }

    /** @override */
    addBinding(name, val) {
        if (this.sealed_) {
            throw Error.internalInterpreterError(
                'tried to bind ' +
                name +
                ' in sealed environment ' +
                this);
        }

        if (!this.bindingIsAcceptable_(name)) {
            throw Error.internalInterpreterError(
                'redefining ' +
                name +
                ' in same env, not allowed');
        }

        /* Macros require a backlink to the environment they were defined in
         to resolve literal identifiers.
         todo bl: is there a better place to put this? */
        if (val instanceof Macro) {
            val.setDefinitionEnv(this);
        }

        this.bindings_[name] = val instanceof Datum ? val.unwrap() : val;
    }

    /** @override */
    mutate(name, newVal, isTopLevel) {
        const maybeBinding = this.bindings_[name];
        if (maybeBinding != null || isTopLevel) {
            if (maybeBinding instanceof Environment) {
                (/** @type {!Environment} */ (maybeBinding)).mutate(name, newVal, isTopLevel);
            } else {
                delete this.bindings_[name];
                this.addBinding(name, newVal);
            }
        } else if (this.enclosingEnv_) {
            this.enclosingEnv_.mutate(name, newVal, isTopLevel);
        } else {
            throw Error.unboundVariable(name);
        }
    }

    /** @override */
    setClosuresFrom(otherEnv) {
        this.closures_ = (/** @type {!Environment} */ (otherEnv)).closures_;
    }

    /** @override */
    child() {
        return new Environment(this);
    }

    /** @override */
    toString() {
        return '<environment-specifier>';
    }
}

exports = Environment;
