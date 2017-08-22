goog.module('r5js.Environment');

const Continuation = goog.require('r5js.Continuation');
const Datum = goog.require('r5js.Datum');
const Error = goog.require('r5js.Error');
const IEnvironment = goog.require('r5js.IEnvironment');
const Lambda = goog.require('r5js.Lambda');
const Macro = goog.require('r5js.Macro');
const Procedure = goog.require('r5js.Procedure');
const Ref = goog.require('r5js.Ref');
const RenameUtil = goog.require('r5js.RenameUtil');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const {Value} = goog.require('r5js.Value');
const {notAProcedure} = goog.require('r5js.runtime.errors');

/** @implements {IEnvironment} */
class Environment {
    /** @param {IEnvironment} enclosingEnv The enclosing environment, if any. */
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

    /** @return {!Environment} This object, for chaining. */
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
            if (IEnvironment.isImplementedBy(binding)
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
            /* I think this is only used for ProcCall.prototype.cpsify_, where
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
            if (IEnvironment.isImplementedBy(binding)) {
                return binding.getProcedure(name);
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

    /**
     * @param {!IEnvironment} other Environment to add closures from.
     * @return {!IEnvironment} This environment, for chaining.
     * @see {r5js.IEnvironment#addClosure}.
     */
    addClosuresFrom(other) {
        /* todo bl: we have to clone the SchemeProcedures to prevent
         some kind of infinite loop. I'm not entirely clear about what loop, though,
         since SchemeProcedure.prototype.cloneWithEnv itself does not do a lot
         of copying. */
        for (const name in other.closures_) {
            this.addBinding(name, other.closures_[name].cloneWithEnv(this));
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
            || name.charAt(0) === RenameUtil.CPS_PREFIX;
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
            if (IEnvironment.isImplementedBy(maybeBinding)) {
                maybeBinding.mutate(name, newVal, isTopLevel);
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

    /**
     * @param {!Environment} otherEnv Environment whose closures
     * this environment should use.
     * TODO bl: this method is only used once. Can I eliminate it?
     */
    setClosuresFrom(otherEnv) {
        this.closures_ = otherEnv.closures_;
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

IEnvironment.addImplementation(Environment);
exports = Environment;