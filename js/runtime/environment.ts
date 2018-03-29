import {UserDefinedProcedure} from "./user_defined_procedure";
import {Error} from "../error";
import {Macro} from "../macro/macro";
import {Procedure} from "./procedure";
import {Continuation} from "./continuation";
import {Lambda} from "./lambda";
import {Datum, UNSPECIFIED_VALUE} from "../ast/datum";
import {Ref} from "../ast/ref";
import {notAProcedure} from "./errors";
import {CPS_PREFIX} from "../parse/rename_util";

export class Environment implements IEnvironment {

  private readonly bindings: { [key: string]: Value } = {};
  private closures: { [key: string]: UserDefinedProcedure } = {};
  private redefsOk: boolean = false;
  private sealed: boolean = false;

  constructor(private readonly enclosingEnv: IEnvironment | null) {}

  /** @override */
  seal() {
    this.sealed = true;
  }

  /** @override */
  allowRedefs() {
    this.redefsOk = true;
    return this;
  }

  clone(): Environment {
    if (this.enclosingEnv) {
      throw Error.internalInterpreterError(
          'clone should only be used during ' +
          'interpreter bootstrapping');
    }

    const cloned = new Environment(null /* enclosingEnv */);

    for (let name_ in this.bindings) {
      const val = this.bindings[name_];
      cloned.bindings[name_] = val instanceof Macro
          ? (val as Macro).clone(cloned)
          : val;
    }

    return cloned;
  }

  /** @override */
  hasBindingRecursive(name) {
    return name in this.bindings ||
        (!!this.enclosingEnv && this.enclosingEnv.hasBindingRecursive(name));
  }

  /** @override */
  get(name) {
    if (name in this.bindings) {
      const binding = this.bindings[name];
      if (binding instanceof Environment && binding.hasBindingRecursive(name)) {
        // Redirects for free ids in macro transcriptions
        return binding.get(name);
      } else if (binding instanceof Macro) {
        return binding;
      } else if (binding instanceof Continuation || binding instanceof Procedure) {
        // We store primitive and non-primitive procedures unwrapped, but wrap them in a Datum
        // if they are requested through get. (getProcedure, which is intended just for evaluating
        // the operator on the trampoline, will return the unwrapped procedures.)
        return new Lambda(name, binding);
      } else if (binding === UNSPECIFIED_VALUE) {
        return binding;
      } else if (binding instanceof Datum) {
        return new Ref(binding);
      } else {
        return binding;
      }
    } else if (name in this.closures) {
      // I think this is only used for ProcCall.prototype.cpsify, where identifiers are used to keep
      // track of things while the structure is changed. Semantic use of procedures should be gated
      // by Environment.prototype.getProcedure, and since that doesn't check the closures map,
      // there should be no danger of accidentally returning a closure.
      return this.closures[name];
    } else if (this.enclosingEnv) {
      // If the current environment has no binding for the name, look one level up
      return this.enclosingEnv.get(name);
    } else {
      throw Error.unboundVariable(name);
    }
  }

  /** @override */
  getProcedure(name: string) {
    if (name in this.bindings) {
      const binding = this.bindings[name];
      if (binding instanceof Environment) {
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

    } else if (this.enclosingEnv) {
      return this.enclosingEnv.getProcedure(name);
    } else {
      return null;
    }
  }

  /** @override */
  addClosuresFrom(other: Environment) {
    // todo bl: we have to clone the SchemeProcedures to prevent some kind of infinite loop.
    // I'm not entirely clear about what loop, though, since SchemeProcedure.prototype.cloneWithEnv
    // itself does not do a lot of copying.
    for (const name in other.closures) {
      this.addBinding(name, other.closures[name].cloneWithEnv(this));
    }
    return this;
  }

  /** @override */
  addClosure(name, proc) {
    if (this.sealed) {
      throw Error.internalInterpreterError(`tried to bind ${name} in sealed environment`);
    } else if (this.closures[name]) {
      throw Error.internalInterpreterError('invariant incorrect');
    } else {
      this.closures[name] = proc;
    }
  }

  private bindingIsAcceptable_(name: string): boolean {
    return !(name in this.bindings)
        || this.redefsOk
        || name.charAt(0) === CPS_PREFIX;
  }

  /** @override */
  addBinding(name: string, val: Value) {
    if (this.sealed) {
      throw Error.internalInterpreterError(
          `tried to bind ${name} in sealed environment ${this}`);
    }

    if (!this.bindingIsAcceptable_(name)) {
      throw Error.internalInterpreterError(`redefining ${name} in same env, not allowed`);
    }

    // Macros require a backlink to the environment they were defined in to resolve literal
    // identifiers. todo bl: is there a better place to put this?
    if (val instanceof Macro) {
      (val as Macro).setDefinitionEnv(this);
    }

    this.bindings[name] = val instanceof Datum ? val.unwrap() : val;
  }

  /** @override */
  mutate(name: string, newVal: Value, isTopLevel: boolean) {
    const maybeBinding = this.bindings[name];
    if (maybeBinding != null || isTopLevel) {
      if (maybeBinding instanceof Environment) {
        maybeBinding.mutate(name, newVal, isTopLevel);
      } else {
        delete this.bindings[name];
        this.addBinding(name, newVal);
      }
    } else if (this.enclosingEnv) {
      this.enclosingEnv.mutate(name, newVal, isTopLevel);
    } else {
      throw Error.unboundVariable(name);
    }
  }

  /** @override */
  setClosuresFrom(otherEnv: Environment) {
    this.closures = otherEnv.closures;
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