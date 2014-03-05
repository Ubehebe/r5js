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


goog.provide('r5js.PrimitiveProcedures');


goog.require('r5js.ast.EnvironmentSpecifier');
goog.require('r5js.ast.Node');
goog.require('r5js.ast.InputPort');
goog.require('r5js.ast.OutputPort');
goog.require('r5js.ArgumentTypeError');
goog.require('r5js.CdrHelper');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.ImmutableError');
goog.require('r5js.IncorrectNumArgs');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.NodeBackedPort');
goog.require('r5js.OutputMode');
goog.require('r5js.ParseError');
goog.require('r5js.PrimitiveProcedureError');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.TooFewArgs');
goog.require('r5js.TooManyArgs');
goog.require('r5js.UnimplementedOptionError');
goog.require('r5js.data');
goog.require('r5js.runtime');
goog.require('r5js.procs');
goog.require('r5js.trampoline');
goog.require('r5js.util.Logger');


/**
 * The names of the different categories of builtins are just
 * for readability; they all get loaded into the same namespace.
 */
r5js.builtins = {};



r5js.builtins['vector'] = {

    /* todo bl: the current vector implementation uses Datums, in other
     words, linked lists! Replace this by something that's actually
     random access. */

    'make-vector': {
        argc: {min: 1, max: 2},
        proc: function(n, fill) {
            /* We want n to be a number (= unwrapped) and fill to be a
             Datum (= wrapped). Unfortunately, if we specify
             argtypes: ['number'] in order to get automatic type checking
             on the first argument, then all the arguments will be
             unwrapped; and if we omit argtypes, then none of the
             arguments will be unwrapped. So we manually unwrap the
             first argument and do the type checking ourselves.
             C'est la vie. */
            n = n.unwrap();
            if (typeof n !== 'number')
                throw new r5js.ArgumentTypeError(n, 0, 'make-vector', r5js.DatumType.NUMBER);
            /* R5RS 6.3.6: "If a second argument is given, then each
             element is initialized to fill. Otherwise the initial
             contents of each element is unspecified."

             False seems like a good default. */
            fill = fill || r5js.data.newIdOrLiteral(false, r5js.DatumType.BOOLEAN);
            var buf = [];
            for (var i = 0; i < n; ++i)
                buf.push(fill.clone());
            return newVectorDatum(buf);
        }
    },
    'vector-length': {
        argc: 1,
        argtypes: ['vector'],
        proc:function (v) {
            return v.isArrayBacked()
                ? v.getPayload().length
                : v.convertVectorToArrayBacked().getPayload().length;

        }
    },
    'vector-ref': {
        argc: 2,
        argtypes: ['vector', 'number'],
        proc: function(v, k) {
            return v.isArrayBacked()
                ? v.getPayload()[k]
                : v.convertVectorToArrayBacked().getPayload()[k];
        }
    },
    'vector-set!': {
        argc:3,
        proc:function (v, k, fill) {
            v = v.unwrap();
            k = k.unwrap();

            if (!v.isVector())
                throw new r5js.ArgumentTypeError(v, 0, 'vector-set!', r5js.DatumType.VECTOR);
            else if (typeof k !== 'number')
                throw new r5js.ArgumentTypeError(k, 1, 'vector-set!', r5js.DatumType.NUMBER);

            if (v.isImmutable())
                throw new r5js.ImmutableError(v.toString());

            if (v.isArrayBacked())
                v.getPayload()[k] = fill;
            else
                v.convertVectorToArrayBacked().getPayload()[k] = fill;

            // todo bl requires a cycle-labeling procedure like set-car! and set-cdr!

            return null;
        }
    }
};


r5js.builtins['eval'] = {
    'scheme-report-environment': {
        argc: 1,
        argtypes: ['number'],
        proc: function(num) {
            if (num === 5)
                return new r5js.ast.EnvironmentSpecifier(
                    /** @type {!r5js.IEnvironment} */(r5js.PrimitiveProcedures.r5RSEnv_));
            else throw new r5js.InternalInterpreterError(
                'unsupported scheme report environment ' + num);
        }
    },
    'null-environment': {
        argc: 1,
        argtypes: ['number'],
        proc: function(num) {
            if (num === 5)
                return new r5js.ast.EnvironmentSpecifier(
                    /** @type {!r5js.IEnvironment} */ (r5js.PrimitiveProcedures.nullEnv_));
            else throw new r5js.InternalInterpreterError(
                'unsupported null environment ' + num);
        }
    }
};


/** @private {r5js.IEnvironment} */
r5js.PrimitiveProcedures.nullEnv_;

/** @private {r5js.IEnvironment} */
r5js.PrimitiveProcedures.r5RSEnv_;


/**
 * @param {!r5js.IEnvironment} nullEnv The null environment, needed by
 *     the eval primitive procedure.
 * @param {!r5js.IEnvironment} r5RSEnv The R5RS environment, needed by
 *     the eval primitive procedure.
 * @param {!r5js.util.Logger} logger Logger.
 */
r5js.PrimitiveProcedures.install = function(nullEnv, r5RSEnv, logger) {
    r5js.PrimitiveProcedures.nullEnv_ = nullEnv;
    r5js.PrimitiveProcedures.r5RSEnv_ = r5RSEnv;

    r5js.runtime.install(r5RSEnv);

  for (var category in r5js.builtins) {
    var procs = r5js.builtins[category];
    for (var name in procs)
      r5js.PrimitiveProcedures.install_(name, procs[name], r5RSEnv, logger);
  }


  /* Experimental Scheme->JS FFI is browser-only for now.
     I used to have if (this.window === this), which is cleverer but
     doesn't work for strict mode. (Thanks, Stack Overflow!) */
  if (Function('return this;')().window) {
    r5RSEnv.addBinding(
        'window',
        r5js.ffiutil.newFFIDatum(new r5js.JsObjOrMethod(window)));
    for (var name in r5js.ffi) {
      r5js.PrimitiveProcedures.install_(name, r5js.ffi[name], r5RSEnv, logger);
    }
  }
};


/**
 * @param {string} name
 * @param {?} definition
 * @param {!r5js.IEnvironment} targetEnv Environment the builtin should be
 *        registered in.
 * @param {!r5js.util.Logger} logger Logger, for informational messages.
 * @return {?}
 * @private
 * TODO bl this function is too long. Split apart.
 */
r5js.PrimitiveProcedures.install_ = function(name, definition, targetEnv, logger) {

  var argc = definition.argc;
  var argtypes = definition.argtypes;
  var proc = definition.proc;

  if (!proc) {
    logger.warning('builtin ' + name + ' unspecified, skipping');
    return targetEnv;
  }

  if (targetEnv.hasBinding(name)) {
    logger.warning('redefining ' + name);
  }

  if (argtypes)
    requirePresenceOf(name, argtypes, targetEnv);

  var binding = function() {

    // Check correct number of arguments
    if (argc) {
      var numArgsFromUser = arguments.length;
      /* If a builtin procedure has special evaluation logic,
             the trampoline will pass it three additional arguments:
             the ProcCall, the Continuation, and the TrampolineResultStruct. */
      if (definition.hasSpecialEvalLogic)
        numArgsFromUser -= 3;
      else if (definition.needsCurrentPorts)
        numArgsFromUser -= 2;

      // If argc is a number, it means exactly that many args are required
      if (typeof argc === 'number' && numArgsFromUser !== argc)
        throw new r5js.IncorrectNumArgs(name, argc, numArgsFromUser);
      else if (argc.min && numArgsFromUser < argc.min)
        throw new r5js.TooFewArgs(name, argc.min, numArgsFromUser);
      else if (argc.max && numArgsFromUser > argc.max)
        throw new r5js.TooManyArgs(name, argc.max, numArgsFromUser);
    }

    var maybeUnwrappedArgs;

    /* If type checking was requested, do the type checking and
         also unwrap the arguments (so that a datum representing 1
         gets unwrapped to a JavaScript number 1). This makes sense
         because if you're writing a procedure that doesn't do any type
         checking, you should be prepared to handle arbitrary objects
         (i.e. Datum objects) in the procedure itself. */
    if (argtypes) {
      maybeUnwrappedArgs = [];

      /* If argtypes is something like 'number', that means every argument
             must be a number. */
      if (typeof argtypes === 'string') {
        var classifier = targetEnv.getProcedure(argtypes + '?');
        for (var i = 0; i < arguments.length; ++i) {
          /* todo bl this wrapping and unwrapping is getting
                     out of hand. */
          if (classifier(arguments[i]).unwrap())
            maybeUnwrappedArgs.push(arguments[i] instanceof r5js.Datum ? arguments[i].unwrap() : arguments[i]);
          else
            throw new r5js.ArgumentTypeError(
                arguments[i],
                i,
                name,
            /** @type {!r5js.DatumType} */ (argtypes)); // TODO probably wrong
        }
      }

    /* If argtypes is an array like ['string', 'number'], we should
             go down the arguments array and ensure each argument
             has its expected type. */
      else if (argtypes instanceof Array) {
        for (var i = 0; i < arguments.length; ++i) {
          /* argtypes might be shorter than arguments. In that
                     case we can't typecheck the extra arguments, but
                     we still need to collect them. */
          if (i < argtypes.length
                        && !targetEnv.getProcedure(argtypes[i] + '?')(arguments[i]).unwrap())
            throw new r5js.ArgumentTypeError(arguments[i], i, name, argtypes[i]);
          maybeUnwrappedArgs.push(arguments[i] instanceof r5js.Datum ? arguments[i].unwrap() : arguments[i]);
        }
      }
    }

  // If no type checking was requested, don't unwrap the args
    else maybeUnwrappedArgs = arguments;

    var returnValue = proc.apply(null, maybeUnwrappedArgs);
    return definition.hasSpecialEvalLogic
            ? null /* A function with special eval logic will set the trampolineResultStruct directly. */
            : r5js.data.maybeWrapResult(returnValue);
  };
  /* We are setting a boolean flag on a JavaScript function object.
     Not sure this is good style, but it saves us having to wrap
     the function in some other object to signal to the trampoline
     that it has special evaluation logic. */
  if (definition.hasSpecialEvalLogic)
    binding.hasSpecialEvalLogic = true;
  else if (definition.needsCurrentPorts)
    binding.needsCurrentPorts = true;
  targetEnv.addBinding(name, binding);
};

function requirePresenceOf(name, argtypes, targetEnv) {
  if (typeof argtypes === 'string' && !targetEnv.hasBinding(argtypes + '?'))
    throw new r5js.InternalInterpreterError('builtin procedure '
            + name
            + ' requires an argument to have type '
            + argtypes
            + ", but the default environment doesn't know about that type yet");
  else if (argtypes instanceof Array) {
    for (var i = 0; i < argtypes.length; ++i)
      if (typeof argtypes[i] === 'string' && !targetEnv.hasBinding(argtypes[i] + '?'))
        throw new r5js.InternalInterpreterError('builtin procedure '
                    + name
                    + ' requires an argument to have type '
                    + argtypes[i]
                    + ", but the default environment doesn't know about that type yet");
  }
}
