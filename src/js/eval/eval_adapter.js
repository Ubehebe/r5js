goog.provide('r5js.EvalAdapter');



/**
 * An {@link r5js.Evaluator} maps input strings to Scheme values.
 * But to interact with the outside world, Scheme values need to be converted
 * to a suitable external representation. This convenience class does both
 * the evaluation and the conversion, saving clients from having to convert
 * results all the time.
 * @param {!r5js.Evaluator} evaluator Evaluator to use.
 * @param {function(!r5js.runtime.Value):T} adapter Function to use to convert
 * Scheme values to a form suitable for use in the target environment.
 * @struct
 * @constructor
 * @template T
 */
r5js.EvalAdapter = function(evaluator, adapter) {
  /** @const @private */ this.evaluator_ = evaluator;
  /** @const @private */ this.adapter_ = adapter;
};


/**
 * @param {string} input
 * @return {T}
 */
r5js.EvalAdapter.prototype.evaluate = function(input) {
  return this.adapter_(this.evaluator_.evaluate(input));
};


/**
 * Maps Scheme values to idiomatic JavaScript values:
 *
 * Scheme strings -> JS strings
 * Scheme numbers -> JS numbers
 * Scheme booleans -> JS booleans
 * Scheme symbols -> JS strings
 * Scheme characters -> JS strings
 * Scheme proper lists -> JS arrays
 * Scheme vectors -> JS arrays
 *
 * This is just intended as a convenience when using the Scheme interpreter
 * from its JavaScript API. The mapping is somewhat arbitrary;
 * the two languages' type systems don't fit exactly. It is also noninjective,
 * so it won't work in the JS -> Scheme direction.
 *
 * @param {!r5js.runtime.Value} value
 * @return {boolean|number|string|!Array|undefined}
 */
r5js.EvalAdapter.toJsValue = function(value) {
  switch (typeof value) {
    case 'number':
    case 'boolean':
    case 'string':
      return value;
    case 'object':
      if (value instanceof r5js.Ref) {
        return r5js.EvalAdapter.toJsValue(value.deref());
      } else if (value instanceof r5js.ast.List ||
          value instanceof r5js.ast.Vector) {
        return value.mapChildren(r5js.EvalAdapter.toJsValue);
      } else if (value instanceof r5js.ast.String ||
          value instanceof r5js.ast.Character) {
        return value.getPayload();
      } else if (value instanceof r5js.Datum) {
        return value.unwrap();
      }
    default:
      return undefined;
  }
};


/**
 * @param {!r5js.runtime.Value} value
 * @return {string}
 */
r5js.EvalAdapter.toDisplayString = function(value) {
  switch (typeof value) {
    case 'number':
      return value + '';
    case 'boolean':
      return value ? '#t' : '#f';
    case 'string':
      return value;
    case 'object':
      if (value instanceof r5js.Ref) {
        return r5js.EvalAdapter.toDisplayString(value.deref());
      } else if (value instanceof r5js.ast.List) {
        var childStrings = value.mapChildren(
            r5js.EvalAdapter.toDisplayString).join(' ');
        return '(' + childStrings + ')';
      } else if (value instanceof r5js.ast.Vector) {
        var childStrings = value.mapChildren(
            r5js.EvalAdapter.toDisplayString).join(' ');
        return '#(' + childStrings;
      } else if (value instanceof r5js.ast.String) {
        return value.getPayload();
      } else if (value instanceof r5js.ast.Character) {
        return value.getPayload();
      } else if (value instanceof r5js.Datum) {
        return r5js.EvalAdapter.toDisplayString(
            value.unwrap());
      }
    default:
      return '';
  }
};


/**
 * @param {!r5js.runtime.Value} value
 * @return {string}
 */
r5js.EvalAdapter.toWriteString = function(value) {
  switch (typeof value) {
    case 'number':
      return value + '';
    case 'boolean':
      return value ? '#t' : '#f';
    case 'string':
      return value;
    case 'object':
      if (value instanceof r5js.Ref) {
        return r5js.EvalAdapter.toWriteString(value.deref());
      } else if (value instanceof r5js.ast.List) {
        var childStrings = value.mapChildren(
            r5js.EvalAdapter.toWriteString).join(' ');
        return '(' + childStrings + ')';
      } else if (value instanceof r5js.ast.Vector) {
        var childStrings = value.mapChildren(
            r5js.EvalAdapter.toWriteString).join(' ');
        return '#(' + childStrings;
      } else if (value instanceof r5js.ast.String) {
        return '"' + value.getPayload() + '"'; // TODO bl escape
      } else if (value instanceof r5js.ast.Character) {
        return '#\\' + value.getPayload();
      } else if (value instanceof r5js.Datum) {
        return r5js.EvalAdapter.toWriteString(value.unwrap());
      }
    default:
      return '';
  }
};


