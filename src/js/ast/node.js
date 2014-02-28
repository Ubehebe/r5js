goog.provide('r5js.ast.EnvironmentSpecifier');
goog.provide('r5js.ast.Node');



/**
 * @interface
 * @template T
 */
r5js.ast.Node = function() {};


/** @return {T} */
r5js.ast.Node.prototype.getPayload = function() {};


/** @return {!r5js.DatumType} */
r5js.ast.Node.prototype.getType = function() {};


/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim. Remove.
 */
r5js.ast.Node.isImplementedBy = function(obj) {
  return obj instanceof r5js.ast.EnvironmentSpecifier;
};



/**
 * @param {!r5js.DatumType} type
 * @param {T} payload
 * @struct
 * @constructor
 * @template T
 * @private
 */
r5js.ast.BaseNode_ = function(type, payload) {
  /** @const @private {!r5js.DatumType} */
  this.type_ = type;

  /** @const @private {T} */
  this.payload_ = payload;
};


/** @return {!r5js.DatumType} */
r5js.ast.BaseNode_.prototype.getType = function() {
  return this.type_;
};


/** @return {T} */
r5js.ast.BaseNode_.prototype.getPayload = function() {
  return this.payload_;
};



/**
 * @param {!r5js.IEnvironment} baseEnv
 * @implements {r5js.ast.Node.<!r5js.IEnvironment>}
 * @extends {r5js.ast.BaseNode_}
 * @struct
 * @constructor
 */
r5js.ast.EnvironmentSpecifier = function(baseEnv) {
  goog.base(this,
      r5js.DatumType.ENVIRONMENT_SPECIFIER,
      new r5js.Environment('', baseEnv));
};
goog.inherits(r5js.ast.EnvironmentSpecifier, r5js.ast.BaseNode_);
