goog.provide('r5js.ast.EnvironmentSpecifier');
goog.provide('r5js.ast.Node');



/**
 * @interface
 * @template T
 */
r5js.ast.Node = function() {};


/** @return {T} */
r5js.ast.Node.prototype.getPayload = function() {};


/**
 * @param {*} obj
 * @return {boolean}
 * TODO bl temporary shim. Remove.
 */
r5js.ast.Node.isImplementedBy = function(obj) {
  return obj instanceof r5js.ast.EnvironmentSpecifier;
};



/**
 * @param {T} payload
 * @struct
 * @constructor
 * @template T
 * @private
 */
r5js.ast.BaseNode_ = function(payload) {
  /** @const @private {T} */
  this.payload_ = payload;
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
  goog.base(this, new r5js.Environment('', baseEnv));
};
goog.inherits(r5js.ast.EnvironmentSpecifier, r5js.ast.BaseNode_);


/**
 * TODO bl: this should do something different depending on
 * (scheme-report-environment 5) or (null-environment 5).
 * @override
 */
r5js.ast.EnvironmentSpecifier.prototype.toString = function() {
  return 'environment';
};

