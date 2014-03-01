goog.provide('r5js.ast.EnvironmentSpecifier');
goog.provide('r5js.ast.InputPort');
goog.provide('r5js.ast.Node');
goog.provide('r5js.ast.OutputPort');



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
  return obj instanceof r5js.ast.EnvironmentSpecifier ||
      obj instanceof r5js.ast.InputPort ||
      obj instanceof r5js.ast.OutputPort;
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
 * @param {!r5js.Port} port
 * @implements {r5js.ast.Node.<!r5js.Port>}
 * @extends {r5js.ast.BaseNode_}
 * @struct
 * @constructor
 */
r5js.ast.InputPort = function(port) {
  goog.base(this, port);
};
goog.inherits(r5js.ast.InputPort, r5js.ast.BaseNode_);



/**
 * @param {!r5js.Port} port
 * @implements {r5js.ast.Node.<!r5js.Port>}
 * @extends {r5js.ast.BaseNode_}
 * @struct
 * @constructor
 */
r5js.ast.OutputPort = function(port) {
  goog.base(this, port);
};
goog.inherits(r5js.ast.OutputPort, r5js.ast.BaseNode_);

