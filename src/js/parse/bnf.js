goog.provide('r5js.bnf');



/** @interface */
r5js.bnf.Rule = function() {};


/** @return {boolean} */
r5js.bnf.Rule.prototype.hasRepetition = function() {};


/** @return {number} */
r5js.bnf.Rule.prototype.getRepetition = function() {};


/** @return {?r5js.parse.Nonterminal} */
r5js.bnf.Rule.prototype.getName = function() {};


/**
 * @return {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal}
 * TODO bl: temporary shim. remove.
 */
r5js.bnf.Rule.prototype.getType = function() {};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.bnf.Rule} This rule, for chaining.
 */
r5js.bnf.Rule.prototype.named = function(nonterminal) {};



/**
 * @param {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @param {number=} opt_repetition
 * @implements {r5js.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.bnf.Rule_ = function(type, opt_repetition) {
  /**
   * @private {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal}
   * @const
   */
  this.type_ = type;

  /** @const @private {number} */
  this.repetition_ = goog.isDef(opt_repetition) ? opt_repetition : -1;

  /** @private {r5js.parse.Nonterminal|null} */
  this.name_ = null;
};


/** @override */
r5js.bnf.Rule_.prototype.named = function(nonterminal) {
  this.name_ = nonterminal;
  return this;
};


/** @override */
r5js.bnf.Rule_.prototype.getName = function() {
  return this.name_;
};


/** @override */
r5js.bnf.Rule_.prototype.getType = function() {
  return this.type_;
};


/** @override */
r5js.bnf.Rule_.prototype.hasRepetition = function() {
  return this.repetition_ !== -1;
};


/** @override */
r5js.bnf.Rule_.prototype.getRepetition = function() {
  return this.repetition_;
};


/**
 * @param {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @return {!r5js.bnf.Rule}
 */
r5js.bnf.one = function(type) {
  return new r5js.bnf.Rule_(type);
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.bnf.Rule}
 */
r5js.bnf.zeroOrMore = function(nonterminal) {
  return new r5js.bnf.Rule_(nonterminal, 0);
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.bnf.Rule}
 */
r5js.bnf.oneOrMore = function(nonterminal) {
  return new r5js.bnf.Rule_(nonterminal, 1);
};
