goog.provide('r5js.read.bnf');


goog.require('r5js.Datum');
// TODO bl circular dependency goog.require('r5js.read.grammar');
goog.require('r5js.parse.Nonterminals');



/** @interface */
r5js.read.bnf.Rule = function() {};


/**
 * @param {!r5js.Datum} ansDatum
 * @param {!r5js.TokenStream} tokenStream
 * @return {r5js.Datum}
 */
r5js.read.bnf.Rule.prototype.match = function(ansDatum, tokenStream) {};



/**
 * @interface
 * @extends {r5js.read.bnf.Rule}
 */
r5js.read.bnf.NamedRule = function() {};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.read.bnf.NamedRule} This rule, for chaining.
 */
r5js.read.bnf.NamedRule.prototype.named = function(nonterminal) {};



/**
 * @param {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @implements {r5js.read.bnf.NamedRule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.One_ = function(type) {
  /**
     * @private {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal}
     * @const
     */
  this.type_ = type;

  /** @private {r5js.parse.Nonterminal|null} */
  this.name_ = null;
};


/** @override */
r5js.read.bnf.One_.prototype.named = function(name) {
  this.name_ = name;
  return this;
};


/** @override */
r5js.read.bnf.One_.prototype.match = function(ansDatum, tokenStream) {
  // The rule will be found in the grammar iff it is a nonterminal.
  var rule = r5js.read.grammar[this.type_];
  return rule ?
      this.matchNonterminal_(ansDatum, tokenStream, rule) :
      this.matchTerminal_(ansDatum, tokenStream);
};


/**
 * @param {!r5js.Datum} ansDatum
 * @param {!r5js.TokenStream} tokenStream
 * @param {!r5js.read.bnf.Rule} rule
 * @return {r5js.Datum}
 * @private
 */
r5js.read.bnf.One_.prototype.matchNonterminal_ = function(
    ansDatum, tokenStream, rule) {
  var parsed = rule.match(new r5js.Datum(), tokenStream);
  if (!parsed) {
    return null;
  }
  ansDatum.type = this.name_ || this.type_;
  ansDatum.appendChild(parsed);
  parsed.parent = ansDatum;
  return ansDatum;
};


/**
 * @param {!r5js.Datum} ansDatum
 * @param {!r5js.TokenStream} tokenStream
 * @return {r5js.Datum}
 * @private
 */
r5js.read.bnf.One_.prototype.matchTerminal_ = function(ansDatum, tokenStream) {
  var token = tokenStream.nextToken();
  return (token && token.getPayload() === this.type_) ?
      ansDatum :
      null;
};



/**
 * @param {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @param {number} minRepetitions
 * @implements {r5js.read.bnf.NamedRule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.AtLeast_ = function(type, minRepetitions) {
  /**
   * @private {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal}
   * @const
   */
  this.type_ = type;

  /** @const @private {number} */
  this.repetition_ = minRepetitions;

  /** @private {r5js.parse.Nonterminal|null} */
  this.name_ = null;
};


/** @override */
r5js.read.bnf.AtLeast_.prototype.match = function(ansDatum, tokenStream) {
  var rule = r5js.read.grammar[this.type_];
  var checkpoint = tokenStream.checkpoint();
  var prev, cur, firstChild;
  var num = 0;
  while (cur = rule.match(new r5js.Datum(), tokenStream)) {
    ++num;
    if (!firstChild)
      firstChild = cur;
    if (prev)
      prev.nextSibling = cur;
    prev = cur;
  }

  if (num >= this.repetition_) {
    ansDatum.type = this.name_ || this.type_;
    // TODO bl is this cast needed, or does it indicate a bug?
    ansDatum.appendChild(/** @type {!r5js.Datum} */ (firstChild));
    if (prev)
      prev.parent = ansDatum;
    return ansDatum;
  } else {
    tokenStream.restore(checkpoint);
    return null;
  }
};


/** @override */
r5js.read.bnf.AtLeast_.prototype.named = function(nonterminal) {
  this.name_ = nonterminal;
  return this;
};


/**
 * @param {!r5js.DatumType|!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @return {!r5js.read.bnf.NamedRule}
 */
r5js.read.bnf.one = function(type) {
  return new r5js.read.bnf.One_(type);
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.read.bnf.NamedRule}
 */
r5js.read.bnf.zeroOrMore = function(nonterminal) {
  return new r5js.read.bnf.AtLeast_(nonterminal, 0);
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.read.bnf.Rule}
 */
r5js.read.bnf.oneOrMore = function(nonterminal) {
  return new r5js.read.bnf.AtLeast_(nonterminal, 1);
};



/**
 * @param {!r5js.DatumType} type
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.OnePrimitive_ = function(type) {
  /** @const @private {!r5js.DatumType} */
  this.type_ = type;
};


/** @override */
r5js.read.bnf.OnePrimitive_.prototype.match = function(ansDatum, tokenStream) {
  var token = tokenStream.nextToken();
  if (!token) {
    return null;
  }
  token.formatDatum(ansDatum);
  return ansDatum.type === this.type_ ? ansDatum : null;
};


/**
 * @param {!r5js.DatumType} type
 * @return {!r5js.read.bnf.Rule}
 */
r5js.read.bnf.onePrimitive = function(type) {
  return new r5js.read.bnf.OnePrimitive_(type);
};



/**
 * @param {!Array.<!r5js.read.bnf.Rule>} rules
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.Seq_ = function(rules) {
  /** @const @private {!Array.<!r5js.read.bnf.Rule>} */
  this.rules_ = rules;
};


/** @override */
r5js.read.bnf.Seq_.prototype.match = function(ansDatum, tokenStream) {
  var checkpoint = tokenStream.checkpoint();
  for (var i = 0; i < this.rules_.length; ++i) {
    if (!this.rules_[i].match(ansDatum, tokenStream)) {
      tokenStream.restore(checkpoint);
      return null;
    }
  }
  return ansDatum;
};


/**
 * @param {...!r5js.read.bnf.Rule} var_args
 * @return {!r5js.read.bnf.Rule}
 * @suppress {checkTypes} for the varargs. TODO bl: is there a safer way
 * that still makes the BNF DSL nice?
 */
r5js.read.bnf.seq = function(var_args) {
  return new r5js.read.bnf.Seq_(arguments);
};



/**
 * @param {!Array.<!r5js.read.bnf.Rule>} rules
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.Choice_ = function(rules) {
  /** @const @private {!Array.<!r5js.read.bnf.Rule>} */
  this.rules_ = rules;
};


/** @override */
r5js.read.bnf.Choice_.prototype.match = function(ansDatum, tokenStream) {
  for (var i = 0; i < this.rules_.length; ++i) {
    var checkpoint = tokenStream.checkpoint();
    var newDatum = new r5js.Datum();
    if (this.rules_[i].match(newDatum, tokenStream)) {
      return newDatum;
    } else {
      tokenStream.restore(checkpoint);
    }
  }
  return null;
};


/**
 * @param {...!r5js.read.bnf.Rule} var_args
 * @return {!r5js.read.bnf.Rule}
 * @suppress {checkTypes}
 */
r5js.read.bnf.choice = function(var_args) {
  return new r5js.read.bnf.Choice_(arguments);
};