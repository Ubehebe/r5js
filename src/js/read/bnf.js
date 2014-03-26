goog.provide('r5js.read.bnf');


goog.require('r5js.Datum');
goog.require('r5js.Quasiquote');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.ast.DottedList');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.ast.Vector');
// TODO bl circular dependency goog.require('r5js.read.grammar');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');



/** @interface */
r5js.read.bnf.Rule = function() {};


/**
 * @param {!r5js.TokenStream} tokenStream
 * @return {r5js.Datum} The datum extracted from the token stream, or null if
 * reading was unsuccessful. Note that this may not be a proper tree:
 * rules like {@link r5js.read.bnf.AtLeast_} should return a list of siblings.
 */
r5js.read.bnf.Rule.prototype.match = function(tokenStream) {};



/**
 * @param {!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.One_ = function(type) {
  /**
     * @private {!r5js.parse.Terminal|!r5js.parse.Nonterminal}
     * @const
     */
  this.type_ = type;
};


/**
 * When a terminal is successfully matched out of the token stream,
 * the rule needs to communicate success (= a non-null return value),
 * but the terminal itself is not useful. This sentinel is returned by
 * {@link r5js.read.bnf.One_#matchTerminal_} to avoid instantiating
 * useless datums.
 * TODO bl: this API can be improved.
 * @const {!r5js.Datum}
 */
r5js.read.bnf.One_.TERMINAL_SENTINEL = new r5js.Datum();


/** @override */
r5js.read.bnf.One_.prototype.match = function(tokenStream) {
  // The rule will be found in the grammar iff it is a nonterminal.
  var rule = r5js.read.grammar[this.type_];
  return rule ? rule.match(tokenStream) : this.matchTerminal_(tokenStream);
};


/**
 * @param {!r5js.TokenStream} tokenStream
 * @return {r5js.Datum}
 * @private
 */
r5js.read.bnf.One_.prototype.matchTerminal_ = function(tokenStream) {
  var token = tokenStream.nextToken();
  if (!token) {
    return null;
  }
  var terminal = token.toDatum();
  return terminal === this.type_ ?
      r5js.read.bnf.One_.TERMINAL_SENTINEL :
      null;
};



/**
 * @param {!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @param {number} minRepetitions
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.AtLeast_ = function(type, minRepetitions) {
  /**
   * @private {!r5js.parse.Terminal|!r5js.parse.Nonterminal}
   * @const
   */
  this.type_ = type;

  /** @const @private {number} */
  this.repetition_ = minRepetitions;
};


/**
 * See {@link r5js.read.bnf.AtLeast_#match} for description.
 * @const {!r5js.Datum}
 * TODO bl: This object is mutable and can be returned from a top-level
 * call to {@link r5js.Reader#read}, which seems like a bug.
 */
r5js.read.bnf.AtLeast_.EMPTY_LIST_SENTINEL = new r5js.Datum();


/** @override */
r5js.read.bnf.AtLeast_.prototype.match = function(tokenStream) {
  var siblingBuffer = new r5js.SiblingBuffer();
  var rule = r5js.read.grammar[this.type_];
  var checkpoint = tokenStream.checkpoint();
  var num = 0, cur;

  while (cur = rule.match(tokenStream)) {
    siblingBuffer.appendSibling(cur);
    ++num;
  }

  if (num >= this.repetition_) {
    /* In the special case when repetition_ is 0 and 0 datums were
      (successfully) matched, siblingBuffer.toSiblings() will return null.
      However, null is used by this API to communicate failure, so we must
      return a different object. */
    return siblingBuffer.toSiblings() ||
        r5js.read.bnf.AtLeast_.EMPTY_LIST_SENTINEL;
  } else {
    tokenStream.restore(checkpoint);
    return null;
  }
};


/**
 * @param {!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @return {!r5js.read.bnf.Rule}
 */
r5js.read.bnf.one = function(type) {
  return new r5js.read.bnf.One_(type);
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.read.bnf.Rule}
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
r5js.read.bnf.OnePrimitive_.prototype.match = function(tokenStream) {
  var token = tokenStream.nextToken();
  if (!token) {
    return null;
  }
  var ansDatum = token.toDatum();
  return (ansDatum instanceof r5js.Datum &&
      ansDatum.getType() === this.type_) ?
      ansDatum :
      null;
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

  /** @private {function(new: r5js.Datum, !r5js.Datum)|null} */
  this.ctor_ = null;
};


/**
 * @param {function(new: r5js.Datum, !r5js.Datum)} ctor
 * @return {!r5js.read.bnf.Seq_} This rule, for chaining.
 */
r5js.read.bnf.Seq_.prototype.named = function(ctor) {
  this.ctor_ = ctor;
  return this;
};


/** @override */
r5js.read.bnf.Seq_.prototype.match = function(tokenStream) {
  var siblingBuffer = new r5js.SiblingBuffer();
  var checkpoint = tokenStream.checkpoint();
  for (var i = 0; i < this.rules_.length; ++i) {
    var rule = this.rules_[i];
    var parsed = rule.match(tokenStream);
    if (parsed === r5js.read.bnf.One_.TERMINAL_SENTINEL) {
      continue;
    } else if (parsed === r5js.read.bnf.AtLeast_.EMPTY_LIST_SENTINEL) {
      continue;
    } else if (parsed) {
      siblingBuffer.appendSibling(parsed);
    } else {
      tokenStream.restore(checkpoint);
      return null;
    }
  }
  return siblingBuffer.toList(
      /** @type {function(new: r5js.Datum, !r5js.Datum)} */ (this.ctor_));
};


/**
 * @param {...!r5js.read.bnf.Rule} var_args
 * @return {!r5js.read.bnf.Seq_}
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
r5js.read.bnf.Choice_.prototype.match = function(tokenStream) {
  for (var i = 0; i < this.rules_.length; ++i) {
    var checkpoint = tokenStream.checkpoint();
    var newDatum;
    if (newDatum = this.rules_[i].match(tokenStream)) {
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
