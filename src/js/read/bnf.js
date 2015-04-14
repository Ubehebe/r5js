/* Copyright 2011-2014 Brendan Linn

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

goog.provide('r5js.read.bnf');


goog.require('r5js.Datum');
goog.require('r5js.SiblingBuffer');
goog.require('r5js.VACUOUS_PROGRAM');
goog.require('r5js.ast.DottedList');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.ast.Vector');
// TODO bl circular dependency goog.require('r5js.read.grammar');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.read.bnf.Rule');


/**
 * @param {!r5js.parse.Terminal|!r5js.parse.Nonterminal} type
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.One_ = function(type) {
  /** @const @private */ this.type_ = type.toString();
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
  const rule = r5js.read.grammar[this.type_];
  return rule ? rule.match(tokenStream) : this.matchTerminal_(tokenStream);
};


/**
 * @param {!r5js.TokenStream} tokenStream
 * @return {r5js.Datum}
 * @private
 */
r5js.read.bnf.One_.prototype.matchTerminal_ = function(tokenStream) {
  const token = tokenStream.nextToken();
  return token === this.type_ ?
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
  /** @const @private */ this.type_ = type.toString();

  /** @const @private {number} */
  this.repetition_ = minRepetitions;
};


/** @override */
r5js.read.bnf.AtLeast_.prototype.match = function(tokenStream) {
  const siblingBuffer = new r5js.SiblingBuffer();
  const rule = r5js.read.grammar[this.type_];
  const checkpoint = tokenStream.checkpoint();
  let num = 0, cur;

  while (cur = rule.match(tokenStream)) {
    siblingBuffer.appendSibling(cur);
    ++num;
  }

  if (num >= this.repetition_) {
    /* In the special case when repetition_ is 0 and 0 datums were
      (successfully) matched, siblingBuffer.toSiblings() will return null.
      However, null is used by this API to communicate failure, so we must
      return a different object. */
    return siblingBuffer.toSiblings() || r5js.VACUOUS_PROGRAM;
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
 * @param {function(new: r5js.Datum, ?)} ctor
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.OnePrimitive_ = function(ctor) {
  /** @const @private */ this.ctor_ = ctor;
};


/** @override */
r5js.read.bnf.OnePrimitive_.prototype.match = function(tokenStream) {
  const token = tokenStream.nextToken();
  return token instanceof this.ctor_ ? token : null;
};


/**
 * @param {function(new: r5js.Datum, ?)} ctor
 * @return {!r5js.read.bnf.Rule}
 */
r5js.read.bnf.onePrimitive = function(ctor) {
  return new r5js.read.bnf.OnePrimitive_(ctor);
};



/**
 * @param {!Array<!r5js.read.bnf.Rule>} rules
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.Seq_ = function(rules) {
  /** @const @private {!Array<!r5js.read.bnf.Rule>} */
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
  const siblingBuffer = new r5js.SiblingBuffer();
  const checkpoint = tokenStream.checkpoint();
  for (let i = 0; i < this.rules_.length; ++i) {
    const rule = this.rules_[i];
    const parsed = rule.match(tokenStream);
    if (parsed === r5js.read.bnf.One_.TERMINAL_SENTINEL) {
      continue;
    } else if (parsed === r5js.VACUOUS_PROGRAM) {
      continue;
    } else if (parsed) {
      siblingBuffer.appendSibling(parsed);
    } else {
      tokenStream.restore(checkpoint);
      return null;
    }
  }

  return r5js.read.bnf.Seq_.maybeCanonicalize(siblingBuffer.toList(
      /** @type {function(new: r5js.Datum, !r5js.Datum)} */ (this.ctor_)));
};


/**
 * (quote x) -> 'x
 * (quasiquote x) -> `x
 * (unquote x) -> ,x
 * (unquote-splicing x) -> ,@x
 * @param {!r5js.Datum} datum
 * @return {!r5js.Datum}
 */
r5js.read.bnf.Seq_.maybeCanonicalize = function(datum) {
  if (!(datum instanceof r5js.ast.List) || !datum.getFirstChild()) {
    return datum;
  }
  const firstChildToStrip = datum.getFirstChild();
  if (!(firstChildToStrip instanceof r5js.ast.SimpleDatum)) {
    return datum;
  }
  const realFirstChild = firstChildToStrip.getNextSibling();
  switch (firstChildToStrip.getPayload()) {
    case r5js.parse.Terminals.QUOTE:
      return new r5js.ast.Quote(realFirstChild);
    case r5js.parse.Terminals.QUASIQUOTE:
      return new r5js.ast.Quasiquote(realFirstChild);
    case r5js.parse.Terminals.UNQUOTE:
      return new r5js.ast.Unquote(realFirstChild);
    case r5js.parse.Terminals.UNQUOTE_SPLICING:
      return new r5js.ast.UnquoteSplicing(realFirstChild);
    default:
      return datum;
  }
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
 * @param {!Array<!r5js.read.bnf.Rule>} rules
 * @implements {r5js.read.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.read.bnf.Choice_ = function(rules) {
  /** @const @private {!Array<!r5js.read.bnf.Rule>} */
  this.rules_ = rules;
};


/** @override */
r5js.read.bnf.Choice_.prototype.match = function(tokenStream) {
  for (let i = 0; i < this.rules_.length; ++i) {
    const checkpoint = tokenStream.checkpoint();
    let newDatum;
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
