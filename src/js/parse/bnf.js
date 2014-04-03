goog.provide('r5js.parse.bnf');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.ast.DottedList');
goog.require('r5js.ast.List');
goog.require('r5js.ast.Quasiquote');
goog.require('r5js.ast.Quote');
goog.require('r5js.ast.SimpleDatum');
goog.require('r5js.ast.Unquote');
goog.require('r5js.ast.UnquoteSplicing');
goog.require('r5js.ast.Vector');
goog.require('r5js.parse.Terminals');
// TODO bl circular dependency goog.require('r5js.Parser');



/** @interface */
r5js.parse.bnf.Rule = function() {};


/**
 * @param {!r5js.parse.bnf.Rule} rule
 * @param {!r5js.Datum} datum
 * @private
 * TODO bl remove.
 */
r5js.parse.bnf.Rule.maybeDesugar_ = function(rule, datum) {
  if (rule.desugarFunc_) {
    datum.setDesugar(rule.desugarFunc_);
  }
};


/**
 * @param {!r5js.DatumStream} datumStream
 * @return {boolean|!r5js.Datum} True iff the parse succeeded.
 */
r5js.parse.bnf.Rule.prototype.match = function(datumStream) {};



/**
 * @param {!r5js.parse.Terminal} terminal
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.OneTerminal_ = function(terminal) {
  /** @const @private */ this.terminal_ = terminal;
};


/**
 * @override
 * TODO bl put the instanceof checks into the Datum subclasses
 */
r5js.parse.bnf.OneTerminal_.prototype.match = function(datumStream) {
  if (this.terminal_ === r5js.parse.Terminals.RPAREN) {
    return datumStream.maybeAdvanceToNextSiblingOfParent();
  }

  var next = datumStream.getNextDatum();
  var match = false;
  switch (this.terminal_) {
    case r5js.parse.Terminals.LPAREN:
      match = next instanceof r5js.ast.List;
      break;
    case r5js.parse.Terminals.LPAREN_DOT:
      match = next instanceof r5js.ast.DottedList;
      break;
    case r5js.parse.Terminals.LPAREN_VECTOR:
      match = next instanceof r5js.ast.Vector;
      break;
    case r5js.parse.Terminals.TICK:
      match = next instanceof r5js.ast.Quote;
      break;
    case r5js.parse.Terminals.BACKTICK:
      match = next instanceof r5js.ast.Quasiquote;
      break;
    case r5js.parse.Terminals.COMMA:
      match = next instanceof r5js.ast.Unquote;
      break;
    case r5js.parse.Terminals.COMMA_AT:
      match = next instanceof r5js.ast.UnquoteSplicing;
      break;
    default: // TODO bl where is this from?
      if (next instanceof r5js.ast.SimpleDatum &&
          next.getPayload() === this.terminal_) {
        datumStream.advanceToNextSibling();
        return true;
      } else {
        return false;
      }
  }

  if (match) {
    datumStream.advanceToChild();
  }
  return match;
};



/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.OneNonterminal_ = function(nonterminal) {
  /** @const @private */ this.nonterminal_ = nonterminal;
  /** @private {!r5js.DesugarFunc|null} */ this.desugarFunc_ = null;
};


/**
 * @param {!r5js.DesugarFunc} desugarFunc
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.OneNonterminal_.prototype.desugar = function(desugarFunc) {
  this.desugarFunc_ = desugarFunc;
  return this;
};


/** @override */
r5js.parse.bnf.OneNonterminal_.prototype.match = function(datumStream) {
  var parsed = r5js.Parser.grammar[this.nonterminal_].match(datumStream);
  if (parsed instanceof r5js.Datum) {
    parsed.setParse(this.nonterminal_);
    r5js.parse.bnf.Rule.maybeDesugar_(this, parsed);
    datumStream.advanceTo(/** @type {!r5js.Datum} */ (parsed.getNextSibling()));
  }
  return parsed;
};


/**
 * @param {!r5js.parse.Terminal|!r5js.parse.Nonterminal} symbol
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.one = function(symbol) {
  return goog.isString(symbol) ?
      new r5js.parse.bnf.OneTerminal_(symbol) :
      new r5js.parse.bnf.OneNonterminal_(symbol);
};



/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @param {number} minRepetitions
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.AtLeast_ = function(nonterminal, minRepetitions) {
  /** @const @private */ this.nonterminal_ = nonterminal;
  /** @const @private */ this.minRepetitions_ = minRepetitions;
};


/** @override */
r5js.parse.bnf.AtLeast_.prototype.match = function(datumStream) {
  var numParsed = 0;
  var parsed;
  while (parsed = r5js.Parser.grammar[this.nonterminal_].match(datumStream)) {
    parsed.setParse(this.nonterminal_);
    ++numParsed;
  }
  return numParsed >= this.minRepetitions_;
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.zeroOrMore = function(nonterminal) {
  return new r5js.parse.bnf.AtLeast_(nonterminal, 0);
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.oneOrMore = function(nonterminal) {
  return new r5js.parse.bnf.AtLeast_(nonterminal, 1);
};



/**
 * @param {function(!r5js.Datum):boolean} predicate
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.MatchDatum_ = function(predicate) {
  /** @const @private */ this.predicate_ = predicate;
};


/** @override */
r5js.parse.bnf.MatchDatum_.prototype.match = function(datumStream) {
  var next = datumStream.getNextDatum();
  if (next && this.predicate_(next)) {
    datumStream.advanceToNextSibling();
    return true;
  } else {
    return false;
  }
};


/**
 * @param {function(!r5js.Datum):boolean} predicate
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.matchDatum = function(predicate) {
  return new r5js.parse.bnf.MatchDatum_(predicate);
};



/**
 * @param {!Array.<!r5js.parse.bnf.Rule>} rules
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.Choice_ = function(rules) {
  /** @const @private */ this.rules_ = rules;
};


/** @override */
r5js.parse.bnf.Choice_.prototype.match = function(datumStream) {
  var parsed;
  for (var i = 0; i < this.rules_.length; ++i) {
    var rule = this.rules_[i];
    if (parsed = rule.match(datumStream)) {
      if (parsed instanceof r5js.Datum) {
        r5js.parse.bnf.Rule.maybeDesugar_(rule, parsed);
      }
      return parsed;
    }
  }
  return false;
};


/**
 * @param {...!r5js.parse.bnf.Rule} var_args
 * @return {!r5js.parse.bnf.Rule}
 * @suppress {checkTypes}
 */
r5js.parse.bnf.choice = function(var_args) {
  return new r5js.parse.bnf.Choice_(arguments);
};



/**
 * @param {!Array.<!r5js.parse.bnf.Rule>} rules
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.Seq_ = function(rules) {
  /** @const @private {!Array.<!r5js.parse.bnf.Rule>} */
  this.rules_ = r5js.parse.bnf.Seq_.rewriteImproperList_(rules);

  /** @private {function(!r5js.Datum, !r5js.IEnvironment)|null} */
  this.desugarFunc_ = null;
};


/** @override */
r5js.parse.bnf.Seq_.prototype.match = function(datumStream) {
  var root = datumStream.getNextDatum();

  for (var i = 0; i < this.rules_.length; ++i) {
    var rule = this.rules_[i];

    // Process parsing actions
    if (!rule.match(datumStream)) {
      datumStream.advanceTo(/** @type {!r5js.Datum} */ (root));
      return false;
    } else if (root instanceof r5js.Datum) {
      r5js.parse.bnf.Rule.maybeDesugar_(rule, root);
    }
  }

  var nextSibling = /** just in case of an empty program */ root &&
      root.getNextSibling();
  datumStream.advanceTo(/** @type {!r5js.Datum} */ (nextSibling));

  if (root instanceof r5js.Datum && this.desugarFunc_) {
    root.setDesugar(this.desugarFunc_);
  }

  return root || false;
};


/**
 * @param {!r5js.DesugarFunc} desugarFunc
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.Seq_.prototype.desugar = function(desugarFunc) {
  this.desugarFunc_ = desugarFunc;
  return this;
};


/**
 * @param {...!r5js.parse.bnf.Rule} var_args
 * @return {!r5js.parse.bnf.Seq_}
 */
r5js.parse.bnf.seq = function(var_args) {
  // Copy the arguments into a real array so that rewriteImproperList_
  // can use Array.prototype.splice.
  var rules = [];
  for (var i = 0; i < arguments.length; ++i) {
    rules.push(arguments[i]);
  }
  return new r5js.parse.bnf.Seq_(rules);
};


/**
 * @param {...!r5js.parse.bnf.Rule} var_args
 * @return {!r5js.parse.bnf.Seq_}
 */
r5js.parse.bnf.list = function(var_args) {
  var rules = [];
  rules.push(r5js.parse.bnf.one(r5js.parse.Terminals.LPAREN));
  for (var i = 0; i < arguments.length; ++i) {
    rules.push(arguments[i]);
  }
  rules.push(r5js.parse.bnf.one(r5js.parse.Terminals.RPAREN));
  return new r5js.parse.bnf.Seq_(rules);
};


/**
 * @param {!r5js.parse.bnf.Rule} beforeDot
 * @param {!r5js.parse.bnf.Rule} afterDot
 * @return {!r5js.parse.bnf.Seq_}
 */
r5js.parse.bnf.dottedList = function(beforeDot, afterDot) {
  var rules = [
    r5js.parse.bnf.one(r5js.parse.Terminals.LPAREN),
    beforeDot,
    r5js.parse.bnf.one(r5js.parse.Terminals.DOT),
    afterDot];
  return new r5js.parse.bnf.Seq_(rules);
};


/**
 * @param {...!r5js.parse.bnf.Rule} var_args
 * @return {!r5js.parse.bnf.Seq_}
 */
r5js.parse.bnf.vector = function(var_args) {
  var rules = [];
  rules.push(r5js.parse.bnf.one(r5js.parse.Terminals.LPAREN_VECTOR));
  for (var i = 0; i < arguments.length; ++i) {
    rules.push(arguments[i]);
  }
  rules.push(r5js.parse.bnf.one(r5js.parse.Terminals.RPAREN));
  return new r5js.parse.bnf.Seq_(rules);
};


/**
 * This is a convenience function: we want to specify parse rules like
 * (<variable>+ . <variable>) as if we don't know ahead of time whether
 * the list is going to be dotted or not, but the reader already knows.
 * Proper and improper lists are both represented as first-child-next-sibling
 * linked lists; the only difference is the type ('(' vs. '.('). So we rewrite
 * the parse rules to conform to the reader's knowledge.
 * @param {!Array.<!r5js.parse.bnf.Rule>} rules
 * @return {!Array.<!r5js.parse.bnf.Rule>} The modified rules array.
 * @private
 */
r5js.parse.bnf.Seq_.rewriteImproperList_ = function(rules) {
  // example: (define (x . y) 1) => (define .( x . ) 1)
  /* No RHS in the grammar has more than one dot.
     This will break if such a rule is added. */
  var indexOfDot = goog.array.findIndex(rules, function(rule) {
    return rule instanceof r5js.parse.bnf.OneTerminal_ &&
        rule.terminal_ === r5js.parse.Terminals.DOT;
  });

  if (indexOfDot === -1) {
    return rules;
  }

  // Find the closest opening paren to the left of the dot and rewrite it as .(
  for (var i = indexOfDot - 1; i >= 0; --i) {
    var rule = rules[i];
    if (rule instanceof r5js.parse.bnf.OneTerminal_ &&
        rule.terminal_ === r5js.parse.Terminals.LPAREN) {
      rules[i] = r5js.parse.bnf.one(r5js.parse.Terminals.LPAREN_DOT);
      break;
    }
  }
  /* Splice out the dot and the datum following the dot -- it has already
     been read as part of the list preceding the dot.
     todo bl: this will cause problems with exactly one part of the grammar:
     <template> -> (<template element>+ . <template>)
     I think it's easier to check for this in the evaluator. */
  rules.splice(indexOfDot, 2);
  return rules;
};

