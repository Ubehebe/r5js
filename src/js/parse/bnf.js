goog.provide('r5js.parse.bnf');


goog.require('r5js.Datum');
goog.require('r5js.DatumType');
goog.require('r5js.parse.Terminals');
// TODO bl circular dependency goog.require('r5js.Parser');



/** @interface */
r5js.parse.bnf.Rule = function() {};


/**
 * @param {!r5js.parse.bnf.Rule} rule
 * @param {!r5js.Datum} datum
 * TODO bl remove.
 */
r5js.parse.bnf.Rule.maybeDesugar = function(rule, datum) {
  if (rule instanceof r5js.parse.bnf.Seq_ && rule.desugarFunc_) {
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
  /** @const @private {!r5js.parse.Terminal} */
  this.terminal_ = terminal;
};


/** @override */
r5js.parse.bnf.OneTerminal_.prototype.match = function(datumStream) {
  var next;
  switch (this.terminal_) {
    case r5js.parse.Terminals.DOT:
      // vacuous; we already rewrote ( ... . as .( ...
      return true;
    case r5js.parse.Terminals.LPAREN:
    case r5js.DatumType.DOTTED_LIST: // TODO bl where is from?
    case r5js.parse.Terminals.LPAREN_VECTOR:
    case r5js.parse.Terminals.TICK:
    case r5js.parse.Terminals.BACKTICK:
    case r5js.parse.Terminals.COMMA:
    case r5js.parse.Terminals.COMMA_AT:
      next = datumStream.getNextDatum();
      if (next && next.type === this.terminal_) {
        datumStream.advanceToChild();
        return true;
      } else {
        return false;
      }
    case r5js.parse.Terminals.RPAREN:
      return datumStream.maybeAdvanceToNextSiblingOfParent();
    default: // TODO bl where is this from?
      // Convenience for things like rhs({type: 'define'})
      next = datumStream.getNextDatum();
      if (next && next.payload === this.terminal_) {
        datumStream.advanceToNextSibling();
        return true;
      } else {
        return false;
      }
  }
};



/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @implements {r5js.parse.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.parse.bnf.OneNonterminal_ = function(nonterminal) {
  /** @const @private {!r5js.parse.Nonterminal} */
  this.nonterminal_ = nonterminal;
};


/** @override */
r5js.parse.bnf.OneNonterminal_.prototype.match = function(datumStream) {
  var parsed = r5js.Parser.grammar[this.nonterminal_].match(datumStream);
  if (parsed instanceof r5js.Datum) {
    parsed.setParse(this.nonterminal_);
    datumStream.advanceTo(/** @type {!r5js.Datum} */ (parsed.nextSibling));
  }
  return parsed;
};


/**
 * @param {!r5js.parse.Terminal|!r5js.parse.Nonterminal} symbol
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.one = function(symbol) {
  // TODO bl: the symbol is a nonterminal iff it is in r5js.Parser.grammar.
  // However, this function can't check that, because r5js.Parser is
  // forward-defined.
  return r5js.parse.isTerminal(symbol) ?
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
  /** @const @private {!r5js.parse.Nonterminal} */
  this.nonterminal_ = nonterminal;

  /** @const @private {number} */
  this.minRepetitions_ = minRepetitions;
};


/** @override */
r5js.parse.bnf.AtLeast_.prototype.match = function(datumStream) {
  var numParsed = 0;

  /* todo bl too hard to understand. Has to do with recovering the
     next pointer after falling off the end of a deeply-nested list. However,
     it only seems to be needed for the let-syntax and letrec-syntax
     nonterminals. This is an indication that I don't understand how the
     parser really works.

     The parser would be much simpler if each parsing action returned
     the datum it parsed on success and null on failure, rather than
     tinkering with the state pointers prev and next. I haven't done this
     so far because it would seem to require passing an additional
     node parameter around. Currently, all the parameters in the parsing
     functions are descriptions of the grammar. I probably need to
     factor the parser into parser logic and a grammar that the parser
     reads. */
  datumStream.maybeRecoverAfterDeeplyNestedList();

  var parsed;
  while (parsed = r5js.Parser.grammar[this.nonterminal_].match(datumStream)) {
    // this.next_ has already been advanced by the success of parseFunction
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
  /** @const @private {function(!r5js.Datum):boolean} */
  this.predicate_ = predicate;
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
  /** @const @private {!Array.<!r5js.parse.bnf.Rule>} */
  this.rules_ = rules;
};


/** @override */
r5js.parse.bnf.Choice_.prototype.match = function(datumStream) {
  var parsed;
  for (var i = 0; i < this.rules_.length; ++i) {
    var rule = this.rules_[i];
    if (parsed = rule.match(datumStream)) {
      if (parsed instanceof r5js.Datum) {
        r5js.parse.bnf.Rule.maybeDesugar(rule, parsed);
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
      r5js.parse.bnf.Rule.maybeDesugar(rule, root);
    }
  }

  var nextSibling = /** just in case of an empty program */ root &&
      root.nextSibling;
  datumStream.advanceTo(/** @type {!r5js.Datum} */ (nextSibling));

  if (root instanceof r5js.Datum && this.desugarFunc_) {
    root.setDesugar(this.desugarFunc_);
  }

  return root || false;
};


/**
 * @param {function(!r5js.Datum, !r5js.IEnvironment)} desugarFunc
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.Seq_.prototype.desugar = function(desugarFunc) {
  this.desugarFunc_ = desugarFunc;
  return this;
};


/**
 * @param {...!r5js.parse.bnf.Rule} var_args
 * @return {!r5js.parse.bnf.Seq_}
 * @suppress {checkTypes}
 */
r5js.parse.bnf.seq = function(var_args) {
  return new r5js.parse.bnf.Seq_(arguments);
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

  /* Change the datum following the dot to be vacuous -- it has already
     been read as part of the list preceding the dot.
     todo bl: this will cause problems with exactly one part of the grammar:
     <template> -> (<template element>+ . <template>)
     I think it's easier to check for this in the evaluator. */
  rules[indexOfDot + 1] = r5js.parse.bnf.one(r5js.parse.Terminals.DOT);
  // Find the closest opening paren to the left of the dot and rewrite it as .(
  for (var i = indexOfDot - 1; i >= 0; --i) {
    var rule = rules[i];
    if (rule instanceof r5js.parse.bnf.OneTerminal_ &&
        rule.terminal_ === r5js.parse.Terminals.LPAREN) {
      rules[i] = r5js.parse.bnf.one(r5js.parse.Terminals.LPAREN_DOT);
      break;
    }
  }
  return rules;
};

