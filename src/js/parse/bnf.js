goog.provide('r5js.parse.bnf');


goog.require('r5js.DatumType');
goog.require('r5js.parse.Terminals');
// TODO bl circular dependency goog.require('r5js.Parser');



/** @interface */
r5js.parse.bnf.Rule = function() {};


/**
 * @param {?} obj
 * @return {boolean}
 * TODO bl remove.
 */
r5js.parse.bnf.Rule.isImplementedBy = function(obj) {
  return obj instanceof r5js.parse.bnf.OneTerminal_ ||
      obj instanceof r5js.parse.bnf.OneNonterminal_ ||
      obj instanceof r5js.parse.bnf.AtLeast_ ||
      obj instanceof r5js.parse.bnf.MatchDatum_;
};


/**
 * @param {?} obj
 * @return {boolean}
 * TODO bl remove; only used in {@link r5js.Parser#rewriteImproperList_}.
 */
r5js.parse.bnf.Rule.isDot = function(obj) {
  return obj instanceof r5js.parse.bnf.OneTerminal_ &&
      obj.terminal_ === r5js.parse.Terminals.DOT;
};


/**
 * @param {?} obj
 * @return {boolean}
 * TODO bl remove; only used in {@link r5js.Parser#rewriteImproperList_}.
 */
r5js.parse.bnf.Rule.isLparen = function(obj) {
  return obj instanceof r5js.parse.bnf.OneTerminal_ &&
      obj.terminal_ === r5js.parse.Terminals.LPAREN;
};


/**
 * @param {!r5js.DatumStream} datumStream
 * @param {!r5js.Parser} parser
 * @return {boolean} True iff the parse succeeded.
 */
r5js.parse.bnf.Rule.prototype.match = function(
    datumStream, parser) {};


/** @return {!r5js.parse.Nonterminal|null} TODO bl remove. */
r5js.parse.bnf.Rule.prototype.getNonterminalType = function() {};



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


/** @override */
r5js.parse.bnf.OneTerminal_.prototype.getNonterminalType = function() {
  return null;
};


/**
 * @param {!r5js.parse.Terminal} terminal
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.oneTerminal = function(terminal) {
  return new r5js.parse.bnf.OneTerminal_(terminal);
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
r5js.parse.bnf.OneNonterminal_.prototype.match = function(
    datumStream, parser) {

  var parsed = r5js.Parser.prototype[this.nonterminal_].call(parser);
  if (parsed) {
    parsed.setParse(this.nonterminal_);
    datumStream.advanceTo(/** @type {!r5js.Datum} */ (parsed.nextSibling));
  }
  return !!parsed;
};


/** @override */
r5js.parse.bnf.OneNonterminal_.prototype.getNonterminalType = function() {
  return this.nonterminal_;
};


/**
 * @param {!r5js.parse.Nonterminal} nonterminal
 * @return {r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.oneNonterminal = function(nonterminal) {
  return new r5js.parse.bnf.OneNonterminal_(nonterminal);
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
r5js.parse.bnf.AtLeast_.prototype.match = function(datumStream, parser) {
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
  while (parsed = r5js.Parser.prototype[this.nonterminal_].call(parser)) {
    // this.next_ has already been advanced by the success of parseFunction
    parsed.setParse(this.nonterminal_);
    ++numParsed;
  }

  return numParsed >= this.minRepetitions_;
};


/** @override */
r5js.parse.bnf.AtLeast_.prototype.getNonterminalType = function() {
  return this.nonterminal_;
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


/** @override */
r5js.parse.bnf.MatchDatum_.prototype.getNonterminalType = function() {
  return null;
};


/**
 * @param {function(!r5js.Datum):boolean} predicate
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.matchDatum = function(predicate) {
  return new r5js.parse.bnf.MatchDatum_(predicate);
};

