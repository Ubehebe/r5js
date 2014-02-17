goog.provide('r5js.bnf');


goog.require('r5js.Datum');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.scan.tokenTypeForDatumType');



/** @interface */
r5js.bnf.Rule = function() {};


/**
 * @param {!r5js.Datum} ansDatum
 * @param {!r5js.scan.TokenStream} tokenStream
 * @param {function():r5js.Datum} parseDatum
 * @param {function():r5js.Datum} parseDatums
 * @return {r5js.Datum}
 */
r5js.bnf.Rule.prototype.match = function(
    ansDatum, tokenStream, parseDatum, parseDatums) {};


/** @return {?r5js.parse.Nonterminal} */
r5js.bnf.Rule.prototype.getName = function() {};


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
r5js.bnf.Rule_.prototype.match = function(
    ansDatum, tokenStream, parseDatum, parseDatums) {
  var parseFunction = this.type_ === r5js.parse.Nonterminals.DATUM ?
      parseDatum :
      parseDatums;
  return this.repetition_ === -1 ?
      this.matchNoRepetition_(ansDatum, parseFunction) :
      this.matchRepetition_(ansDatum, tokenStream, parseFunction);
};


/**
 * @param {!r5js.Datum} ansDatum
 * @param {function(): r5js.Datum} parseFunction
 * @return {r5js.Datum}
 * @private
 */
r5js.bnf.Rule_.prototype.matchNoRepetition_ = function(
    ansDatum, parseFunction) {
  var parsed = parseFunction();
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
 * @param {!r5js.scan.TokenStream} tokenStream
 * @param {function():r5js.Datum} parseFunction
 * @return {r5js.Datum}
 * @private
 */
r5js.bnf.Rule_.prototype.matchRepetition_ = function(
    ansDatum, tokenStream, parseFunction) {
  var checkpoint = tokenStream.checkpoint();
  var prev, cur, firstChild;
  var num = 0;
  while (cur = parseFunction()) {
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
r5js.bnf.Rule_.prototype.named = function(nonterminal) {
  this.name_ = nonterminal;
  return this;
};


/** @override */
r5js.bnf.Rule_.prototype.getName = function() {
  return this.name_;
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



/**
 * @param {!r5js.parse.Terminal} terminal
 * @implements {r5js.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.bnf.OneTerminal_ = function(terminal) {
  /** @const @private {!r5js.parse.Terminal} */
  this.terminal_ = terminal;
};


/** @override */
r5js.bnf.OneTerminal_.prototype.getName = function() {
  return 'oops!';
};


/** @override */
r5js.bnf.OneTerminal_.prototype.named = function() {
  return this;
};


/** @override */
r5js.bnf.OneTerminal_.prototype.match = function(ansDatum, tokenStream) {
  var token = tokenStream.nextToken();
  return (token && token.getPayload() === this.terminal_) ?
      ansDatum :
      null;
};


/**
 * @param {!r5js.parse.Terminal} terminal
 * @return {!r5js.bnf.Rule}
 */
r5js.bnf.oneTerminal = function(terminal) {
  return new r5js.bnf.OneTerminal_(terminal);
};



/**
 * @param {!r5js.DatumType} type
 * @implements {r5js.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.bnf.OnePrimitive_ = function(type) {
  /** @const @private {!r5js.DatumType} */
  this.type_ = type;
};


/** @override */
r5js.bnf.OnePrimitive_.prototype.getName = function() {
  return 'sorry';
};


/** @override */
r5js.bnf.OnePrimitive_.prototype.named = function() {
  return this;
};


/** @override */
r5js.bnf.OnePrimitive_.prototype.match = function(ansDatum, tokenStream) {
  var token = tokenStream.nextToken();
  if (!token) {
    return null;
  }
  if (!token.matchesType(/** @type {!r5js.scan.TokenType} */ (
      r5js.scan.tokenTypeForDatumType(this.type_)))) {
    return null;
  }
  ansDatum.payload = token.getPayload();
  ansDatum.type = this.type_;
  return ansDatum;
};


/**
 * @param {!r5js.DatumType} type
 * @return {!r5js.bnf.Rule}
 */
r5js.bnf.onePrimitive = function(type) {
  return new r5js.bnf.OnePrimitive_(type);
};



/**
 * @param {!Array.<!r5js.bnf.Rule>} rules
 * @implements {r5js.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.bnf.Seq_ = function(rules) {
  /** @const @private {!Array.<!r5js.bnf.Rule>} */
  this.rules_ = rules;
};


/** @override */
r5js.bnf.Seq_.prototype.named = function() {
  return this;
};


/** @override */
r5js.bnf.Seq_.prototype.getName = function() {
  return 'whoops';
};


/** @override */
r5js.bnf.Seq_.prototype.match = function(
    ansDatum, tokenStream, parseDatum, parseDatums) {
  var checkpoint = tokenStream.checkpoint();
  for (var i = 0; i < this.rules_.length; ++i) {
    if (!this.rules_[i].match(
        ansDatum, tokenStream, parseDatum, parseDatums)) {
      tokenStream.restore(checkpoint);
      return null;
    }
  }
  return ansDatum;
};


/**
 * @param {...!r5js.bnf.Rule} var_args
 * @return {!r5js.bnf.Rule}
 * @suppress {checkTypes} for the varargs. TODO bl: is there a safer way
 * that still makes the BNF DSL nice?
 */
r5js.bnf.seq = function(var_args) {
  return new r5js.bnf.Seq_(arguments);
};



/**
 * @param {!Array.<!r5js.bnf.Rule>} rules
 * @implements {r5js.bnf.Rule}
 * @struct
 * @constructor
 * @private
 */
r5js.bnf.Choice_ = function(rules) {
  /** @const @private {!Array.<!r5js.bnf.Rule>} */
  this.rules_ = rules;
};


/** @override */
r5js.bnf.Choice_.prototype.match = function(
    ansDatum, tokenStream, parseDatum, parseDatums) {
  for (var i = 0; i < this.rules_.length; ++i) {
    var checkpoint = tokenStream.checkpoint();
    var newDatum = new r5js.Datum();
    if (this.rules_[i].match(newDatum, tokenStream, parseDatum, parseDatums)) {
      return newDatum;
    } else {
      tokenStream.restore(checkpoint);
    }
  }
  return null;
};


/** @override */
r5js.bnf.Choice_.prototype.named = function() {
  return this;
};


/** @override */
r5js.bnf.Choice_.prototype.getName = function() {
  return 'xyzzy';
};


/**
 * @param {...!r5js.bnf.Rule} var_args
 * @return {!r5js.bnf.Rule}
 * @suppress {checkTypes}
 */
r5js.bnf.choice = function(var_args) {
  return new r5js.bnf.Choice_(arguments);
};