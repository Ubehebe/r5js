goog.provide('r5js.parse.bnf');


goog.require('r5js.DatumType');
goog.require('r5js.parse.Terminals');



/** @interface */
r5js.parse.bnf.Rule = function() {};


/**
 * @param {?} obj
 * @return {boolean}
 */
r5js.parse.bnf.Rule.isImplementedBy = function(obj) {
  return obj instanceof r5js.parse.bnf.OneTerminal_; // TODO bl
};


/**
 * @param {!r5js.DatumStream} datumStream
 * @return {boolean} True iff the parse succeeded.
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
  /** @const @private {!r5js.parse.Nonterminal} */
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
 * @param {!r5js.parse.Terminal} terminal
 * @return {!r5js.parse.bnf.Rule}
 */
r5js.parse.bnf.oneTerminal = function(terminal) {
  return new r5js.parse.bnf.OneTerminal_(terminal);
};
