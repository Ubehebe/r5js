goog.provide('r5js.parse.Terminals');
goog.provide('r5js.parse.isTerminal');


/** @typedef {string} */
r5js.parse.Terminal;


/** @enum {string} */
r5js.parse.Terminals = {
  BACKTICK: '`',
  BEGIN: 'begin',
  COMMA: ',',
  COMMA_AT: ',@',
  DEFINE: 'define',
  DEFINE_SYNTAX: 'define-syntax',
  DOT: '.',
  ELLIPSIS: '...',
  IF: 'if',
  LAMBDA: 'lambda',
  LET_SYNTAX: 'let-syntax',
  LETREC_SYNTAX: 'letrec-syntax',
  LPAREN: '(',
  LPAREN_VECTOR: '#(',
  QUASIQUOTE: 'quasiquote',
  QUOTE: 'quote',
  RPAREN: ')',
  SET: 'set!',
  SYNTAX_RULES: 'syntax-rules',
  TICK: "'",
  UNQUOTE: 'unquote',
  UNQUOTE_SPLICING: 'unquote-splicing'
};


/**
 * @param {string} str
 * @return {boolean}
 * TODO bl: remove.
 */
r5js.parse.isTerminal = function(str) {
    for (var key in r5js.parse.Terminals) {
        if (str === r5js.parse.Terminals[key]) {
            return true;
        }
    }
    return false;
};
