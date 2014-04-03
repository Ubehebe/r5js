goog.provide('r5js.parse.Nonterminals');



/**
 * @param {string} name
 * @struct
 * @constructor
 */
r5js.parse.Nonterminal = function(name) {
  /** @const */ this.name = name;
};


/** @override */
r5js.parse.Nonterminal.prototype.toString = function() {
  return this.name;
};


/** @enum {!r5js.parse.Nonterminal} */
r5js.parse.Nonterminals = {
  ALTERNATE: new r5js.parse.Nonterminal('alternate'),
  ASSIGNMENT: new r5js.parse.Nonterminal('assignment'),
  COMMAND: new r5js.parse.Nonterminal('command'),
  COMMAND_OR_DEFINITION: new r5js.parse.Nonterminal('command-or-definition'),
  CONDITIONAL: new r5js.parse.Nonterminal('conditional'),
  CONSEQUENT: new r5js.parse.Nonterminal('consequent'),
  DATUM: new r5js.parse.Nonterminal('datum'),
  DATUMS: new r5js.parse.Nonterminal('datums'),
  DEFINITION: new r5js.parse.Nonterminal('definition'),
  EXPRESSION: new r5js.parse.Nonterminal('expression'),
  FORMALS: new r5js.parse.Nonterminal('formals'),
  KEYWORD: new r5js.parse.Nonterminal('keyword'),
  LAMBDA_EXPRESSION: new r5js.parse.Nonterminal('lambda-expression'),
  LIST_QQ_TEMPLATE: new r5js.parse.Nonterminal('list-qq-template'),
  LITERAL: new r5js.parse.Nonterminal('literal'),
  MACRO_BLOCK: new r5js.parse.Nonterminal('macro-block'),
  MACRO_USE: new r5js.parse.Nonterminal('macro-use'),
  OPERAND: new r5js.parse.Nonterminal('operand'),
  OPERATOR: new r5js.parse.Nonterminal('operator'),
  PATTERN: new r5js.parse.Nonterminal('pattern'),
  PATTERN_DATUM: new r5js.parse.Nonterminal('pattern-datum'),
  PATTERN_IDENTIFIER: new r5js.parse.Nonterminal('pattern-identifier'),
  PROCEDURE_CALL: new r5js.parse.Nonterminal('procedure-call'),
  PROGRAM: new r5js.parse.Nonterminal('program'),
  QUASIQUOTATION: new r5js.parse.Nonterminal('quasiquotation'),
  QUOTATION: new r5js.parse.Nonterminal('quotation'),
  QQ_TEMPLATE: new r5js.parse.Nonterminal('qq-template'),
  QQ_TEMPLATE_OR_SPLICE: new r5js.parse.Nonterminal('qq-template-or-splice'),
  SELF_EVALUATING: new r5js.parse.Nonterminal('self-evaluating'),
  SPLICING_UNQUOTATION: new r5js.parse.Nonterminal('splicing-unquotation'),
  SYNTAX_DEFINITION: new r5js.parse.Nonterminal('syntax-definition'),
  SYNTAX_RULE: new r5js.parse.Nonterminal('syntax-rule'),
  SYNTAX_SPEC: new r5js.parse.Nonterminal('syntax-spec'),
  TEMPLATE: new r5js.parse.Nonterminal('template'),
  TEMPLATE_DATUM: new r5js.parse.Nonterminal('template-datum'),
  TEST: new r5js.parse.Nonterminal('test'),
  TRANSFORMER_SPEC: new r5js.parse.Nonterminal('transformer-spec'),
  UNQUOTATION: new r5js.parse.Nonterminal('unquotation'),
  VARIABLE: new r5js.parse.Nonterminal('variable'),
  VECTOR_QQ_TEMPLATE: new r5js.parse.Nonterminal('vector-qq-template')
};
