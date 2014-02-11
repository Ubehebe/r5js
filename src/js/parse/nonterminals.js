goog.provide('r5js.parse.Nonterminals');


/** @typedef {string} */
r5js.parse.Nonterminal;


/** @enum {!r5js.parse.Nonterminal} */
r5js.parse.Nonterminals = {
  ALTERNATE: 'alternate',
  ASSIGNMENT: 'assignment',
  COMMAND: 'command',
  COMMAND_OR_DEFINITION: 'command-or-definition',
  CONDITIONAL: 'conditional',
  CONSEQUENT: 'consequent',
  DEFINITION: 'definition',
  EXPRESSION: 'expression',
  FORMALS: 'formals',
  KEYWORD: 'keyword',
  LAMBDA_EXPRESSION: 'lambda-expression',
  LIST_QQ_TEMPLATE: 'list-qq-template',
  LITERAL: 'literal',
  MACRO_BLOCK: 'macro-block',
  MACRO_USE: 'macro-use',
  OPERAND: 'operand',
  OPERATOR: 'operator',
  PATTERN: 'pattern',
  PATTERN_DATUM: 'pattern-datum',
  PATTERN_IDENTIFIER: 'pattern-identifier',
  PROCEDURE_CALL: 'procedure-call',
  PROGRAM: 'program',
  QUASIQUOTATION: 'quasiquotation',
  QUOTATION: 'quotation',
  QQ_TEMPLATE: 'qq-template',
  QQ_TEMPLATE_OR_SPLICE: 'qq-template-or-splice',
  SELF_EVALUATING: 'self-evaluating',
  SPLICING_UNQUOTATION: 'splicing-unquotation',
  SYNTAX_DEFINITION: 'syntax-definition',
  SYNTAX_RULE: 'syntax-rule',
  SYNTAX_SPEC: 'syntax-spec',
  TEMPLATE: 'template',
  TEMPLATE_DATUM: 'template-datum',
  TEST: 'test',
  TRANSFORMER_SPEC: 'transformer-spec',
  UNQUOTATION: 'unquotation',
  VARIABLE: 'variable',
  VECTOR_QQ_TEMPLATE: 'vector-qq-template'
};
