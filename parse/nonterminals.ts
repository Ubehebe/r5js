export class Nonterminal {
  /* TODO how to forbid construction outside this file? */constructor(readonly name: string) {
  }

  /** @override */
  toString() {
    return this.name;
  }
}

export const ALTERNATE = new Nonterminal('alternate');
export const ASSIGNMENT = new Nonterminal('assignment');
export const COMMAND = new Nonterminal('command');
export const COMMAND_OR_DEFINITION = new Nonterminal('command-or-definition');
export const CONDITIONAL = new Nonterminal('conditional');
export const CONSEQUENT = new Nonterminal('consequent');
export const DATUM = new Nonterminal('datum');
export const DATUMS = new Nonterminal('datums');
export const DEFINITION = new Nonterminal('definition');
export const EXPRESSION = new Nonterminal('expression');
export const FORMALS = new Nonterminal('formals');
export const KEYWORD = new Nonterminal('keyword');
export const LAMBDA_EXPRESSION = new Nonterminal('lambda-expression');
export const LIST_QQ_TEMPLATE = new Nonterminal('list-qq-template');
export const LITERAL = new Nonterminal('literal');
export const MACRO_BLOCK = new Nonterminal('macro-block');
export const MACRO_USE = new Nonterminal('macro-use');
export const OPERAND = new Nonterminal('operand');
export const OPERATOR = new Nonterminal('operator');
export const PATTERN = new Nonterminal('pattern');
export const PATTERN_DATUM = new Nonterminal('pattern-datum');
export const PATTERN_IDENTIFIER = new Nonterminal('pattern-identifier');
export const PROCEDURE_CALL = new Nonterminal('procedure-call');
export const PROGRAM = new Nonterminal('program');
export const QUASIQUOTATION = new Nonterminal('quasiquotation');
export const QUOTATION = new Nonterminal('quotation');
export const QQ_TEMPLATE = new Nonterminal('qq-template');
export const QQ_TEMPLATE_OR_SPLICE = new Nonterminal('qq-template-or-splice');
export const SELF_EVALUATING = new Nonterminal('self-evaluating');
export const SPLICING_UNQUOTATION = new Nonterminal('splicing-unquotation');
export const SYNTAX_DEFINITION = new Nonterminal('syntax-definition');
export const SYNTAX_RULE = new Nonterminal('syntax-rule');
export const SYNTAX_SPEC = new Nonterminal('syntax-spec');
export const TEMPLATE = new Nonterminal('template');
export const TEMPLATE_DATUM = new Nonterminal('template-datum');
export const TEST = new Nonterminal('test');
export const TRANSFORMER_SPEC = new Nonterminal('transformer-spec');
export const UNQUOTATION = new Nonterminal('unquotation');
export const VARIABLE = new Nonterminal('variable');
export const VECTOR_QQ_TEMPLATE = new Nonterminal('vector-qq-template');