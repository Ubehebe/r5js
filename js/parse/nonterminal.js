goog.module('r5js.parse.Nonterminal');


class Nonterminal {
    /** @param {string} name */
    constructor(name) {
        /** @const */ this.name = name;
    }

    /** @override */
    toString() {
        return this.name;
    }
}

/** @enum {!Nonterminal} */
Nonterminal.Nonterminals = {
    ALTERNATE: new Nonterminal('alternate'),
    ASSIGNMENT: new Nonterminal('assignment'),
    COMMAND: new Nonterminal('command'),
    COMMAND_OR_DEFINITION: new Nonterminal('command-or-definition'),
    CONDITIONAL: new Nonterminal('conditional'),
    CONSEQUENT: new Nonterminal('consequent'),
    DATUM: new Nonterminal('datum'),
    DATUMS: new Nonterminal('datums'),
    DEFINITION: new Nonterminal('definition'),
    EXPRESSION: new Nonterminal('expression'),
    FORMALS: new Nonterminal('formals'),
    KEYWORD: new Nonterminal('keyword'),
    LAMBDA_EXPRESSION: new Nonterminal('lambda-expression'),
    LIST_QQ_TEMPLATE: new Nonterminal('list-qq-template'),
    LITERAL: new Nonterminal('literal'),
    MACRO_BLOCK: new Nonterminal('macro-block'),
    MACRO_USE: new Nonterminal('macro-use'),
    OPERAND: new Nonterminal('operand'),
    OPERATOR: new Nonterminal('operator'),
    PATTERN: new Nonterminal('pattern'),
    PATTERN_DATUM: new Nonterminal('pattern-datum'),
    PATTERN_IDENTIFIER: new Nonterminal('pattern-identifier'),
    PROCEDURE_CALL: new Nonterminal('procedure-call'),
    PROGRAM: new Nonterminal('program'),
    QUASIQUOTATION: new Nonterminal('quasiquotation'),
    QUOTATION: new Nonterminal('quotation'),
    QQ_TEMPLATE: new Nonterminal('qq-template'),
    QQ_TEMPLATE_OR_SPLICE: new Nonterminal('qq-template-or-splice'),
    SELF_EVALUATING: new Nonterminal('self-evaluating'),
    SPLICING_UNQUOTATION: new Nonterminal('splicing-unquotation'),
    SYNTAX_DEFINITION: new Nonterminal('syntax-definition'),
    SYNTAX_RULE: new Nonterminal('syntax-rule'),
    SYNTAX_SPEC: new Nonterminal('syntax-spec'),
    TEMPLATE: new Nonterminal('template'),
    TEMPLATE_DATUM: new Nonterminal('template-datum'),
    TEST: new Nonterminal('test'),
    TRANSFORMER_SPEC: new Nonterminal('transformer-spec'),
    UNQUOTATION: new Nonterminal('unquotation'),
    VARIABLE: new Nonterminal('variable'),
    VECTOR_QQ_TEMPLATE: new Nonterminal('vector-qq-template')
};

exports = Nonterminal;
