goog.module('r5js.ParserImpl');

const Assignment = goog.require('r5js.Assignment');
const Branch = goog.require('r5js.Branch');
const {CompoundDatum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/compound_datum');
const {Datum, ProcCallLike, UNSPECIFIED_VALUE, VACUOUS_PROGRAM, getLastProcCallLike} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const {SimpleDatum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/simple_datum');
const {Identifier} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {DatumStream} = require('/js/parse/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/datum_stream');
const EllipsisTransformer = goog.require('r5js.EllipsisTransformer');
const {Grammar} = require('/js/parse/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/grammar');
const ListLikeTransformer = goog.require('r5js.ListLikeTransformer');
const Macro = goog.require('r5js.Macro');
const {Macro: MacroDatum} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/macro');
const MacroIdTransformer = goog.require('r5js.MacroIdTransformer');
const Parser = goog.require('r5js.Parser');
const ProcCall = goog.require('r5js.ProcCall');
const {Rule} = require('/js/parse/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/rule');
const RuleFactory = goog.require('r5js.parse.RuleFactory');
const {SiblingBuffer} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/sibling_buffer');
const {String} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/string');
const Subtransformer = goog.require('r5js.Subtransformer');
const TopLevelAssignment = goog.require('r5js.TopLevelAssignment');
const TopLevelSyntaxAssignment = goog.require('r5js.TopLevelSyntaxAssignment');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const VarargsUserDefinedProcedure = goog.require('r5js.VarargsUserDefinedProcedure');
const {Vector} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/vector');
const {extractDefinition} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/util');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {List} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/list');
const {
    ALTERNATE,
    ASSIGNMENT,
    COMMAND,
    COMMAND_OR_DEFINITION,
    CONDITIONAL,
    CONSEQUENT,
    DATUM,
    DATUMS,
    DEFINITION,
    EXPRESSION,
    FORMALS,
    KEYWORD,
    LAMBDA_EXPRESSION,
    LIST_QQ_TEMPLATE,
    LITERAL,
    MACRO_BLOCK,
    MACRO_USE,
    OPERAND,
    OPERATOR,
    PATTERN,
    PATTERN_DATUM,
    PATTERN_IDENTIFIER,
    PROCEDURE_CALL,
    PROGRAM,
    QUASIQUOTATION,
    QUOTATION,
    QQ_TEMPLATE,
    QQ_TEMPLATE_OR_SPLICE,
    SELF_EVALUATING,
    SPLICING_UNQUOTATION,
    SYNTAX_DEFINITION,
    SYNTAX_RULE,
    SYNTAX_SPEC,
    TEMPLATE,
    TEMPLATE_DATUM,
    TEST,
    TRANSFORMER_SPEC,
    UNQUOTATION,
    VARIABLE,
    VECTOR_QQ_TEMPLATE,
    Nonterminal,
} = require('/js/read/shim_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {RenameHelper} = require('/js/ast/rename_helper_collect_es6_sources.es6/node_modules/__main__/js/ast/rename_helper');
const {
    BACKTICK,
    BEGIN,
    COMMA,
    COMMA_AT,
    DEFINE,
    DEFINE_SYNTAX,
    DOT,
    ELLIPSIS,
    IF,
    LAMBDA,
    LET_SYNTAX,
    LETREC_SYNTAX,
    LPAREN,
    LPAREN_DOT,
    LPAREN_VECTOR,
    QUASIQUOTE,
    QUOTE,
    RPAREN,
    SET,
    SYNTAX_RULES,
    TICK,
    UNQUOTE,
    UNQUOTE_SPLICING,
} = require('/js/parse/terminals_collect_es6_sources.es6/node_modules/__main__/js/parse/terminals');
const {isParserSensitiveId} = require('/js/parse/rename_util_collect_es6_sources.es6/node_modules/__main__/js/parse/rename_util');

/* todo bl: this file should not exist.

 Short explanation: (define if +) (if 1 2 3) => 6 ; legal

 Longer explanation: having read the standard closely, I believe
 that Scheme allows every identifier to be rebound in principle*.
 This includes identifiers that are unbound in the default environment,
 like "foo"; identifiers that are bound to values in the default environment,
 like "+"; and identifiers that are bound to syntax in the default
 environment. This last group includes identifiers that are bound
 to macros, like "let", but also identifiers that are "bound to" builtin
 syntax, like "define".

 It is this last subgroup that makes having a "parser" a bad idea.
 In the example above, (define if +) must parse as a definition, which
 means if must parse as a variable. (if 1 2 3) must parse as a procedure
 call, not as a conditional.

 It might be possible to make the parser aware of the fundamental
 syntax identifiers that are currently rebound, but it seems like a lot
 of work. The real solution is to delete the parser, putting the datum
 tree straight on the trampoline. The fundamental syntax identifiers
 would merely point to "BuiltinSyntax" objects in the default environment.

 The drawback of this approach would be that all the "well-formedness"
 code currently in the parser, and expressed declaratively as grammar
 rules, would have to migrate somehow to the trampoline. For many
 cases, it would be okay for the trampoline to try its best and raise
 an error if it really couldn't evaluate something. For example,

 (if 1 2 3 4)

 would be an easy error to detect if if has its default "BuiltinSyntax"
 binding. However, there would be some tricky cases. How would
 we assure that all the definitions in a procedure body come before
 all the expressions, and conversely, how would we allow intermixing
 of definitions and expressions at the top level? How would we detect
 that the following is in fact ungrammatical:

 (define (foo) (begin (define x 1) (+ x x)))

 So there are many questions to think about before excising the parser.

 In the meantime, I've written a hack that is appropriate as long as
 rebinding of fundamental syntax identifiers is rare (which, arguably,
 it should be, since it decreases program readability). If the parser
 parses a fundamental syntax identifier as a variable, after parsing is
 complete, it renames the variable to something safe and does a complete
 re-parse.

 *footnote: the standard is somewhat contradictory on whether all
 identifiers can be variables. The rule in the lexical grammar (7.1.1) is

 <variable> -> <any <identifier> that isn’t also a <syntactic keyword>>

 Against this is the discussion in section 4.3 which states:

 "The syntactic keyword of a macro may shadow variable bindings,
 and local variable bindings may shadow keyword bindings."

 Additionally, the last example in section 4.3.1 appears to show
 if being rebound to #f. Finally, as circumstantial evidence both
 MIT Scheme and PLT Scheme support things like the example at the top of
 this comment.

 Note that rebinding an identifier is disallowed in certain cases that
 might lead to circularity (see section 5.3); this explicitly disallows
 (define define ...), though both MIT Scheme and PLT Scheme allow that.

 todo bl: implement the circularity-checking algorithm described
 in R6RS. */


/** @implements {Parser} */
class ParserImpl {
    /** @param {!Datum} root The root of the tree to parse. */
    constructor(root) {
        /** @const @private {!DatumStream} */
        this.datumStream_ = DatumStream.create(root);
    }

    /** @override */
    parse(nonterminal=PROGRAM) {
        const parsedRoot = /** @type {!Datum} */ (
            grammar[nonterminal].match(this.datumStream_));
        if (parsedRoot) {
            /* Special case for non-vacuous but malformed programs.
             A "successful" parse of a nonvacuous program that has only
             the top-level PROGRAM nonterminal set is actually a parse error,
             since it matched none of the right-hand side PROGRAM rules.
             (This is a deficiency in the parser implementation that I don't care
             to correct, since the parser should eventually go away.)
             This includes the empty list program, which reads successfully as a datum
             but doesn't parse as anything. Occurrences of the empty list under
             the top level are runtime errors (IllegalEmptyApplication) rather
             than parse errors. */
            if (nonterminal === PROGRAM &&
                parsedRoot != VACUOUS_PROGRAM && !parsedRoot.peekParse()) {
                return null;
            }
            parsedRoot.setParse(nonterminal);
        }
        return (nonterminal === PROGRAM)
            ? maybeFixParserSensitiveIds(parsedRoot)
            : parsedRoot;
    }
}

    /** @private {boolean} */
let fixParserSensitiveIds_ = false;

/** @const {!Object<!Nonterminal, !Rule>} */
const grammar = {};

class GrammarImpl extends Grammar {

    constructor() {
        super();
    }

    /** @override */
    ruleFor(nonterminal) {
        return grammar[nonterminal];
    }
}

/** @const {!RuleFactory} */
const _ = new RuleFactory(new GrammarImpl());

/* <expression> -> <variable>
 | <literal>
 | <procedure call>
 | <lambda expression>
 | <conditional>
 | <assignment>
 | <derived expression> (these are all macros, not needed in grammar)
 | <macro use>
 | <macro block>
 */
grammar[EXPRESSION] =
    /* In order to support shadowing of syntactic keywords,
    the order of the following rules is important. Consider:

    (define if 3)
    (+ 1 if) => 4

    For (+ 1 if) to parse as a procedure call, if must parse as a variable.
    This somewhat contradicts the rule at 7.1.1:

    <variable> -> <any <identifier> that isn't also a <syntactic keyword>>

    But if we can't ensure that variables aren't syntactic keywords, we
    must ensure that "built-in" syntax isn't accidentally captured
    as variables. If the procedure call RHS was listed before the lambda
    expression RHS, then for example

     (lambda () 1)

     would parse as a procedure call.

     todo bl: the real solution is to simplify the grammar even more
     so that (lambda () 1) parses as something like a macro use, then
     install a "super-macro" for lambda that contains custom logic in
     JavaScript. That way,  the syntactic keyword could be shadowed
     appropriately. */
    _.choice(
    _.one(VARIABLE),
    _.one(LITERAL),
    _.one(LAMBDA_EXPRESSION),
    _.one(CONDITIONAL),
    _.one(ASSIGNMENT),
    _.one(QUASIQUOTATION).desugar((node, env) =>
            (/** @type {!CompoundDatum} */ (node)).setQuasiquotationLevel(1)),
    _.list(
            _.one(BEGIN),
            _.oneOrMore(EXPRESSION)).
        desugar((node, env) => node.at(EXPRESSION).sequence(env)),
    _.one(MACRO_BLOCK),
    _.one(PROCEDURE_CALL),
    _.one(MACRO_USE));

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
grammar[VARIABLE] = _.seq(
    _.matchDatum(datum => {
      const isIdentifier = datum instanceof Identifier;
      if (isIdentifier && isParserSensitiveId(
          (/** @type {!Identifier} */(datum)).getPayload())) {
        fixParserSensitiveIds_ = true;
      }
      return isIdentifier;
    }));

// <literal> -> <quotation> | <self-evaluating>
grammar[LITERAL] = _.choice(
    _.one(SELF_EVALUATING),
    _.one(QUOTATION));


// <quotation> -> '<datum> | (quote <datum>)
grammar[QUOTATION] = _.seq(
    // Terminals.QUOTE has already been canonicalized as Terminals.TICK
    // (see read.bnf.Seq_#maybeCanonicalize).
    _.one(TICK),
    _.one(DATUM));


grammar[DATUM] = _.seq(
    _.matchDatum(datum => true));


// <self-evaluating> -> <boolean> | <number> | <character> | <string>
grammar[SELF_EVALUATING] = _.seq(
    _.matchDatum(datum => {
      const ans = (datum instanceof SimpleDatum
          && !(datum instanceof Identifier))
          || datum instanceof Vector /* TODO bl document */;
      if (datum instanceof String) {
        // to defeat string-set! on a literal
        datum.setImmutable();
      }
      return ans;
    }));


// <procedure call> -> (<operator> <operand>*)
// <operator> -> <expression>
// <operand> -> <expression>
grammar[PROCEDURE_CALL] = _.list(
    _.one(OPERATOR),
    _.zeroOrMore(OPERAND)).
        desugar((node, env) => {
      const operatorNode = node.at(OPERATOR);
      // will be null if 0 operands
      const operands = node.at(OPERAND);

      if (operatorNode instanceof Identifier) {
        return new ProcCall(operatorNode, operands);
      }

    // Example: ((f x) y) => (f x [_0 (_0 y [_1 ...])])
      else {
        const desugaredOp = /** @type {!ProcCallLike} */ (
            operatorNode.desugar(env));
        const last = getLastProcCallLike(desugaredOp);
        const opName = last.getResultName();
        last.setNext(new ProcCall(
            new Identifier(opName), operands));
        return desugaredOp;
      }
        });


grammar[OPERATOR] = _.one(EXPRESSION);


grammar[OPERAND] = _.one(EXPRESSION);


// <lambda expression> -> (lambda <formals> <body>)
// <body> -> <definition>* <sequence>
// <sequence> -> <command>* <expression>
// <command> -> <expression>
grammar[LAMBDA_EXPRESSION] = _.list(
    _.one(LAMBDA),
    _.one(FORMALS),
    _.zeroOrMore(DEFINITION),
    _.oneOrMore(EXPRESSION)).
        desugar((node, env) => {
      const formalRoot = node.at(FORMALS);
      let formals;
      let treatAsDotted = false;

      // (lambda (x y) ...)
      if (formalRoot instanceof List) {
        formals = formalRoot.mapChildren(child =>
             /** @type {!Datum} */ (
              (/** @type {!SimpleDatum} */ (child)).getPayload()));
      }

    // (lambda (x y z . w) ...)
      else if (formalRoot.isImproperList()) {
        formals = (/** @type {!CompoundDatum} */ (formalRoot)).
            mapChildren(child =>
                /** @type {!Datum} */ (
                  (/** @type {!SimpleDatum} */ (child)).getPayload()));
        treatAsDotted = true;
      }

    /* (lambda <variable> <body>)
             R5RS 4.1.4:
             "The procedure takes any number of arguments; when the procedure
             is called, the sequence of actual arguments is converted into a
             newly allocated list, and the list is stored in the binding of the
             <variable>." */
      else {
        formals = [(/** @type {!SimpleDatum} */(formalRoot)).
                  getPayload()];
        treatAsDotted = true;
      }

      const name = newAnonymousLambdaName();
      const proc = treatAsDotted
          ? new VarargsUserDefinedProcedure(
          formals, formalRoot.getNextSibling(), env, name)
          : new UserDefinedProcedure(
              formals, formalRoot.getNextSibling(), env, name);
      env.addClosure(name, proc);
      return new Identifier(name).toProcCallLike();
        });


/* Why are there no <body> or <sequence> nonterminals?
 Because there is no datum associated with those nonterminals.
 For example, in (lambda () (define x 1) x), the text of <body>
 is (define x 1) x, which is not a datum.

 We could of course change the datum tree to accommodate this -- perhaps
 most easily by inserting a vacuous parent node. But as I understand it, the
 whole purpose of keeping the datum tree around during parsing is to make
 on-the-fly reinterpretation of the datum tree easy. For example, perhaps we
 parsed (foo x y) as a procedure call and now it needs to be reparsed as a
 macro use. If we had made changes to the datum tree, we might have to undo
 them now.

 I think that the only nonterminals that don't correspond to datums are:

 <body> -> <definition>* <sequence>
 <sequence> -> <command>* <expression>
 <def formals> -> <variable>* | <variable>* . <variable>
 <program> -> <command or definition>*

 So I decided to modify the grammar to replace these nonterminals by their
 RHSes.

 */

// <formals> -> (<variable>*) | <variable> | (<variable>+ . <variable>)
grammar[FORMALS] = _.choice(
    _.list(_.zeroOrMore(VARIABLE)),
    _.one(VARIABLE),
    _.dottedList(
        _.oneOrMore(VARIABLE),
        _.one(VARIABLE)));


/**
 * <definition> -> (define <variable> <expression>)
 * | (define (<variable> <def formals>) <body>)
 * | (begin <definition>*)
 * | <def formals> -> <variable>* | <variable>* . <variable>
 */
grammar[DEFINITION] = _.choice(
    _.list(
        _.one(DEFINE),
        _.one(VARIABLE),
        _.one(EXPRESSION)).desugar((node, env) => {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      const variable = /** @type {!SimpleDatum} */ (node.at(VARIABLE));
      const desugaredExpr = /** @type {!ProcCallLike} */ (variable.getNextSibling().desugar(env, true));
      const last = getLastProcCallLike(desugaredExpr);
      const cpsName = last.getResultName();
      last.setNext(
          TopLevelAssignment.of(variable.getPayload(), cpsName));
      return desugaredExpr;
    }),
    _.list(
        _.one(DEFINE),
        _.list(_.oneOrMore(VARIABLE)),
        _.zeroOrMore(DEFINITION),
        _.oneOrMore(EXPRESSION)).desugar((node, env) => {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      const def = extractDefinition(node);
      const name = /** @type {!SimpleDatum} */ (def.getFirstChild());
      const lambda = /** @type {!CompoundDatum} */(name.getNextSibling());
      const formalRoot = /** @type {!CompoundDatum} */ (lambda.getFirstChild().getNextSibling());
      const formals = formalRoot.mapChildren(child => (/** @type {!SimpleDatum} */(child)).getPayload());
      const anonymousName = newAnonymousLambdaName();
      env.addBinding(
          anonymousName,
          new UserDefinedProcedure(
              formals, formalRoot.getNextSibling(), env, name.getPayload()));
      return TopLevelAssignment.of(name.getPayload(), anonymousName);
    }),
    _.list(
        _.one(DEFINE),
        _.dottedList(
            _.oneOrMore(VARIABLE),
            _.one(VARIABLE)),
        _.zeroOrMore(DEFINITION),
        _.oneOrMore(EXPRESSION)).desugar((node, env) => {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      const def = extractDefinition(node);
      const name = /** @type {!SimpleDatum} */ (def.getFirstChild());
      const lambda = /** @type {!CompoundDatum} */ (name.getNextSibling());
      const formalRoot = lambda.getFirstChild().getNextSibling();
      const formals = formalRoot instanceof CompoundDatum
          ? formalRoot.mapChildren(child => (/** @type {!SimpleDatum} */(child)).getPayload())
          : [(/** @type {!SimpleDatum} */(formalRoot)).getPayload()];
      const anonymousName = newAnonymousLambdaName();
      env.addBinding(
          anonymousName,
          new VarargsUserDefinedProcedure(
              formals, formalRoot.getNextSibling(), env, name.getPayload()));
      return TopLevelAssignment.of(
          /** @type {string} */(name.getPayload()), // TODO bl
          anonymousName);
    }),
    _.list(
        _.one(BEGIN),
        _.zeroOrMore(DEFINITION)).
    desugar((node, env) => {
      const def = node.at(DEFINITION);
      return def && def.sequence(env);
        }));


// <conditional> -> (if <test> <consequent> <alternate>)
grammar[CONDITIONAL] = _.choice(
    _.list(
        _.one(IF),
        _.one(TEST),
        _.one(CONSEQUENT),
        _.one(ALTERNATE)).
    desugar((node, env) => {
      const test = /** @type {!ProcCallLike} */ (
          node.at(TEST).desugar(env, true));
      const consequent = /** @type {!ProcCall} */ (
          node.at(CONSEQUENT).desugar(env, true));
      const alternate = /** @type {!ProcCall} */ (
          node.at(ALTERNATE).desugar(env, true));
      const testEndpoint = getLastProcCallLike(test);
      const branch = new Branch(testEndpoint.getResultName(),
          consequent, alternate);
      testEndpoint.setNext(branch);
      return test;
    }),
    _.list(
        _.one(IF),
        _.one(TEST),
        _.one(CONSEQUENT)).desugar((node, env) => {
      const test = /** @type {!ProcCallLike} */ (
          node.at(TEST).desugar(env, true));
      const consequent = /** @type {!ProcCall} */ (
          node.at(CONSEQUENT).desugar(env, true));
      const testEndpoint = getLastProcCallLike(test);
      const branch = new Branch(
          testEndpoint.getResultName(),
          consequent,
          (/** @type {!Datum} */(UNSPECIFIED_VALUE)).toProcCallLike());
      testEndpoint.setNext(branch);
      return test;
    }));


// <test> -> <expression>
grammar[TEST] = _.one(EXPRESSION);


// <consequent> -> <expression>
grammar[CONSEQUENT] = _.one(EXPRESSION);


// <alternate> -> <expression> | <empty>
grammar[ALTERNATE] = _.one(EXPRESSION);


// <assignment> -> (set! <variable> <expression>)
grammar[ASSIGNMENT] = _.list(
    _.one(SET),
    _.one(VARIABLE),
    _.one(EXPRESSION)).
        desugar((node, env) => {
      // (set! x (+ y z)) => (+ y z [_0 (set! x _0 ...)])
      const variable = /** @type {!SimpleDatum} */ (node.at(VARIABLE));
      const desugaredExpr = /** @type {!ProcCallLike} */ (
          variable.getNextSibling().desugar(env, true));
      const lastContinuable = getLastProcCallLike(desugaredExpr);
      const cpsName = lastContinuable.getResultName();
      lastContinuable.setNext(Assignment.create(
          /** @type {string} */ (variable.getPayload()), cpsName));
      return desugaredExpr;
        });


// <quasiquotation> -> <quasiquotation 1>
// <quasiquotation D> -> `<qq template D> | (quasiquote <qq template D>)
grammar[QUASIQUOTATION] = _.seq(
    // Terminals.QUASIQUOTE has already been canonicalized as
    // Terminals.BACKTICK (see read.bnf.Seq_#maybeCanonicalize)
    _.one(BACKTICK),
    _.one(QQ_TEMPLATE));


/* <qq template 0> -> <expression>
 <qq template D> -> <simple datum>
 | <list qq template D>
 | <vector qq template D>
 | <unquotation D>
 */
grammar[QQ_TEMPLATE] = _.choice(
    _.matchDatum(datum => datum instanceof SimpleDatum),
    _.one(LIST_QQ_TEMPLATE),
    _.one(VECTOR_QQ_TEMPLATE),
    _.one(UNQUOTATION));


/*<list qq template D> -> (<qq template or splice D>*)
 | (<qq template or splice D>+ . <qq template D>)
 | '<qq template D>
 | <quasiquotation D+1>
 */
grammar[LIST_QQ_TEMPLATE] = _.choice(
    _.list(_.zeroOrMore(QQ_TEMPLATE_OR_SPLICE)),
    _.dottedList(
        _.oneOrMore(QQ_TEMPLATE_OR_SPLICE),
        _.one(QQ_TEMPLATE_OR_SPLICE)),
    _.seq(
        _.one(TICK),
        _.one(QQ_TEMPLATE)),
    _.one(QUASIQUOTATION));


// <vector qq template D> -> #(<qq template or splice D>*)
grammar[VECTOR_QQ_TEMPLATE] =
    _.vector(
        _.zeroOrMore(QQ_TEMPLATE_OR_SPLICE));


// <unquotation D> -> ,<qq template D-1> | (unquote <qq template D-1>)
grammar[UNQUOTATION] = _.seq(
    // Terminals.QUOTE has already been canonicalized as Terminals.COMMA
    // (see read.bnf.Seq_.#maybeCanonicalize).
    _.one(COMMA),
    _.one(QQ_TEMPLATE));


// <qq template or splice D> -> <qq template D> | <splicing unquotation D>
grammar[QQ_TEMPLATE_OR_SPLICE] = _.choice(
    _.seq(_.one(QQ_TEMPLATE)), // TODO bl one-element sequence
    _.one(SPLICING_UNQUOTATION));


/* <splicing unquotation D> -> ,@<qq template D-1>
 | (unquote-splicing <qq template D-1>)
 */
grammar[SPLICING_UNQUOTATION] = _.seq(
    // Terminals.UNQUOTE_SPLICING has already been canonicalized as
    // Terminals.COMMA_AT (see read.bnf.Seq_#maybeCanonicalize).
    _.one(COMMA_AT),
    _.one(QQ_TEMPLATE));

// <macro use> -> (<keyword> <datum>*)
grammar[MACRO_USE] = _.list(
    _.one(KEYWORD),
    _.zeroOrMore(DATUM)).
        desugar((node, env) => {
      /* Desugaring of a macro use is trivial. We must leave the "argument"
                datums as-is for the macro pattern matching facility to use.
                The trampoline knows what to do with raw datums in such a
                context. */
      return new ProcCall(
          /** @type {!Identifier} */ (node.at(KEYWORD)),
          node.at(DATUM));
    });


// <keyword> -> <identifier>
grammar[KEYWORD] = _.seq(
    _.matchDatum(datum => datum instanceof Identifier));


/* <macro block> -> (let-syntax (<syntax spec>*) <body>)
 | (letrec-syntax (<syntax-spec>*) <body>) */
grammar[MACRO_BLOCK] = _.choice(
    _.list(
        _.one(LET_SYNTAX),
        _.list(_.zeroOrMore(SYNTAX_SPEC)),
        _.zeroOrMore(DEFINITION),
        _.oneOrMore(EXPRESSION)).
    desugar((node, env) => desugarMacroBlock(node, env, 'let')),
    _.list(
        _.one(LETREC_SYNTAX),
        _.list(_.zeroOrMore(SYNTAX_SPEC)),
        _.zeroOrMore(DEFINITION),
        _.oneOrMore(EXPRESSION)).
    desugar((node, env) => desugarMacroBlock(node, env, 'letrec')));


// <syntax spec> -> (<keyword> <transformer spec>)
grammar[SYNTAX_SPEC] = _.list(
    _.one(KEYWORD),
    _.one(TRANSFORMER_SPEC));


// <transformer spec> -> (syntax-rules (<identifier>*) <syntax rule>*)
grammar[TRANSFORMER_SPEC] = _.list(
    _.one(SYNTAX_RULES),
    _.list(_.zeroOrMore(PATTERN_IDENTIFIER)),
    _.zeroOrMore(SYNTAX_RULE)).
        desugar((node, env) => {
      /*4.3.2: It is an error for ... to appear in <literals>.
                So we can reuse the pattern-identifier nonterminal
                to check this in the parser. Win! */
      const ids = (/** @type {!CompoundDatum} */ (node)).firstSublist().
          at(PATTERN_IDENTIFIER);
      const rules = node.at(SYNTAX_RULE);
      // todo bl implement: It is an error for the same pattern
      // variable to appear more than once in a <pattern>.
      return new Macro(ids, /** @type {!Datum} */(rules), env);
        });


// <syntax rule> -> (<pattern> <template>)
grammar[SYNTAX_RULE] = _.list(
    _.one(PATTERN),
    _.one(TEMPLATE));


/* <pattern> -> <pattern identifier>
 | (<pattern>*)
 | (<pattern>+ . <pattern>)
 | (<pattern>+ <ellipsis>)
 | #(<pattern>*)
 | #(<pattern>+ <ellipsis>)
 | <pattern datum>
 */
grammar[PATTERN] = _.choice(
    _.list(
        _.oneOrMore(PATTERN),
        _.one(ELLIPSIS)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.list();
      for (let cur = node.at(PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof SimpleDatum &&
            nextSibling.getPayload() === ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!Subtransformer} */ (cur.desugar(env))));
          break;
        } else {
          ans.addSubtransformer(/** @type {!Subtransformer} */ (
              cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.vector(
        _.oneOrMore(PATTERN),
        _.one(ELLIPSIS)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.vector();
      for (let cur = node.at(PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!Subtransformer} */ (cur.desugar(env))));
          break;
        } else {
          ans.addSubtransformer(
              /** @type {!Subtransformer} */ (cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.one(PATTERN_IDENTIFIER).desugar(node =>
      MacroIdTransformer.pattern(/** @type {!SimpleDatum} */ (node))),
    _.list(_.zeroOrMore(PATTERN)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.list();
      for (let cur = node.at(PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        ans.addSubtransformer(/** @type {!Subtransformer} */(
            cur.desugar(env)));
      }
      return ans;
    }),
    _.seq(
        _.one(LPAREN),
        _.oneOrMore(PATTERN),
        _.one(DOT),
        _.one(PATTERN),
        _.one(RPAREN)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.dottedList();
      for (let cur = node.at(PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        ans.addSubtransformer(/** @type {!Subtransformer} */(
            cur.desugar(env)));
      }
      return ans;
    }),
    _.vector(_.zeroOrMore(PATTERN)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.vector();
      for (let cur = node.at(PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        ans.addSubtransformer(/** @type {!Subtransformer} */ (
            cur.desugar(env)));
      }
      return ans;
    }),
    _.one(PATTERN_DATUM).desugar(node =>
      MacroIdTransformer.pattern(/** @type {!SimpleDatum} */ (node))));

// <pattern datum> -> <string> | <character> | <boolean> | <number>
grammar[PATTERN_DATUM] = _.seq(
    _.matchDatum(datum => datum instanceof SimpleDatum && !(datum instanceof Identifier)));


/* <template> -> <pattern identifier>
 | (<template element>*)
 | (<template element>+ . <template>)
 | #(<template element>*)
 | <template datum>
 <template element> -> <template> | <template> <ellipsis>

 The reader does not support (X+ . Y) where X != Y.
 (Internally, it converts this into something like .(X+), so it just keeps
 looking for X's.) The rule <template> -> (<template element>+ . <template>)
 appears to be the only part of the grammar where this occurs. So I have
 changed the rules for <template> to the following, which I believe is
 equivalent:

 <template> -> <pattern identifier>
 | (<template>*)
 | (<template>+ . <template>)
 | #(<template element>*)
 | <template datum>
 | <ellipsis>

 Anyway, the rules for validating templates with ellipses in them are vague
 (4.3.2: "It is an error if the output cannot be built up [from the template]
 as specified") and I can do this during evaluation of a macro if necessary. */
grammar[TEMPLATE] = _.choice(
    _.one(PATTERN_IDENTIFIER).desugar(node =>
      MacroIdTransformer.template(/** @type {!SimpleDatum} */(node))),
    _.seq(_.one(ELLIPSIS)), // TODO bl one-element sequence
    _.one(TEMPLATE_DATUM).desugar(node =>
      MacroIdTransformer.template(/** @type {!SimpleDatum} */ (node))),
    _.dottedList(
        _.oneOrMore(TEMPLATE),
        _.one(TEMPLATE)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.dottedList();
      for (let cur = node.at(TEMPLATE);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!Subtransformer} */ (cur.desugar(env))));
          cur = cur.getNextSibling();
        } else {
          ans.addSubtransformer(/** @type {!Subtransformer} */(
              cur.desugar(env)));
        }
      }

      return ans;
    }),
    _.list(_.zeroOrMore(TEMPLATE)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.list();
      for (let cur = node.at(TEMPLATE);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!Subtransformer} */ (cur.desugar(env))));
          cur = cur.getNextSibling();
        } else {
          ans.addSubtransformer(/** @type {!Subtransformer} */(
              cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.vector(_.zeroOrMore(TEMPLATE)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.vector();
      for (let cur = node.at(TEMPLATE);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!Subtransformer} */ (cur.desugar(env))));
          cur = cur.getNextSibling();
        } else {
          ans.addSubtransformer(/** @type {!Subtransformer} */ (
              cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.seq(
        _.one(TICK),
        _.one(TEMPLATE)).
    desugar((node, env) => {
      const ans = ListLikeTransformer.quote();
      ans.addSubtransformer(/** @type {!Subtransformer} */ (
          node.at(TEMPLATE).desugar(env)));
      return ans;
    }));


// <template datum> -> <pattern datum>
grammar[TEMPLATE_DATUM] = _.one(
    PATTERN_DATUM);


// <pattern identifier> -> <any identifier except ...>
grammar[PATTERN_IDENTIFIER] = _.seq(
    _.matchDatum(datum => datum instanceof Identifier && datum.getPayload() !== ELLIPSIS));


// <program> -> <command or definition>*
grammar[PROGRAM] = _.seq(
    _.zeroOrMore(COMMAND_OR_DEFINITION)).
        desugar((node, env) =>
      // VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of a
      // special case that I don't care.
      node === VACUOUS_PROGRAM ? node : node.sequence(env));


/* <command or definition> -> <command>
 | <definition>
 | <syntax definition>
 | (begin <command or definition>*)
 */
grammar[COMMAND_OR_DEFINITION] = _.choice(
    _.one(DEFINITION),
    _.one(SYNTAX_DEFINITION),
    _.one(DEFINITION),
    _.one(SYNTAX_DEFINITION),
    _.list(
        _.one(BEGIN),
        _.zeroOrMore(COMMAND_OR_DEFINITION)).
    desugar((node, env) => {
      const firstCommand = node.at(COMMAND_OR_DEFINITION);
      return firstCommand && firstCommand.sequence(env);
        }),
    _.one(COMMAND));


// <command> -> <expression>
grammar[COMMAND] = _.one(
    EXPRESSION);


// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
grammar[SYNTAX_DEFINITION] = _.list(
    _.one(DEFINE_SYNTAX),
    _.one(KEYWORD),
    _.one(TRANSFORMER_SPEC)).
        desugar((node, env) => {
      const kw = (/** @type {!Identifier} */ (node.at(
          KEYWORD))).getPayload();
      const macro = /** @type {!Macro} */ (
          node.at(TRANSFORMER_SPEC).desugar(env));
      if (!macro.allPatternsBeginWith(kw))
        throw Error.macro(kw, 'all patterns must begin with ' + kw);
      const anonymousName = newAnonymousLambdaName();
      env.addBinding(anonymousName, macro);
      return TopLevelSyntaxAssignment.of(kw, anonymousName);
    });

/** @return {string} */
function newAnonymousLambdaName() {
    /* TODO bl: goog.getUid requires an object parameter, so this method
     creates a throwaway object. Requiring this function to take an object
     parameter could reduce garbage. */
    return 'proc' + goog.getUid(new Object());
}

/**
 * @param {?Datum} root
 * @return {?Datum}
 */
function maybeFixParserSensitiveIds(root) {
    if (!root || !fixParserSensitiveIds_) {
        return root;
    }
    fixParserSensitiveIds_ = false;
    const helper = new RenameHelper(null /* parent */);
    root.fixParserSensitiveIds(helper);
    return helper.wasUsed() ? new ParserImpl(root).parse() : root;
}

/**
 * R5RS 4.3.1: "Let-syntax and letrec-syntax are analogous to let and letrec,
 * but they bind syntactic keywords to macro transformers instead of binding
 * variables to locations that contain values."
 *
 * In this implementation, a macro is just another kind of object that can
 * be stored in an environment, so we reuse the existing let machinery.
 * For example:
 *
 * (let-syntax ((foo (syntax-rules () ((foo) 'hi)))) ...)
 *
 * desugars as
 *
 * (let ((foo [SchemeMacro object])) ...)
 *
 * We just need to be sure that the SchemeMacro object inserted directly
 * into the parse tree plays well when the tree is transcribed and reparsed.
 *
 * @param {!CompoundDatum} datum Datum to desugar.
 * @param {!IEnvironment} env TODO bl.
 * @param {string} operatorName TODO bl.
 * @return {!ProcCallLike}
 */
function desugarMacroBlock(datum, env, operatorName) {
    const letBindings = new SiblingBuffer();

    datum.firstSublist().forEachChild(spec => {
        spec = /** @type {!CompoundDatum} */ (spec); // TODO bl
        const kw = spec.at(KEYWORD).clone(null /* parent */);
        const macro = /** @type {!Macro} */ (
            spec.at(TRANSFORMER_SPEC).desugar(env));
        const buf = new SiblingBuffer();
        /* We have to wrap the SchemeMacro object in a Datum to get it into
         the parse tree. */
        buf.appendSibling(kw);
        buf.appendSibling(new MacroDatum(macro));
        letBindings.appendSibling(buf.toList(List));
    });

    const _let = new SiblingBuffer();
    _let.appendSibling(
        letBindings.toList(List)
    ).appendSibling(
            /** @type {!Datum} */(datum.firstSublist().getNextSibling()));

    return new ProcCall(new Identifier(operatorName), _let.toSiblings());
}

exports = {grammar, ParserImpl};
