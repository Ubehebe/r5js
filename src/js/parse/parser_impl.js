goog.module('r5js.ParserImpl');

const _ = goog.require('r5js.parse.bnf');
const Branch = goog.require('r5js.Branch');
const CompoundDatum = goog.require('r5js.ast.CompoundDatum');
const Datum = goog.require('r5js.Datum');
const DatumStream = goog.require('r5js.DatumStream');
const datumutil = goog.require('r5js.datumutil');
const DottedListTransformer = goog.require('r5js.DottedListTransformer');
const EllipsisTransformer = goog.require('r5js.EllipsisTransformer');
const error = goog.require('r5js.error');
const Identifier = goog.require('r5js.ast.Identifier');
const IEnvironment = goog.require('r5js.IEnvironment');
const ITransformer = goog.require('r5js.ITransformer');
const List = goog.require('r5js.ast.List');
const ListTransformer = goog.require('r5js.ListTransformer');
const Macro = goog.require('r5js.Macro');
const MacroDatum = goog.require('r5js.ast.Macro');
const newAssignment = goog.require('r5js.newAssignment');
const Nonterminal = goog.require('r5js.parse.Nonterminal');
const Nonterminals = goog.require('r5js.parse.Nonterminals');
const Number = goog.require('r5js.ast.Number');
const Parser = goog.require('r5js.Parser');
const PatternIdTransformer = goog.require('r5js.PatternIdTransformer');
const ProcCall = goog.require('r5js.ProcCall');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const QuoteTransformer = goog.require('r5js.QuoteTransformer');
const RenameHelper = goog.require('r5js.RenameHelper');
const RenameUtil = goog.require('r5js.RenameUtil');
const SiblingBuffer = goog.require('r5js.SiblingBuffer');
const SimpleDatum = goog.require('r5js.ast.SimpleDatum');
const String = goog.require('r5js.ast.String');
const TemplateIdTransformer = goog.require('r5js.TemplateIdTransformer');
const Terminals = goog.require('r5js.parse.Terminals');
const TopLevelAssignment = goog.require('r5js.TopLevelAssignment');
const TopLevelSyntaxAssignment = goog.require('r5js.TopLevelSyntaxAssignment');
const UNSPECIFIED_VALUE = goog.require('r5js.runtime.UNSPECIFIED_VALUE');
const UserDefinedProcedure = goog.require('r5js.UserDefinedProcedure');
const VACUOUS_PROGRAM = goog.require('r5js.VACUOUS_PROGRAM');
const VarargsUserDefinedProcedure = goog.require('r5js.VarargsUserDefinedProcedure');
const VectorTransformer = goog.require('r5js.VectorTransformer');
const Vector = goog.require('r5js.ast.Vector');

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

    /**
     * @param {!Nonterminal=} opt_nonterminal
     * @override TODO bl compiler bug
     */
    parse(opt_nonterminal) {
        const nonterminal = opt_nonterminal || Nonterminals.PROGRAM;
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
            if (nonterminal === Nonterminals.PROGRAM &&
                parsedRoot != VACUOUS_PROGRAM && !parsedRoot.peekParse()) {
                return null;
            }
            parsedRoot.setParse(nonterminal);
        }
        return (nonterminal === Nonterminals.PROGRAM)
            ? ParserImpl.maybeFixParserSensitiveIds_(parsedRoot)
            : parsedRoot;
    }

    /**
     * @param {Datum} root
     * @return {Datum}
     * @private
     */
    static maybeFixParserSensitiveIds_(root) {
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
     * @private
     */
    static desugarMacroBlock_(datum, env, operatorName) {
        const letBindings = new SiblingBuffer();

        datum.firstSublist().forEachChild(function (spec) {
            spec = /** @type {!CompoundDatum} */ (spec); // TODO bl
            const kw = spec.at(Nonterminals.KEYWORD).clone(null /* parent */);
            const macro = /** @type {!Macro} */ (
                spec.at(Nonterminals.TRANSFORMER_SPEC).desugar(env));
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
            /** @type {!Datum} */ (datum.firstSublist().getNextSibling()));

        return new ProcCall(new Identifier(operatorName), _let.toSiblings());
    }
}

    /** @private {boolean} */
let fixParserSensitiveIds_ = false;

/** @const {!Object<!Nonterminal, !_.Rule>} */
const grammar = {};

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
grammar[Nonterminals.EXPRESSION] =
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
    _.one(Nonterminals.VARIABLE),
    _.one(Nonterminals.LITERAL),
    _.one(Nonterminals.LAMBDA_EXPRESSION),
    _.one(Nonterminals.CONDITIONAL),
    _.one(Nonterminals.ASSIGNMENT),
    _.one(Nonterminals.QUASIQUOTATION).desugar(function(node, env) {
      return (/** @type {!CompoundDatum} */ (node)).
          setQuasiquotationLevel(1);
    }),
    _.list(
            _.one(Terminals.BEGIN),
            _.oneOrMore(Nonterminals.EXPRESSION)).
        desugar(function(node, env) {
      return node.at(Nonterminals.EXPRESSION).sequence(env);
        }),
    _.one(Nonterminals.MACRO_BLOCK),
    _.one(Nonterminals.PROCEDURE_CALL),
    _.one(Nonterminals.MACRO_USE));

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
grammar[Nonterminals.VARIABLE] = _.seq(
    _.matchDatum(function(datum) {
      const isIdentifier = datum instanceof Identifier;
      if (isIdentifier && RenameUtil.isParserSensitiveId(
          (/** @type {!Identifier} */(datum)).getPayload())) {
        fixParserSensitiveIds_ = true;
      }
      return isIdentifier;
    }));

// <literal> -> <quotation> | <self-evaluating>
grammar[Nonterminals.LITERAL] = _.choice(
    _.one(Nonterminals.SELF_EVALUATING),
    _.one(Nonterminals.QUOTATION));


// <quotation> -> '<datum> | (quote <datum>)
grammar[Nonterminals.QUOTATION] = _.seq(
    // Terminals.QUOTE has already been canonicalized as Terminals.TICK
    // (see read.bnf.Seq_#maybeCanonicalize).
    _.one(Terminals.TICK),
    _.one(Nonterminals.DATUM));


grammar[Nonterminals.DATUM] = _.seq(
    _.matchDatum(function(datum) {
      return true;
    }));


// <self-evaluating> -> <boolean> | <number> | <character> | <string>
grammar[Nonterminals.SELF_EVALUATING] = _.seq(
    _.matchDatum(function(datum) {
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
grammar[Nonterminals.PROCEDURE_CALL] = _.list(
    _.one(Nonterminals.OPERATOR),
    _.zeroOrMore(Nonterminals.OPERAND)).
        desugar(function(node, env) {

      const operatorNode = node.at(Nonterminals.OPERATOR);
      // will be null if 0 operands
      const operands = node.at(Nonterminals.OPERAND);

      if (operatorNode instanceof Identifier) {
        return new ProcCall(operatorNode, operands);
      }

    // Example: ((f x) y) => (f x [_0 (_0 y [_1 ...])])
      else {
        const desugaredOp = /** @type {!ProcCallLike} */ (
            operatorNode.desugar(env));
        const last = ProcCallLike.getLast(desugaredOp);
        const opName = last.getResultName();
        last.setNext(new ProcCall(
            new Identifier(opName), operands));
        return desugaredOp;
      }
        });


grammar[Nonterminals.OPERATOR] = _.one(
    Nonterminals.EXPRESSION);


grammar[Nonterminals.OPERAND] = _.one(
    Nonterminals.EXPRESSION);


// <lambda expression> -> (lambda <formals> <body>)
// <body> -> <definition>* <sequence>
// <sequence> -> <command>* <expression>
// <command> -> <expression>
grammar[Nonterminals.LAMBDA_EXPRESSION] = _.list(
    _.one(Terminals.LAMBDA),
    _.one(Nonterminals.FORMALS),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.oneOrMore(Nonterminals.EXPRESSION)).
        desugar(function(node, env) {
      const formalRoot = node.at(Nonterminals.FORMALS);
      let formals;
      let treatAsDotted = false;

      // (lambda (x y) ...)
      if (formalRoot instanceof List) {
        formals = formalRoot.mapChildren(function(child) {
          return /** @type {!Datum} */ (
              (/** @type {!SimpleDatum} */ (child)).getPayload());
            });
      }

    // (lambda (x y z . w) ...)
      else if (formalRoot.isImproperList()) {
        formals = (/** @type {!CompoundDatum} */ (formalRoot)).
            mapChildren(function(child) {
              return /** @type {!Datum} */ (
                  (/** @type {!SimpleDatum} */ (child)).getPayload());
            });
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

      const name = RenameUtil.newAnonymousLambdaName();
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
grammar[Nonterminals.FORMALS] = _.choice(
    _.list(_.zeroOrMore(Nonterminals.VARIABLE)),
    _.one(Nonterminals.VARIABLE),
    _.dottedList(
        _.oneOrMore(Nonterminals.VARIABLE),
        _.one(Nonterminals.VARIABLE)));


/**
 * <definition> -> (define <variable> <expression>)
 * | (define (<variable> <def formals>) <body>)
 * | (begin <definition>*)
 * | <def formals> -> <variable>* | <variable>* . <variable>
 */
grammar[Nonterminals.DEFINITION] = _.choice(
    _.list(
        _.one(Terminals.DEFINE),
        _.one(Nonterminals.VARIABLE),
        _.one(Nonterminals.EXPRESSION)).
    desugar(/** @suppress {checkTypes} */ function(node, env) {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      const variable = node.at(Nonterminals.VARIABLE);
      const desugaredExpr = variable.getNextSibling().desugar(env, true);
      const last = ProcCallLike.getLast(desugaredExpr);
      const cpsName = last.getResultName();
      last.setNext(
          TopLevelAssignment.of(variable.getPayload(), cpsName));
      return desugaredExpr;
    }),
    _.list(
        _.one(Terminals.DEFINE),
        _.list(_.oneOrMore(Nonterminals.VARIABLE)),
        _.zeroOrMore(Nonterminals.DEFINITION),
        _.oneOrMore(Nonterminals.EXPRESSION)).
    desugar(/** @suppress {checkTypes} */function(node, env) {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      const def = datumutil.extractDefinition(node);
      const name = def.getFirstChild();
      const lambda = name.getNextSibling();
      const formalRoot = lambda.getFirstChild().getNextSibling();
      const formals = formalRoot.mapChildren(function(child) {
        return child.getPayload();
      });
      const anonymousName = RenameUtil.newAnonymousLambdaName();
      env.addBinding(
          anonymousName,
          new UserDefinedProcedure(
              formals, formalRoot.getNextSibling(), env, name));
      return TopLevelAssignment.of(name.getPayload(), anonymousName);
    }),
    _.list(
        _.one(Terminals.DEFINE),
        _.dottedList(
            _.oneOrMore(Nonterminals.VARIABLE),
            _.one(Nonterminals.VARIABLE)),
        _.zeroOrMore(Nonterminals.DEFINITION),
        _.oneOrMore(Nonterminals.EXPRESSION)).
    desugar(/** @suppress {checkTypes} */function(node, env) {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      const def = datumutil.extractDefinition(node);
      const name = def.getFirstChild();
      const lambda = name.getNextSibling();
      const formalRoot = lambda.getFirstChild().getNextSibling();
      const formals = formalRoot instanceof CompoundDatum ?
          formalRoot.mapChildren(function(child) {
            return child.getPayload();
          }) :
          [formalRoot.getPayload()];
      const anonymousName = RenameUtil.newAnonymousLambdaName();
      env.addBinding(
          anonymousName,
          new VarargsUserDefinedProcedure(
              formals, formalRoot.getNextSibling(), env, name));
      return TopLevelAssignment.of(
          /** @type {string} */(name.getPayload()), // TODO bl
          anonymousName);
    }),
    _.list(
        _.one(Terminals.BEGIN),
        _.zeroOrMore(Nonterminals.DEFINITION)).
    desugar(function(node, env) {
      const def = node.at(Nonterminals.DEFINITION);
      return def && def.sequence(env);
        }));


// <conditional> -> (if <test> <consequent> <alternate>)
grammar[Nonterminals.CONDITIONAL] = _.choice(
    _.list(
        _.one(Terminals.IF),
        _.one(Nonterminals.TEST),
        _.one(Nonterminals.CONSEQUENT),
        _.one(Nonterminals.ALTERNATE)).
    desugar(function(node, env) {
      const test = /** @type {!ProcCallLike} */ (
          node.at(Nonterminals.TEST).desugar(env, true));
      const consequent = /** @type {!ProcCall} */ (
          node.at(Nonterminals.CONSEQUENT).desugar(env, true));
      const alternate = /** @type {!ProcCall} */ (
          node.at(Nonterminals.ALTERNATE).desugar(env, true));
      const testEndpoint = ProcCallLike.getLast(test);
      const branch = new Branch(testEndpoint.getResultName(),
          consequent, alternate);
      testEndpoint.setNext(branch);
      return test;
    }),
    _.list(
        _.one(Terminals.IF),
        _.one(Nonterminals.TEST),
        _.one(Nonterminals.CONSEQUENT)).
    desugar(/** @suppress {checkTypes} */function(node, env) {
      const test = /** @type {!ProcCallLike} */ (
          node.at(Nonterminals.TEST).desugar(env, true));
      const consequent = /** @type {!ProcCallLike} */ (
          node.at(Nonterminals.CONSEQUENT).desugar(env, true));

      const testEndpoint = ProcCallLike.getLast(test);
      const branch = new Branch(
          testEndpoint.getResultName(),
          consequent,
          UNSPECIFIED_VALUE.toProcCallLike());
      testEndpoint.setNext(branch);
      return test;
    }));


// <test> -> <expression>
grammar[Nonterminals.TEST] = _.one(
    Nonterminals.EXPRESSION);


// <consequent> -> <expression>
grammar[Nonterminals.CONSEQUENT] = _.one(
    Nonterminals.EXPRESSION);


// <alternate> -> <expression> | <empty>
grammar[Nonterminals.ALTERNATE] = _.one(
    Nonterminals.EXPRESSION);


// <assignment> -> (set! <variable> <expression>)
grammar[Nonterminals.ASSIGNMENT] = _.list(
    _.one(Terminals.SET),
    _.one(Nonterminals.VARIABLE),
    _.one(Nonterminals.EXPRESSION)).
        desugar(function(node, env) {
      // (set! x (+ y z)) => (+ y z [_0 (set! x _0 ...)])
      const variable = /** @type {!SimpleDatum} */ (
          node.at(Nonterminals.VARIABLE));
      const desugaredExpr = /** @type {!ProcCallLike} */ (
          variable.getNextSibling().desugar(env, true));
      const lastContinuable = ProcCallLike.getLast(desugaredExpr);
      const cpsName = lastContinuable.getResultName();
      lastContinuable.setNext(newAssignment(
          /** @type {string} */ (variable.getPayload()), cpsName));
      return desugaredExpr;
        });


// <quasiquotation> -> <quasiquotation 1>
// <quasiquotation D> -> `<qq template D> | (quasiquote <qq template D>)
grammar[Nonterminals.QUASIQUOTATION] = _.seq(
    // Terminals.QUASIQUOTE has already been canonicalized as
    // Terminals.BACKTICK (see read.bnf.Seq_#maybeCanonicalize)
    _.one(Terminals.BACKTICK),
    _.one(Nonterminals.QQ_TEMPLATE));


/* <qq template 0> -> <expression>
 <qq template D> -> <simple datum>
 | <list qq template D>
 | <vector qq template D>
 | <unquotation D>
 */
grammar[Nonterminals.QQ_TEMPLATE] = _.choice(
    _.matchDatum(function(datum) {
      return datum instanceof SimpleDatum;
    }),
    _.one(Nonterminals.LIST_QQ_TEMPLATE),
    _.one(Nonterminals.VECTOR_QQ_TEMPLATE),
    _.one(Nonterminals.UNQUOTATION));


/*<list qq template D> -> (<qq template or splice D>*)
 | (<qq template or splice D>+ . <qq template D>)
 | '<qq template D>
 | <quasiquotation D+1>
 */
grammar[Nonterminals.LIST_QQ_TEMPLATE] = _.choice(
    _.list(_.zeroOrMore(Nonterminals.QQ_TEMPLATE_OR_SPLICE)),
    _.dottedList(
        _.oneOrMore(Nonterminals.QQ_TEMPLATE_OR_SPLICE),
        _.one(Nonterminals.QQ_TEMPLATE_OR_SPLICE)),
    _.seq(
        _.one(Terminals.TICK),
        _.one(Nonterminals.QQ_TEMPLATE)),
    _.one(Nonterminals.QUASIQUOTATION));


// <vector qq template D> -> #(<qq template or splice D>*)
grammar[Nonterminals.VECTOR_QQ_TEMPLATE] =
    _.vector(
        _.zeroOrMore(Nonterminals.QQ_TEMPLATE_OR_SPLICE));


// <unquotation D> -> ,<qq template D-1> | (unquote <qq template D-1>)
grammar[Nonterminals.UNQUOTATION] = _.seq(
    // Terminals.QUOTE has already been canonicalized as Terminals.COMMA
    // (see read.bnf.Seq_.#maybeCanonicalize).
    _.one(Terminals.COMMA),
    _.one(Nonterminals.QQ_TEMPLATE));


// <qq template or splice D> -> <qq template D> | <splicing unquotation D>
grammar[Nonterminals.QQ_TEMPLATE_OR_SPLICE] = _.choice(
    _.seq(_.one(Nonterminals.QQ_TEMPLATE)), // TODO bl one-element sequence
    _.one(Nonterminals.SPLICING_UNQUOTATION));


/* <splicing unquotation D> -> ,@<qq template D-1>
 | (unquote-splicing <qq template D-1>)
 */
grammar[Nonterminals.SPLICING_UNQUOTATION] = _.seq(
    // Terminals.UNQUOTE_SPLICING has already been canonicalized as
    // Terminals.COMMA_AT (see read.bnf.Seq_#maybeCanonicalize).
    _.one(Terminals.COMMA_AT),
    _.one(Nonterminals.QQ_TEMPLATE));

// <macro use> -> (<keyword> <datum>*)
grammar[Nonterminals.MACRO_USE] = _.list(
    _.one(Nonterminals.KEYWORD),
    _.zeroOrMore(Nonterminals.DATUM)).
        desugar(function(node, env) {
      /* Desugaring of a macro use is trivial. We must leave the "argument"
                datums as-is for the macro pattern matching facility to use.
                The trampoline knows what to do with raw datums in such a
                context. */
      return new ProcCall(
          /** @type {!Identifier} */ (node.at(Nonterminals.KEYWORD)),
          node.at(Nonterminals.DATUM));
    });


// <keyword> -> <identifier>
grammar[Nonterminals.KEYWORD] = _.seq(
    _.matchDatum(function(datum) {
      return datum instanceof Identifier;
    }));


/* <macro block> -> (let-syntax (<syntax spec>*) <body>)
 | (letrec-syntax (<syntax-spec>*) <body>) */
grammar[Nonterminals.MACRO_BLOCK] = _.choice(
    _.list(
        _.one(Terminals.LET_SYNTAX),
        _.list(_.zeroOrMore(Nonterminals.SYNTAX_SPEC)),
        _.zeroOrMore(Nonterminals.DEFINITION),
        _.oneOrMore(Nonterminals.EXPRESSION)).
    desugar(function(node, env) {
      return ParserImpl.desugarMacroBlock_(node, env, 'let');
    }),
    _.list(
        _.one(Terminals.LETREC_SYNTAX),
        _.list(_.zeroOrMore(Nonterminals.SYNTAX_SPEC)),
        _.zeroOrMore(Nonterminals.DEFINITION),
        _.oneOrMore(Nonterminals.EXPRESSION)).
    desugar(function(node, env) {
      return ParserImpl.desugarMacroBlock_(node, env, 'letrec');
    }));


// <syntax spec> -> (<keyword> <transformer spec>)
grammar[Nonterminals.SYNTAX_SPEC] = _.list(
    _.one(Nonterminals.KEYWORD),
    _.one(Nonterminals.TRANSFORMER_SPEC));


// <transformer spec> -> (syntax-rules (<identifier>*) <syntax rule>*)
grammar[Nonterminals.TRANSFORMER_SPEC] = _.list(
    _.one(Terminals.SYNTAX_RULES),
    _.list(_.zeroOrMore(Nonterminals.PATTERN_IDENTIFIER)),
    _.zeroOrMore(Nonterminals.SYNTAX_RULE)).
        desugar(function(node, env) {
      /*4.3.2: It is an error for ... to appear in <literals>.
                So we can reuse the pattern-identifier nonterminal
                to check this in the parser. Win! */
      const ids = (/** @type {!CompoundDatum} */ (node)).firstSublist().
          at(Nonterminals.PATTERN_IDENTIFIER);
      const rules = node.at(Nonterminals.SYNTAX_RULE);
      // todo bl implement: It is an error for the same pattern
      // variable to appear more than once in a <pattern>.
      return new Macro(ids, /** @type {!Datum} */(rules), env);
        });


// <syntax rule> -> (<pattern> <template>)
grammar[Nonterminals.SYNTAX_RULE] = _.list(
    _.one(Nonterminals.PATTERN),
    _.one(Nonterminals.TEMPLATE));


/* <pattern> -> <pattern identifier>
 | (<pattern>*)
 | (<pattern>+ . <pattern>)
 | (<pattern>+ <ellipsis>)
 | #(<pattern>*)
 | #(<pattern>+ <ellipsis>)
 | <pattern datum>
 */
grammar[Nonterminals.PATTERN] = _.choice(
    _.list(
        _.oneOrMore(Nonterminals.PATTERN),
        _.one(Terminals.ELLIPSIS)).
    desugar(function(node, env) {
      const ans = new ListTransformer();
      for (let cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof SimpleDatum &&
            nextSibling.getPayload() === Terminals.ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!ITransformer} */ (cur.desugar(env))));
          break;
        } else {
          ans.addSubtransformer(/** @type {!ITransformer} */ (
              cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.vector(
        _.oneOrMore(Nonterminals.PATTERN),
        _.one(Terminals.ELLIPSIS)).
    desugar(function(node, env) {
      const ans = new VectorTransformer();
      for (let cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === Terminals.ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!ITransformer} */ (cur.desugar(env))));
          break;
        } else {
          ans.addSubtransformer(
              /** @type {!ITransformer} */ (cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.one(Nonterminals.PATTERN_IDENTIFIER).desugar(function(node) {
      return new PatternIdTransformer(
          /** @type {!SimpleDatum} */ (node));
    }),
    _.list(_.zeroOrMore(Nonterminals.PATTERN)).
    desugar(function(node, env) {
      const ans = new ListTransformer();
      for (let cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        ans.addSubtransformer(/** @type {!ITransformer} */(
            cur.desugar(env)));
      }
      return ans;
    }),
    _.seq(
        _.one(Terminals.LPAREN),
        _.oneOrMore(Nonterminals.PATTERN),
        _.one(Terminals.DOT),
        _.one(Nonterminals.PATTERN),
        _.one(Terminals.RPAREN)).
    desugar(function(node, env) {
      const ans = new DottedListTransformer();
      for (let cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        ans.addSubtransformer(/** @type {!ITransformer} */(
            cur.desugar(env)));
      }
      return ans;
    }),
    _.vector(_.zeroOrMore(Nonterminals.PATTERN)).
    desugar(function(node, env) {
      const ans = new VectorTransformer();
      for (let cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.getNextSibling()) {
        ans.addSubtransformer(/** @type {!ITransformer} */ (
            cur.desugar(env)));
      }
      return ans;
    }),
    _.one(Nonterminals.PATTERN_DATUM).desugar(function(node) {
      return new PatternIdTransformer(
          /** @type {!SimpleDatum} */ (node));
    }));


// <pattern datum> -> <string> | <character> | <boolean> | <number>
grammar[Nonterminals.PATTERN_DATUM] = _.seq(
    _.matchDatum(function(datum) {
      return datum instanceof SimpleDatum &&
          !(datum instanceof Identifier);
    }));


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
grammar[Nonterminals.TEMPLATE] = _.choice(
    _.one(Nonterminals.PATTERN_IDENTIFIER).desugar(function(node) {
      return new TemplateIdTransformer(
          /** @type {!SimpleDatum} */(node));
    }),
    _.seq(_.one(Terminals.ELLIPSIS)), // TODO bl one-element sequence
    _.one(Nonterminals.TEMPLATE_DATUM).desugar(function(node) {
      return new TemplateIdTransformer(
          /** @type {!SimpleDatum} */ (node));
    }),
    _.dottedList(
        _.oneOrMore(Nonterminals.TEMPLATE),
        _.one(Nonterminals.TEMPLATE)).
    desugar(function(node, env) {
      const ans = new DottedListTransformer();
      for (let cur = node.at(Nonterminals.TEMPLATE);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === Terminals.ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!ITransformer} */ (cur.desugar(env))));
          cur = cur.getNextSibling();
        } else {
          ans.addSubtransformer(/** @type {!ITransformer} */(
              cur.desugar(env)));
        }
      }

      return ans;
    }),
    _.list(_.zeroOrMore(Nonterminals.TEMPLATE)).
    desugar(function(node, env) {
      const ans = new ListTransformer();
      for (let cur = node.at(Nonterminals.TEMPLATE);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === Terminals.ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!ITransformer} */ (cur.desugar(env))));
          cur = cur.getNextSibling();
        } else {
          ans.addSubtransformer(/** @type {!ITransformer} */(
              cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.vector(_.zeroOrMore(Nonterminals.TEMPLATE)).
    desugar(function(node, env) {
      const ans = new VectorTransformer();
      for (let cur = node.at(Nonterminals.TEMPLATE);
           cur;
           cur = cur.getNextSibling()) {
        const nextSibling = cur.getNextSibling();
        if (nextSibling instanceof Identifier &&
            nextSibling.getPayload() === Terminals.ELLIPSIS) {
          ans.addSubtransformer(
              new EllipsisTransformer(
                  /** @type {!ITransformer} */ (cur.desugar(env))));
          cur = cur.getNextSibling();
        } else {
          ans.addSubtransformer(/** @type {!ITransformer} */ (
              cur.desugar(env)));
        }
      }
      return ans;
    }),
    _.seq(
        _.one(Terminals.TICK),
        _.one(Nonterminals.TEMPLATE)).
    desugar(function(node, env) {
      const ans = new QuoteTransformer();
      ans.addSubtransformer(/** @type {!ITransformer} */ (
          node.at(Nonterminals.TEMPLATE).desugar(env)));
      return ans;
    }));


// <template datum> -> <pattern datum>
grammar[Nonterminals.TEMPLATE_DATUM] = _.one(
    Nonterminals.PATTERN_DATUM);


// <pattern identifier> -> <any identifier except ...>
grammar[Nonterminals.PATTERN_IDENTIFIER] = _.seq(
    _.matchDatum(function(datum) {
      return datum instanceof Identifier &&
          datum.getPayload() !== Terminals.ELLIPSIS;
    }));


// <program> -> <command or definition>*
grammar[Nonterminals.PROGRAM] = _.seq(
    _.zeroOrMore(Nonterminals.COMMAND_OR_DEFINITION)).
        desugar(function(node, env) {
      // VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of a
      // special case that I don't care.
      return node === VACUOUS_PROGRAM ? node : node.sequence(env);
    });


/* <command or definition> -> <command>
 | <definition>
 | <syntax definition>
 | (begin <command or definition>*)
 */
grammar[Nonterminals.COMMAND_OR_DEFINITION] = _.choice(
    _.one(Nonterminals.DEFINITION),
    _.one(Nonterminals.SYNTAX_DEFINITION),
    _.one(Nonterminals.DEFINITION),
    _.one(Nonterminals.SYNTAX_DEFINITION),
    _.list(
        _.one(Terminals.BEGIN),
        _.zeroOrMore(Nonterminals.COMMAND_OR_DEFINITION)).
    desugar(function(node, env) {
      const firstCommand = node.at(Nonterminals.COMMAND_OR_DEFINITION);
      return firstCommand && firstCommand.sequence(env);
        }),
    _.one(Nonterminals.COMMAND));


// <command> -> <expression>
grammar[Nonterminals.COMMAND] = _.one(
    Nonterminals.EXPRESSION);


// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
grammar[Nonterminals.SYNTAX_DEFINITION] = _.list(
    _.one(Terminals.DEFINE_SYNTAX),
    _.one(Nonterminals.KEYWORD),
    _.one(Nonterminals.TRANSFORMER_SPEC)).
        desugar(function(node, env) {
      const kw = (/** @type {!Identifier} */ (node.at(
          Nonterminals.KEYWORD))).getPayload();
      const macro = /** @type {!Macro} */ (
          node.at(Nonterminals.TRANSFORMER_SPEC).desugar(env));
      if (!macro.allPatternsBeginWith(kw))
        throw error.macro(kw, 'all patterns must begin with ' + kw);
      const anonymousName = RenameUtil.newAnonymousLambdaName();
      env.addBinding(anonymousName, macro);
      return TopLevelSyntaxAssignment.of(kw, anonymousName);
    });

exports.ParserImpl = ParserImpl;
exports.grammar = grammar;

