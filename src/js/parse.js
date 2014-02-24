/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


goog.provide('r5js.Parser');

goog.require('goog.array');
goog.require('r5js.Continuation');
goog.require('r5js.Datum');
goog.require('r5js.DatumStreamImpl');
goog.require('r5js.DatumType');
goog.require('r5js.EllipsisTransformer');
goog.require('r5js.IdOrLiteralTransformer');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ListLikeTransformer');
goog.require('r5js.Macro');
goog.require('r5js.MacroError');
goog.require('r5js.Procedure');
goog.require('r5js.RenameHelper');
goog.require('r5js.data');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.parse.bnf');
goog.require('r5js.procs');


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



/**
 * @param {!r5js.Datum} root The root of the tree to parse.
 * @implements {r5js.IParser}
 * @constructor
 */
r5js.Parser = function(root) {
  /** @const @private {!r5js.DatumStream} */
  this.datumStream_ = new r5js.DatumStreamImpl(root);
};


/**
 * @param {!r5js.parse.Nonterminal=} opt_nonterminal
 * @return {r5js.Datum}
 */
r5js.Parser.prototype.parse = function(opt_nonterminal) {
  // TODO bl: unify these two cases.
  if (goog.isDef(opt_nonterminal)) {
    var root = this.datumStream_.getNextDatum();
    if (!r5js.parse.bnf.one(opt_nonterminal).
        match(this.datumStream_)) {
      /* This check is necessary because root may be the special
             sentinel object for empty lists. */
      if (root instanceof r5js.Datum)
        root.unsetParse();
      this.datumStream_.advanceTo(/** @type {!r5js.Datum} */ (root));
      return null;
    }
    var nextSibling = /** just in case of an empty program */ root &&
        root.nextSibling;
    this.datumStream_.advanceTo(/** @type {!r5js.Datum} */ (nextSibling));
    return root;
  } else {
    var ans = r5js.Parser.grammar[r5js.parse.Nonterminals.PROGRAM].
        match(this.datumStream_);
    if (ans instanceof r5js.Datum && ans.nonterminals) {
      // See comments at top of Parser.
      if (r5js.Parser.fixParserSensitiveIds_) {
        r5js.Parser.fixParserSensitiveIds_ = false;
        var helper = new r5js.RenameHelper(null);
        ans.fixParserSensitiveIds(helper);
        if (helper.wasUsed()) {
          /* todo bl inefficient, but i've had errors fusing this
                     into fixParserSensitiveIds() */
          for (var cur = ans; cur; cur = cur.nextSibling)
            cur.unsetParse();
          return new r5js.Parser(ans).parse(opt_nonterminal);
        } else return ans;
      } else return ans;
    } else {
      /* Do not return a node if its nonterminals haven't been set;
             this means parsing failed. Exception: if an lhs was passed in,
             this was for debugging, and we want to present whatever we
             finished with. */
      return goog.isDef(opt_nonterminal) ?
          /** @type {r5js.Datum} */(ans) :
          null;
    }
  }
};


/** @private {boolean} */
r5js.Parser.fixParserSensitiveIds_;


/** @const {!Object.<!r5js.parse.Nonterminal, !r5js.parse.bnf.Rule>} */
r5js.Parser.grammar = {};


goog.scope(function() {
var _ = r5js.parse.bnf;
var Terminals = r5js.parse.Terminals;
var Nonterminals = r5js.parse.Nonterminals;


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
r5js.Parser.grammar[Nonterminals.EXPRESSION] =
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
     JavaScript. That way, the syntactic keyword could be shadowed
     appropriately. */
    _.choice(
        _.seq(
            _.one(Nonterminals.VARIABLE)),
        _.seq(
            _.one(Nonterminals.LITERAL)),
        _.seq(
            _.one(Nonterminals.LAMBDA_EXPRESSION)),
        _.seq(
            _.one(Nonterminals.CONDITIONAL)),
        _.seq(
            _.one(Nonterminals.ASSIGNMENT)),
        _.seq(
            _.one(Nonterminals.QUASIQUOTATION)).
            desugar(function(node, env) {
      return node.normalizeInput().decorateQuasiquote(1);
            }),
        _.seq(
            _.one(Terminals.LPAREN),
            _.one(Terminals.BEGIN),
            _.oneOrMore(Nonterminals.EXPRESSION),
            _.one(Terminals.RPAREN)),
        _.seq(
            _.one(Nonterminals.MACRO_BLOCK)),
        _.seq(
            _.one(Nonterminals.PROCEDURE_CALL)),
        _.seq(
            _.one(Nonterminals.MACRO_USE)));

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
r5js.Parser.grammar[Nonterminals.VARIABLE] = _.seq(
    _.matchDatum(function(datum) {
      // because it may be emptyListSentinel
      var ans = datum instanceof r5js.Datum && datum.isIdentifier();
      if (ans && isParserSensitiveId(/** @type {string} */ (datum.payload))) {
        r5js.Parser.fixParserSensitiveIds_ = true;
      }
      return ans;
    }));

// <literal> -> <quotation> | <self-evaluating>
r5js.Parser.grammar[Nonterminals.LITERAL] = _.choice(
    _.one(Nonterminals.SELF_EVALUATING),
    _.one(Nonterminals.QUOTATION));


// <quotation> -> '<datum> | (quote <datum>)
r5js.Parser.grammar[Nonterminals.QUOTATION] = _.choice(
    _.seq(
    _.one(Terminals.TICK),
    _.one(Nonterminals.DATUM)).
    desugar(function(node, env) {
      return node.normalizeInput();
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.QUOTE),
    _.one(Nonterminals.DATUM),
    _.one(Terminals.RPAREN)).
    desugar(function(node, env) {
      return node.normalizeInput();
    }));


r5js.Parser.grammar[Nonterminals.DATUM] = _.seq(
    _.matchDatum(function(datum) {
      return true;
    }));


// <self-evaluating> -> <boolean> | <number> | <character> | <string>
r5js.Parser.grammar[Nonterminals.SELF_EVALUATING] = _.seq(
    _.matchDatum(function(datum) {
      switch (datum.type) {
        case r5js.DatumType.BOOLEAN:
        case r5js.DatumType.NUMBER:
        case r5js.DatumType.CHARACTER:
          return true;
        case r5js.DatumType.STRING:
          /* String literals could have escaped backslashes
                     and double quotes, but we want to store them unescaped. */
          // to defeat string-set! on a literal
          datum.unescapeStringLiteral().setImmutable();
          return true;
        default:
          return false;
      }
    }));


// <procedure call> -> (<operator> <operand>*)
// <operator> -> <expression>
// <operand> -> <expression>
r5js.Parser.grammar[Nonterminals.PROCEDURE_CALL] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Nonterminals.OPERATOR),
    _.zeroOrMore(Nonterminals.OPERAND),
    _.one(Terminals.RPAREN)).
        desugar(function(node, env) {

      var operatorNode = node.at(Nonterminals.OPERATOR);
      // will be null if 0 operands
      var operands = node.at(Nonterminals.OPERAND);

      if (operatorNode.isLiteral()) {
        return r5js.procs.newProcCall(
            operatorNode,
            operands,
            new r5js.Continuation());
      }

    // Example: ((f x) y) => (f x [_0 (_0 y [_1 ...])])
      else {
        var desugaredOp = operatorNode.desugar(env);
        var lastContinuation = desugaredOp.getLastContinuable().continuation;
        var opName = lastContinuation.lastResultName;
        lastContinuation.nextContinuable = r5js.procs.newProcCall(
            r5js.data.newIdOrLiteral(opName),
            operands,
            new r5js.Continuation());
        return desugaredOp;
      }
        });


r5js.Parser.grammar[Nonterminals.OPERATOR] = _.seq(
    _.one(Nonterminals.EXPRESSION));


r5js.Parser.grammar[Nonterminals.OPERAND] = _.seq(
    _.one(Nonterminals.EXPRESSION));


// <lambda expression> -> (lambda <formals> <body>)
// <body> -> <definition>* <sequence>
// <sequence> -> <command>* <expression>
// <command> -> <expression>
r5js.Parser.grammar[Nonterminals.LAMBDA_EXPRESSION] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.LAMBDA),
    _.one(Nonterminals.FORMALS),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.oneOrMore(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
        desugar(function(node, env) {
      var formalRoot = node.at(Nonterminals.FORMALS);
      var formals;
      var treatAsDotted = false;

      // (lambda (x y) ...)
      if (formalRoot.isList()) {
        formals = formalRoot.mapChildren(function(child) {
          return child.payload;
            });
      }

    // (lambda (x y z . w) ...)
      else if (formalRoot.isImproperList()) {
        formals = formalRoot.mapChildren(function(child) {
          return child.payload;
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
        formals = [formalRoot.payload];
        treatAsDotted = true;
      }

      var name = newAnonymousLambdaName();
      env.addClosure(
          name,
          new r5js.Procedure(
              formals, treatAsDotted, formalRoot.nextSibling, env, name));
      return newIdShim(r5js.data.newIdOrLiteral(name));
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
r5js.Parser.grammar[Nonterminals.FORMALS] = _.choice(
    _.seq(
    _.one(Terminals.LPAREN),
    _.zeroOrMore(Nonterminals.VARIABLE),
    _.one(Terminals.RPAREN)),
    _.seq(
    _.one(Nonterminals.VARIABLE)),
    _.seq(
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.VARIABLE),
    _.one(Terminals.DOT),
    _.one(Nonterminals.VARIABLE),
    _.one(Terminals.RPAREN)));


/**
 * <definition> -> (define <variable> <expression>)
 * | (define (<variable> <def formals>) <body>)
 * | (begin <definition>*)
 * | <def formals> -> <variable>* | <variable>* . <variable>
 */
r5js.Parser.grammar[Nonterminals.DEFINITION] = _.choice(
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.DEFINE),
    _.one(Nonterminals.VARIABLE),
    _.one(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
    desugar(/** @suppress {checkTypes} */ function(node, env) {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      var variable = node.at(Nonterminals.VARIABLE);
      var desugaredExpr = variable.nextSibling.desugar(env, true);
      var lastContinuable = desugaredExpr.getLastContinuable();
      var cpsName = lastContinuable.continuation.lastResultName;
      lastContinuable.continuation.nextContinuable =
          r5js.procs.newAssignment(
          variable.payload,
          cpsName,
          new r5js.Continuation()).
          setTopLevelAssignment();
      return desugaredExpr;
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.DEFINE),
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.VARIABLE),
    _.one(Terminals.RPAREN),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.oneOrMore(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
    desugar(/** @suppress {checkTypes} */function(node, env) {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      var def = node.extractDefinition();
      var name = def.firstChild;
      var lambda = name.nextSibling;
      var formalRoot = lambda.firstChild.nextSibling;
      var formals = formalRoot.mapChildren(function(child) {
        return child.payload;
      });
      var anonymousName = newAnonymousLambdaName();
      env.addBinding(
          anonymousName,
          new r5js.Procedure(
              formals, false, formalRoot.nextSibling, env, name));
      return r5js.procs.newAssignment(
          name.payload,
          anonymousName,
          new r5js.Continuation()).
          setTopLevelAssignment();
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.DEFINE),
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.VARIABLE),
    _.one(Terminals.DOT),
    _.one(Nonterminals.VARIABLE),
    _.one(Terminals.RPAREN),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.oneOrMore(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
    desugar(/** @suppress {checkTypes} */function(node, env) {
      /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
      var def = node.extractDefinition();
      var name = def.firstChild;
      var lambda = name.nextSibling;
      var formalRoot = lambda.firstChild.nextSibling;
      var formals = formalRoot.firstChild ?
          formalRoot.mapChildren(function(child) {
            return child.payload;
          }) :
          [formalRoot.payload];
      var anonymousName = newAnonymousLambdaName();
      env.addBinding(
          anonymousName,
          new r5js.Procedure(formals, true, formalRoot.nextSibling, env, name));
      return r5js.procs.newAssignment(
          /** @type {string} */(name.payload), // TODO bl
          anonymousName,
          new r5js.Continuation()).
          setTopLevelAssignment();
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.BEGIN),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.one(Terminals.RPAREN)));


// <conditional> -> (if <test> <consequent> <alternate>)
r5js.Parser.grammar[Nonterminals.CONDITIONAL] = _.choice(
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.IF),
    _.one(Nonterminals.TEST),
    _.one(Nonterminals.CONSEQUENT),
    _.one(Nonterminals.ALTERNATE),
    _.one(Terminals.RPAREN)).
    desugar(function(node, env) {
      var test = node.at(Nonterminals.TEST).desugar(env, true);
      var consequent = node.at(Nonterminals.CONSEQUENT).
          desugar(env, true);
      var alternate = node.at(Nonterminals.ALTERNATE).
          desugar(env, true);

      var testEndpoint = test.getLastContinuable();

      var testName = r5js.data.newIdOrLiteral(
          testEndpoint.continuation.lastResultName);
      var branch = newBranch(
          testName,
          consequent,
          alternate,
          new r5js.Continuation());
      testEndpoint.continuation.nextContinuable = branch;
      return test;
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.IF),
    _.one(Nonterminals.TEST),
    _.one(Nonterminals.CONSEQUENT),
    _.one(Terminals.RPAREN)).
    desugar(function(node, env) {
      var test = node.at(Nonterminals.TEST).desugar(env, true);
      var consequent = node.at(Nonterminals.CONSEQUENT).
          desugar(env, true);

      var testEndpoint = test.getLastContinuable();

      var testName = r5js.data.newIdOrLiteral(
          testEndpoint.continuation.lastResultName);
      var branch = newBranch(
          testName,
          consequent,
          null,
          new r5js.Continuation());
      testEndpoint.continuation.nextContinuable = branch;
      return test;
    }));


// <test> -> <expression>
r5js.Parser.grammar[Nonterminals.TEST] = _.seq(
    _.one(Nonterminals.EXPRESSION));


// <consequent> -> <expression>
r5js.Parser.grammar[Nonterminals.CONSEQUENT] = _.seq(
    _.one(Nonterminals.EXPRESSION));


// <alternate> -> <expression> | <empty>
r5js.Parser.grammar[Nonterminals.ALTERNATE] = _.seq(
    _.one(Nonterminals.EXPRESSION));


// <assignment> -> (set! <variable> <expression>)
r5js.Parser.grammar[Nonterminals.ASSIGNMENT] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.SET),
    _.one(Nonterminals.VARIABLE),
    _.one(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
        desugar(function(node, env) {
      // (set! x (+ y z)) => (+ y z [_0 (set! x _0 ...)])
      var variable = node.at(Nonterminals.VARIABLE);
      var desugaredExpr = variable.nextSibling.desugar(env, true);
      var lastContinuable = desugaredExpr.getLastContinuable();
      var cpsName = lastContinuable.continuation.lastResultName;
      lastContinuable.continuation.nextContinuable =
          r5js.procs.newAssignment(
          variable.payload,
          cpsName,
          new r5js.Continuation());
      return desugaredExpr;
        });


// <quasiquotation> -> <quasiquotation 1>
// <quasiquotation D> -> `<qq template D> | (quasiquote <qq template D>)
r5js.Parser.grammar[Nonterminals.QUASIQUOTATION] = _.choice(
    _.seq(
    _.one(Terminals.BACKTICK),
    _.one(Nonterminals.QQ_TEMPLATE)),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.QUASIQUOTE),
    _.one(Nonterminals.QQ_TEMPLATE),
    _.one(Terminals.RPAREN)));


/* <qq template 0> -> <expression>
 <qq template D> -> <simple datum>
 | <list qq template D>
 | <vector qq template D>
 | <unquotation D>
 */
r5js.Parser.grammar[Nonterminals.QQ_TEMPLATE] = _.choice(
    _.matchDatum(function(datum) {
      switch (datum.type) {
        case r5js.DatumType.BOOLEAN:
        case r5js.DatumType.NUMBER:
        case r5js.DatumType.CHARACTER:
        case r5js.DatumType.STRING:
        case r5js.DatumType.IDENTIFIER:
          return true;
        default:
          return false;
      }
    }),
    _.one(Nonterminals.LIST_QQ_TEMPLATE),
    _.one(Nonterminals.VECTOR_QQ_TEMPLATE),
    _.one(Nonterminals.UNQUOTATION));


/*<list qq template D> -> (<qq template or splice D>*)
 | (<qq template or splice D>+ . <qq template D>)
 | '<qq template D>
 | <quasiquotation D+1>
 */
r5js.Parser.grammar[Nonterminals.LIST_QQ_TEMPLATE] = _.choice(
    _.seq(
        _.one(Terminals.LPAREN),
        _.zeroOrMore(Nonterminals.QQ_TEMPLATE_OR_SPLICE),
        _.one(Terminals.RPAREN)),
    _.seq(
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.QQ_TEMPLATE_OR_SPLICE),
    _.one(Terminals.DOT),
    _.one(
        Nonterminals.QQ_TEMPLATE_OR_SPLICE),
    _.one(Terminals.RPAREN)),
    _.seq(
    _.one(Terminals.TICK),
    _.one(Nonterminals.QQ_TEMPLATE)),
    _.seq(
    _.one(Nonterminals.QUASIQUOTATION)));


// <vector qq template D> -> #(<qq template or splice D>*)
r5js.Parser.grammar[Nonterminals.VECTOR_QQ_TEMPLATE] =
    _.seq(
    _.one(Terminals.LPAREN_VECTOR),
    _.zeroOrMore(Nonterminals.QQ_TEMPLATE_OR_SPLICE),
    _.one(Terminals.RPAREN));


// <unquotation D> -> ,<qq template D-1> | (unquote <qq template D-1>)
r5js.Parser.grammar[Nonterminals.UNQUOTATION] =
    _.choice(
    _.seq(
    _.one(Terminals.COMMA),
    _.one(Nonterminals.QQ_TEMPLATE)),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.UNQUOTE),
    _.one(Nonterminals.QQ_TEMPLATE),
    _.one(Terminals.RPAREN)));


// <qq template or splice D> -> <qq template D> | <splicing unquotation D>
r5js.Parser.grammar[Nonterminals.QQ_TEMPLATE_OR_SPLICE] =
    _.choice(
    _.seq(
    _.one(Nonterminals.QQ_TEMPLATE)),
    _.seq(
    _.one(
        Nonterminals.SPLICING_UNQUOTATION)));


/* <splicing unquotation D> -> ,@<qq template D-1>
 | (unquote-splicing <qq template D-1>)
 */
r5js.Parser.grammar[Nonterminals.SPLICING_UNQUOTATION] =
    _.choice(
    _.seq(
    _.one(Terminals.COMMA_AT),
    _.one(Nonterminals.QQ_TEMPLATE)),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.UNQUOTE_SPLICING),
    _.one(Nonterminals.QQ_TEMPLATE),
    _.one(Terminals.RPAREN)));


// <macro use> -> (<keyword> <datum>*)
r5js.Parser.grammar[Nonterminals.MACRO_USE] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Nonterminals.KEYWORD),
    _.zeroOrMore(Nonterminals.DATUM),
    _.one(Terminals.RPAREN)).
        desugar(function(node, env) {
      /* Desugaring of a macro use is trivial. We must leave the "argument"
                datums as-is for the macro pattern matching facility to use.
                The trampoline knows what to do with raw datums in such a
                context. */
      return r5js.procs.newProcCall(
          node.at(Nonterminals.KEYWORD),
          node.at(Nonterminals.DATUM),
          new r5js.Continuation());
    });


// <keyword> -> <identifier>
r5js.Parser.grammar[Nonterminals.KEYWORD] = _.seq(
    _.matchDatum(function(datum) {
      /* TODO bl: Tests fail when I replace this type switch by
        datum.isIdentifier(), suggesting that this argument is not always
        a Datum. Investigate. */
      return datum.type === r5js.DatumType.IDENTIFIER;
    }));


/* <macro block> -> (let-syntax (<syntax spec>*) <body>)
 | (letrec-syntax (<syntax-spec>*) <body>) */
r5js.Parser.grammar[Nonterminals.MACRO_BLOCK] =
    _.choice(
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.LET_SYNTAX),
    _.one(Terminals.LPAREN),
    _.zeroOrMore(Nonterminals.SYNTAX_SPEC),
    _.one(Terminals.RPAREN),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.oneOrMore(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
    desugar(function(node, env) {
      return r5js.Continuation.desugarMacroBlock(node, env, 'let');
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.LETREC_SYNTAX),
    _.one(Terminals.LPAREN),
    _.zeroOrMore(Nonterminals.SYNTAX_SPEC),
    _.one(Terminals.RPAREN),
    _.zeroOrMore(Nonterminals.DEFINITION),
    _.oneOrMore(Nonterminals.EXPRESSION),
    _.one(Terminals.RPAREN)).
    desugar(function(node, env) {
      return r5js.Continuation.desugarMacroBlock(node, env, 'letrec');
    }));


// <syntax spec> -> (<keyword> <transformer spec>)
r5js.Parser.grammar[Nonterminals.SYNTAX_SPEC] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Nonterminals.KEYWORD),
    _.one(Nonterminals.TRANSFORMER_SPEC),
    _.one(Terminals.RPAREN));


// <transformer spec> -> (syntax-rules (<identifier>*) <syntax rule>*)
r5js.Parser.grammar[Nonterminals.TRANSFORMER_SPEC] =
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.SYNTAX_RULES),
    _.one(Terminals.LPAREN),
    _.zeroOrMore(Nonterminals.PATTERN_IDENTIFIER),
    _.one(Terminals.RPAREN),
    _.zeroOrMore(Nonterminals.SYNTAX_RULE),
    _.one(Terminals.RPAREN)).
        desugar(function(node, env) {
      /*4.3.2: It is an error for ... to appear in <literals>.
                So we can reuse the pattern-identifier nonterminal
                to check this in the parser. Win! */
      var ids = node.at(Terminals.LPAREN).
          at(Nonterminals.PATTERN_IDENTIFIER);
      var rules = node.at(Nonterminals.SYNTAX_RULE);
      // todo bl implement: It is an error for the same pattern
      // variable to appear more than once in a <pattern>.
      return new r5js.Macro(ids, /** @type {!r5js.Datum} */(rules), env);
        });


// <syntax rule> -> (<pattern> <template>)
r5js.Parser.grammar[Nonterminals.SYNTAX_RULE] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Nonterminals.PATTERN),
    _.one(Nonterminals.TEMPLATE),
    _.one(Terminals.RPAREN));


/* <pattern> -> <pattern identifier>
 | (<pattern>*)
 | (<pattern>+ . <pattern>)
 | (<pattern>+ <ellipsis>)
 | #(<pattern>*)
 | #(<pattern>+ <ellipsis>)
 | <pattern datum>
 */
r5js.Parser.grammar[Nonterminals.PATTERN] = _.choice(
    _.seq(
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.PATTERN),
    _.one(Terminals.ELLIPSIS),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.LIST);
      for (var cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.nextSibling) {
        if (cur.nextSibling &&
            cur.nextSibling.payload === Terminals.ELLIPSIS) {
          ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
          break;
        } else {
          ans.addSubtransformer(cur.desugar());
        }
      }
      return ans;
    }),
    _.seq(
    _.one(Terminals.LPAREN_VECTOR),
    _.oneOrMore(Nonterminals.PATTERN),
    _.one(Terminals.ELLIPSIS),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.VECTOR);
      for (var cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.nextSibling) {
        if (cur.nextSibling &&
            cur.nextSibling.payload === Terminals.ELLIPSIS) {
          ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
          break;
        } else {
          ans.addSubtransformer(cur.desugar());
        }
      }
      return ans;
    }),
    _.seq(
    _.one(Nonterminals.PATTERN_IDENTIFIER)).
    desugar(function(node) {
      return new r5js.IdOrLiteralTransformer(node);
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.zeroOrMore(Nonterminals.PATTERN),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.LIST);
      for (var cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.nextSibling) {
        ans.addSubtransformer(cur.desugar());
      }
      return ans;
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.PATTERN),
    _.one(Terminals.DOT),
    _.one(Nonterminals.PATTERN),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.DOTTED_LIST);
      for (var cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.nextSibling) {
        ans.addSubtransformer(cur.desugar());
      }
      return ans;
    }),
    _.seq(
    _.one(Terminals.LPAREN_VECTOR),
    _.zeroOrMore(Nonterminals.PATTERN),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.VECTOR);
      for (var cur = node.at(Nonterminals.PATTERN);
           cur;
           cur = cur.nextSibling) {
        ans.addSubtransformer(cur.desugar());
      }
      return ans;
    }),
    _.seq(
    _.one(Nonterminals.PATTERN_DATUM)).
    desugar(function(node) {
      return new r5js.IdOrLiteralTransformer(node);
    }));


// <pattern datum> -> <string> | <character> | <boolean> | <number>
r5js.Parser.grammar[Nonterminals.PATTERN_DATUM] = _.seq(
    _.matchDatum(function(datum) {
      switch (datum.type) {
        case r5js.DatumType.BOOLEAN:
        case r5js.DatumType.NUMBER:
        case r5js.DatumType.CHARACTER:
        case r5js.DatumType.STRING:
          return true;
        default:
          return false;
      }
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
r5js.Parser.grammar[Nonterminals.TEMPLATE] = _.choice(
    _.seq(
    _.one(Nonterminals.PATTERN_IDENTIFIER)).
    desugar(function(node) {
      return new r5js.IdOrLiteralTransformer(node);
    }),
    _.seq(
        _.one(Terminals.ELLIPSIS)),
    _.seq(
    _.one(Nonterminals.TEMPLATE_DATUM)).
    desugar(function(node) {
      return new r5js.IdOrLiteralTransformer(node);
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.oneOrMore(Nonterminals.TEMPLATE),
    _.one(Terminals.DOT),
    _.one(Nonterminals.TEMPLATE),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.DOTTED_LIST);
      for (var cur = node.at(Nonterminals.TEMPLATE);
           cur;
           cur = cur.nextSibling) {
        if (cur.nextSibling &&
            cur.nextSibling.payload === Terminals.ELLIPSIS) {
          ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
          cur = cur.nextSibling;
        } else {
          ans.addSubtransformer(cur.desugar());
        }
      }

      return ans;
    }),
    _.seq(
    _.one(Terminals.LPAREN),
    _.zeroOrMore(Nonterminals.TEMPLATE),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.LIST);
      for (var cur = node.at(Nonterminals.TEMPLATE);
           cur;
           cur = cur.nextSibling) {
        if (cur.nextSibling &&
            cur.nextSibling.payload === Terminals.ELLIPSIS) {
          ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
          cur = cur.nextSibling;
        } else {
          ans.addSubtransformer(cur.desugar());
        }
      }
      return ans;
    }),
    _.seq(
    _.one(Terminals.LPAREN_VECTOR),
    _.zeroOrMore(Nonterminals.TEMPLATE),
    _.one(Terminals.RPAREN)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.VECTOR);
      for (var cur = node.at(Nonterminals.TEMPLATE);
           cur;
           cur = cur.nextSibling) {
        if (cur.nextSibling &&
            cur.nextSibling.payload === Terminals.ELLIPSIS) {
          ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
          cur = cur.nextSibling;
        } else {
          ans.addSubtransformer(cur.desugar());
        }
      }
      return ans;
    }),
    _.seq(
    _.one(Terminals.TICK),
    _.one(Nonterminals.TEMPLATE)).
    desugar(function(node) {
      var ans = new r5js.ListLikeTransformer(r5js.DatumType.QUOTE);
      ans.addSubtransformer(node.at(Nonterminals.TEMPLATE).
          desugar());
      return ans;
    }));


// <template datum> -> <pattern datum>
r5js.Parser.grammar[Nonterminals.TEMPLATE_DATUM] = _.seq(
    _.one(Nonterminals.PATTERN_DATUM));


// <pattern identifier> -> <any identifier except ...>
r5js.Parser.grammar[Nonterminals.PATTERN_IDENTIFIER] = _.seq(
    _.matchDatum(function(datum) {
      /* TODO bl: Tests fail when I replace this type switch by
         datum.isIdentifier(), suggesting that this argument is not
         always a Datum. Investigate. */
      return datum.type === r5js.DatumType.IDENTIFIER &&
          datum.payload !== Terminals.ELLIPSIS;
    }));


// <program> -> <command or definition>*
r5js.Parser.grammar[Nonterminals.PROGRAM] = _.seq(
    _.zeroOrMore(Nonterminals.COMMAND_OR_DEFINITION)).
        desugar(function(node, env) {
      return node.sequence(env);
    });


/* <command or definition> -> <command>
 | <definition>
 | <syntax definition>
 | (begin <command or definition>*)
 */
r5js.Parser.grammar[Nonterminals.COMMAND_OR_DEFINITION] = _.choice(
    _.seq(
    _.one(Nonterminals.DEFINITION)),
    _.seq(
    _.one(Nonterminals.SYNTAX_DEFINITION)),
    _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.BEGIN),
    _.zeroOrMore(Nonterminals.COMMAND_OR_DEFINITION),
    _.one(Terminals.RPAREN)),
    _.seq(
    _.one(Nonterminals.COMMAND)));


// <command> -> <expression>
r5js.Parser.grammar[Nonterminals.COMMAND] = _.seq(
    _.one(Nonterminals.EXPRESSION));


// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
r5js.Parser.grammar[Nonterminals.SYNTAX_DEFINITION] = _.seq(
    _.one(Terminals.LPAREN),
    _.one(Terminals.DEFINE_SYNTAX),
    _.one(Nonterminals.KEYWORD),
    _.one(Nonterminals.TRANSFORMER_SPEC),
    _.one(Terminals.RPAREN)).
        desugar(function(node, env) {
      var kw = node.at(Nonterminals.KEYWORD).payload;
      var macro = node.at(Nonterminals.TRANSFORMER_SPEC).
          desugar(env);
      if (!macro.allPatternsBeginWith(kw))
        throw new r5js.MacroError(kw, 'all patterns must begin with ' + kw);
      var anonymousName = newAnonymousLambdaName();
      env.addBinding(anonymousName, macro);
      return r5js.procs.newAssignment(
          kw,
          anonymousName,
          new r5js.Continuation()).
          setTopLevelAssignment().
          setSyntaxAssignment();
    });

});  // goog.scope
