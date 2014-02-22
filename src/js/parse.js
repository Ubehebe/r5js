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
goog.require('r5js.data');
goog.require('r5js.Datum');
goog.require('r5js.DatumStreamImpl');
goog.require('r5js.DatumType');
goog.require('r5js.EllipsisTransformer');
goog.require('r5js.IdOrLiteralTransformer');
goog.require('r5js.InternalInterpreterError');
goog.require('r5js.ListLikeTransformer');
goog.require('r5js.Macro');
goog.require('r5js.MacroError');
goog.require('r5js.parse.bnf');
goog.require('r5js.Procedure');
goog.require('r5js.procs');
goog.require('r5js.RenameHelper');
goog.require('r5js.parse.Nonterminals');
goog.require('r5js.parse.Terminals');
goog.require('r5js.Macro');


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

    /** @private {boolean} */
    this.fixParserSensitiveIds_ = false;
};


/**
 * When a parse of a node n succeeds, n is returned and this.next_
 * is advanced to the next node to parse. When a parse of n fails,
 * null is returned and this.next_ still points to n.
 * @param {...*} var_args
 * @return {r5js.Datum} The root of the parse tree, or null if parsing failed.
 * TODO bl: narrow the signature.
 */
r5js.Parser.prototype.rhs = function(var_args) {
    var root = this.datumStream_.getNextDatum();

    /* This is a convenience function: we want to specify parse rules like
     (<variable>+ . <variable>) as if we don't know ahead of time whether
     the list is going to be dotted or not, but the reader already knows.
     Proper and improper lists are both represented as first-child-next-sibling
     linked lists; the only difference is the type ('(' vs. '.('). So we rewrite the
     parse rules to conform to the reader's knowledge. */
    r5js.Parser.rewriteImproperList_(arguments);

    for (var i = 0; i < arguments.length; ++i) {
        var element = arguments[i];

        // Process parsing actions
        if (r5js.parse.bnf.Rule.isImplementedBy(element)) {
            if (!(/** @type {!r5js.parse.bnf.Rule} */ (element)).match(this.datumStream_, this)) {
                /* This check is necessary because root may be the special
                 sentinel object for empty lists. */
                if (root instanceof r5js.Datum)
                    root.unsetParse();
                this.datumStream_.advanceTo(/** @type {!r5js.Datum} */ (root));
                return null;
            }
        }

        if (element.desugar) {
            /* todo bl this is an error in the text of the grammar, and
                should be caught at startup time, not parsing time. It would be
                nice to add a preprocessing step to the interpreter to verify
                the integrity of all its data structures before it starts
                accepting input from the user. */
            if (i !== arguments.length - 1)
                throw new r5js.InternalInterpreterError('desugaring actions '
                + 'are only allowed as the last element in the right-hand '
                + 'side of a grammar rule.');
            /* If we are here, root must be an instance of Datum. The only
                other possibility is the emptyListSentinel, but that always
                causes parsing to fail, so we could not be here. */
            if (root)
                root.setDesugar(element.desugar);
        }

    }

    var nextSibling = /** just in case of an empty program */ root && root.nextSibling;
    this.datumStream_.advanceTo(/** @type {!r5js.Datum} */ (nextSibling));
    return root;
};


/**
 * @param {...*} var_args
 * @private
 * TODO bl: narrow the signature.
 */
r5js.Parser.prototype.alternation_ = function(var_args) {
    var possibleRhs;
    for (var i = 0; i < arguments.length; ++i) {
        if (possibleRhs = this.rhs.apply(this, arguments[i]))
            return possibleRhs;
    }
    return null;
};


/**
 * @param {!goog.array.ArrayLike} rhsArgs
 * @private
 */
r5js.Parser.rewriteImproperList_ = function(rhsArgs) {
    // example: (define (x . y) 1) => (define .( x . ) 1)
    /* No RHS in the grammar has more than one dot.
     This will break if such a rule is added. */
    var indexOfDot = goog.array.findIndex(rhsArgs, function(arg) {
        return arg.type === r5js.parse.Terminals.DOT ||
            r5js.parse.bnf.Rule.isDot(arg);
    });

    if (indexOfDot === -1) {
        return;
    }

    /* Change the datum following the dot to be vacuous -- it has already
     been read as part of the list preceding the dot.
     todo bl: this will cause problems with exactly one part of the grammar:
     <template> -> (<template element>+ . <template>)
     I think it's easier to check for this in the evaluator. */
    rhsArgs[indexOfDot + 1] = r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DOT);
    // Find the closest opening paren to the left of the dot and rewrite it as .(
    for (var i = indexOfDot - 1; i >= 0; --i) {
        var arg = rhsArgs[i];
        if (r5js.parse.bnf.Rule.isLparen(arg) ||
            rhsArgs[i].type === r5js.parse.Terminals.LPAREN) {
            rhsArgs[i] = r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN_DOT);
            return;
        }
    }
};


/**
 * @param {function(!r5js.Datum):boolean} predicate Predicate to apply
 * to the next datum.
 * @return {boolean} True iff the predicate applied and the parser advanced
 * to the next datum.
 * @private
 */
r5js.Parser.prototype.nextIf_ = function(predicate) {
    var next = this.datumStream_.getNextDatum();
    if (next && predicate(next)) {
        this.datumStream_.advanceToNextSibling();
        return true;
    } else {
        return false;
    }
};


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
r5js.Parser.prototype[r5js.parse.Nonterminals.EXPRESSION] = function() {
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
    return this.alternation_(
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VARIABLE)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.LITERAL)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.LAMBDA_EXPRESSION)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.CONDITIONAL)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.ASSIGNMENT)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QUASIQUOTATION),
            {desugar: function(node, env) {
                return node.normalizeInput().decorateQuasiquote(1);
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.BEGIN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.EXPRESSION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.MACRO_BLOCK)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PROCEDURE_CALL)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.MACRO_USE)
        ]);
};

// <variable> -> <any <identifier> that isn't also a <syntactic keyword>>
r5js.Parser.prototype[r5js.parse.Nonterminals.VARIABLE] = function() {
    var self = this;
    return this.rhs(
        r5js.parse.bnf.matchDatum(function(datum) {
            var ans = datum instanceof r5js.Datum // because it may be emptyListSentinel
                && datum.isIdentifier();
            if (ans && isParserSensitiveId(/** @type {string} */ (datum.payload))) {
                self.fixParserSensitiveIds_ = true;
            }
            return ans;
        }));
};


// <literal> -> <quotation> | <self-evaluating>
r5js.Parser.prototype[r5js.parse.Nonterminals.LITERAL] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.SELF_EVALUATING)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QUOTATION)
        ]);
};


// <quotation> -> '<datum> | (quote <datum>)
r5js.Parser.prototype[r5js.parse.Nonterminals.QUOTATION] = function() {

    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.TICK),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.DATUM),
            {desugar: function(node, env) {
                return node.normalizeInput();
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.QUOTE),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.DATUM),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
                return node.normalizeInput();
            }
            }
        ]);
};


r5js.Parser.prototype[r5js.parse.Nonterminals.DATUM] = function() {
    return this.rhs(r5js.parse.bnf.matchDatum(function(datum) {
        return true;
    }));
};


// <self-evaluating> -> <boolean> | <number> | <character> | <string>
r5js.Parser.prototype[r5js.parse.Nonterminals.SELF_EVALUATING] = function() {
    return this.rhs(
        r5js.parse.bnf.matchDatum(function(datum) {
            switch (datum.type) {
                case r5js.DatumType.BOOLEAN:
                case r5js.DatumType.NUMBER:
                case r5js.DatumType.CHARACTER:
                    return true;
                case r5js.DatumType.STRING:
                    /* String literals could have escaped backslashes
                     and double quotes, but we want to store them unescaped. */
                    datum.unescapeStringLiteral().setImmutable(); // to defeat string-set! on a literal
                    return true;
                default:
                    return false;
            }
        }));
};


// <procedure call> -> (<operator> <operand>*)
// <operator> -> <expression>
// <operand> -> <expression>
r5js.Parser.prototype[r5js.parse.Nonterminals.PROCEDURE_CALL] = function() {

    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.OPERATOR),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.OPERAND),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        {desugar: function(node, env) {

            var operatorNode = node.at(r5js.parse.Nonterminals.OPERATOR);
            var operands = node.at(r5js.parse.Nonterminals.OPERAND); // will be null if 0 operands

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
        }
        }
    );
};


r5js.Parser.prototype[r5js.parse.Nonterminals.OPERATOR] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION));
};


r5js.Parser.prototype[r5js.parse.Nonterminals.OPERAND] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION));
};

// <lambda expression> -> (lambda <formals> <body>)
// <body> -> <definition>* <sequence>
// <sequence> -> <command>* <expression>
// <command> -> <expression>
r5js.Parser.prototype[r5js.parse.Nonterminals.LAMBDA_EXPRESSION] = function() {

    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LAMBDA),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.FORMALS),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DEFINITION),
        r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.EXPRESSION),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        {desugar: function(node, env) {
            var formalRoot = node.at(r5js.parse.Nonterminals.FORMALS);
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
                new r5js.Procedure(formals, treatAsDotted, formalRoot.nextSibling, env, name));
            return newIdShim(r5js.data.newIdOrLiteral(name));
        }
        }
    );
};

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
r5js.Parser.prototype[r5js.parse.Nonterminals.FORMALS] = function() {

    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VARIABLE)
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DOT),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ]);
};

/*
<definition> -> (define <variable> <expression>)
| (define (<variable> <def formals>) <body>)
| (begin <definition>*)
 <def formals> -> <variable>* | <variable>* . <variable>
 */
r5js.Parser.prototype[r5js.parse.Nonterminals.DEFINITION] = function() {

    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DEFINE),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
                /* If we're here, this must be a top-level definition, so we
                should rewrite it as an assignment. Definitions internal
                to a procedure are intercepted in the SchemeProcedure
                constructor and rewritten as letrec bindings, so they never
                get here.

                todo bl: make this flow of control explicit. */
                var variable = node.at(r5js.parse.Nonterminals.VARIABLE);
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
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DEFINE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DEFINITION),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.EXPRESSION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
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
                    new r5js.Procedure(formals, false, formalRoot.nextSibling, env, name));
                return r5js.procs.newAssignment(
                    name.payload,
                    anonymousName,
                    new r5js.Continuation()).
                    setTopLevelAssignment();
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DEFINE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DOT),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VARIABLE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DEFINITION),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.EXPRESSION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
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
                var formals = formalRoot.firstChild
                    ? formalRoot.mapChildren(function(child) {
                    return child.payload;
                }) : [formalRoot.payload];
                var anonymousName = newAnonymousLambdaName();
                env.addBinding(
                    anonymousName,
                    new r5js.Procedure(formals, true, formalRoot.nextSibling, env, name));
                return r5js.procs.newAssignment(
                    name.payload,
                    anonymousName,
                    new r5js.Continuation()).
                    setTopLevelAssignment();
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.BEGIN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DEFINITION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
            // will be recursively desugared automatically by sequence()
        ]);

};

// <conditional> -> (if <test> <consequent> <alternate>)
r5js.Parser.prototype[r5js.parse.Nonterminals.CONDITIONAL] = function() {

    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.IF),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TEST),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.CONSEQUENT),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.ALTERNATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
                var test = node.at(r5js.parse.Nonterminals.TEST).desugar(env, true);
                var consequent = node.at(r5js.parse.Nonterminals.CONSEQUENT).desugar(env, true);
                var alternate = node.at(r5js.parse.Nonterminals.ALTERNATE).desugar(env, true);

                var testEndpoint = test.getLastContinuable();

                var testName = r5js.data.newIdOrLiteral(testEndpoint.continuation.lastResultName);
                var branch = newBranch(
                    testName,
                    consequent,
                    alternate,
                    new r5js.Continuation());
                testEndpoint.continuation.nextContinuable = branch;
                return test;
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.IF),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TEST),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.CONSEQUENT),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
                var test = node.at(r5js.parse.Nonterminals.TEST).desugar(env, true);
                var consequent = node.at(r5js.parse.Nonterminals.CONSEQUENT).desugar(env, true);

                var testEndpoint = test.getLastContinuable();

                var testName = r5js.data.newIdOrLiteral(testEndpoint.continuation.lastResultName);
                var branch = newBranch(
                    testName,
                    consequent,
                    null,
                    new r5js.Continuation());
                testEndpoint.continuation.nextContinuable = branch;
                return test;
            }
            }
        ]
    );
};

// <test> -> <expression>
r5js.Parser.prototype[r5js.parse.Nonterminals.TEST] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION));
};


// <consequent> -> <expression>
r5js.Parser.prototype[r5js.parse.Nonterminals.CONSEQUENT] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION));
};


// <alternate> -> <expression> | <empty>
r5js.Parser.prototype[r5js.parse.Nonterminals.ALTERNATE] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION));
};


// <assignment> -> (set! <variable> <expression>)
r5js.Parser.prototype[r5js.parse.Nonterminals.ASSIGNMENT] = function() {

    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.SET),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VARIABLE),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        {desugar: function(node, env) {
            // (set! x (+ y z)) => (+ y z [_0 (set! x _0 ...)])
            var variable = node.at(r5js.parse.Nonterminals.VARIABLE);
            var desugaredExpr = variable.nextSibling.desugar(env, true);
            var lastContinuable = desugaredExpr.getLastContinuable();
            var cpsName = lastContinuable.continuation.lastResultName;
            lastContinuable.continuation.nextContinuable =
                r5js.procs.newAssignment(
                    variable.payload,
                    cpsName,
                    new r5js.Continuation());
            return desugaredExpr;
        }
        }
    );
};


// <quasiquotation> -> <quasiquotation 1>
// <quasiquotation D> -> `<qq template D> | (quasiquote <qq template D>)
r5js.Parser.prototype[r5js.parse.Nonterminals.QUASIQUOTATION] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.BACKTICK),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE)
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.QUASIQUOTE),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ]
    );
};


/* <qq template 0> -> <expression>
 <qq template D> -> <simple datum>
 | <list qq template D>
 | <vector qq template D>
 | <unquotation D>
 */
r5js.Parser.prototype[r5js.parse.Nonterminals.QQ_TEMPLATE] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.matchDatum(function(datum) {
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
            })
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.LIST_QQ_TEMPLATE)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.VECTOR_QQ_TEMPLATE)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.UNQUOTATION)
        ]
    );
};


/*<list qq template D> -> (<qq template or splice D>*)
 | (<qq template or splice D>+ . <qq template D>)
 | '<qq template D>
 | <quasiquotation D+1>
 */
r5js.Parser.prototype[r5js.parse.Nonterminals.LIST_QQ_TEMPLATE] = function() {
  return this.alternation_(
    [
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.QQ_TEMPLATE_OR_SPLICE),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
    ],
      [
          r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
          r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.QQ_TEMPLATE_OR_SPLICE),
          r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DOT),
          r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE_OR_SPLICE),
          r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
      ],
      [
          r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.TICK),
          r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE)
      ],
      [
          r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QUASIQUOTATION)
      ]
  );
};


// <vector qq template D> -> #(<qq template or splice D>*)
r5js.Parser.prototype[r5js.parse.Nonterminals.VECTOR_QQ_TEMPLATE] = function() {
    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN_VECTOR),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.QQ_TEMPLATE_OR_SPLICE),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN));
};


// <unquotation D> -> ,<qq template D-1> | (unquote <qq template D-1>)
r5js.Parser.prototype[r5js.parse.Nonterminals.UNQUOTATION] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.COMMA),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE)
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.UNQUOTE),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ]
    );
};


// <qq template or splice D> -> <qq template D> | <splicing unquotation D>
r5js.Parser.prototype[r5js.parse.Nonterminals.QQ_TEMPLATE_OR_SPLICE] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.SPLICING_UNQUOTATION)
        ]
    );
};


/* <splicing unquotation D> -> ,@<qq template D-1>
 | (unquote-splicing <qq template D-1>)
 */
r5js.Parser.prototype[r5js.parse.Nonterminals.SPLICING_UNQUOTATION] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.COMMA_AT),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE)
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.UNQUOTE_SPLICING),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.QQ_TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ]
    );
};


// <macro use> -> (<keyword> <datum>*)
r5js.Parser.prototype[r5js.parse.Nonterminals.MACRO_USE] = function() {

    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.KEYWORD),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DATUM),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        {desugar: function(node, env) {
            /* Desugaring of a macro use is trivial. We must leave the "argument"
                datums as-is for the macro pattern matching facility to use.
                The trampoline knows what to do with raw datums in such a
                context. */
            return r5js.procs.newProcCall(
                node.at(r5js.parse.Nonterminals.KEYWORD),
                node.at(r5js.parse.Nonterminals.DATUM),
                new r5js.Continuation());
        }
        });
};


// <keyword> -> <identifier>
r5js.Parser.prototype[r5js.parse.Nonterminals.KEYWORD] = function() {
    return this.rhs(r5js.parse.bnf.matchDatum(function(datum) {
        /* TODO bl: Tests fail when I replace this type switch by
        datum.isIdentifier(), suggesting that this argument is not always
        a Datum. Investigate. */
        return datum.type === r5js.DatumType.IDENTIFIER;
    }));
};


/* <macro block> -> (let-syntax (<syntax spec>*) <body>)
 | (letrec-syntax (<syntax-spec>*) <body>) */
r5js.Parser.prototype[r5js.parse.Nonterminals.MACRO_BLOCK] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LET_SYNTAX),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.SYNTAX_SPEC),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DEFINITION),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.EXPRESSION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
                return r5js.Continuation.desugarMacroBlock(node, env, 'let');
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LETREC_SYNTAX),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.SYNTAX_SPEC),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.DEFINITION),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.EXPRESSION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node, env) {
                return r5js.Continuation.desugarMacroBlock(node, env, 'letrec');
            }
            }
        ]);
};


// <syntax spec> -> (<keyword> <transformer spec>)
r5js.Parser.prototype[r5js.parse.Nonterminals.SYNTAX_SPEC] = function() {
    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.KEYWORD),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TRANSFORMER_SPEC),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN));
};


// <transformer spec> -> (syntax-rules (<identifier>*) <syntax rule>*)
r5js.Parser.prototype[r5js.parse.Nonterminals.TRANSFORMER_SPEC] = function() {
    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.SYNTAX_RULES),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.PATTERN_IDENTIFIER),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.SYNTAX_RULE),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        {desugar: function(node, env) {
            /*4.3.2: It is an error for ... to appear in <literals>.
                So we can reuse the pattern-identifier nonterminal
                to check this in the parser. Win! */
            var ids = node.at(r5js.parse.Terminals.LPAREN).at(r5js.parse.Nonterminals.PATTERN_IDENTIFIER);
            var rules = node.at(r5js.parse.Nonterminals.SYNTAX_RULE);
            // todo bl implement: It is an error for the same pattern
            // variable to appear more than once in a <pattern>.
            return new r5js.Macro(ids, rules, env);
        }
        }
    );
};


// <syntax rule> -> (<pattern> <template>)
r5js.Parser.prototype[r5js.parse.Nonterminals.SYNTAX_RULE] = function() {
    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PATTERN),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TEMPLATE),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN));
};


/* <pattern> -> <pattern identifier>
 | (<pattern>*)
 | (<pattern>+ . <pattern>)
 | (<pattern>+ <ellipsis>)
 | #(<pattern>*)
 | #(<pattern>+ <ellipsis>)
 | <pattern datum>
 */
r5js.Parser.prototype[r5js.parse.Nonterminals.PATTERN] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.PATTERN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.ELLIPSIS),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.LIST);
                for (var cur = node.at(r5js.parse.Nonterminals.PATTERN); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === r5js.parse.Terminals.ELLIPSIS) {
                        ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
                        break;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN_VECTOR),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.PATTERN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.ELLIPSIS),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.VECTOR);
                for (var cur = node.at(r5js.parse.Nonterminals.PATTERN); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === r5js.parse.Terminals.ELLIPSIS) {
                        ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
                        break;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }}
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PATTERN_IDENTIFIER),
            {desugar: function(node) {
                return new r5js.IdOrLiteralTransformer(node);
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.PATTERN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.LIST);
                for (var cur = node.at(r5js.parse.Nonterminals.PATTERN); cur; cur = cur.nextSibling)
                    ans.addSubtransformer(cur.desugar());
                return ans;
            }}
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.PATTERN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DOT),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PATTERN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.DOTTED_LIST);
                for (var cur = node.at(r5js.parse.Nonterminals.PATTERN); cur; cur = cur.nextSibling)
                    ans.addSubtransformer(cur.desugar());
                return ans;
            }}
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN_VECTOR),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.PATTERN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.VECTOR);
                for (var cur = node.at(r5js.parse.Nonterminals.PATTERN); cur; cur = cur.nextSibling)
                    ans.addSubtransformer(cur.desugar());
                return ans;
            }}
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PATTERN_DATUM),
            {desugar: function(node) {
                return new r5js.IdOrLiteralTransformer(node);
            }}
        ]
    );
};


// <pattern datum> -> <string> | <character> | <boolean> | <number>
r5js.Parser.prototype[r5js.parse.Nonterminals.PATTERN_DATUM] = function() {
    return this.rhs(
        r5js.parse.bnf.matchDatum(function(datum) {
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
};


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
r5js.Parser.prototype[r5js.parse.Nonterminals.TEMPLATE] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PATTERN_IDENTIFIER),
            {desugar: function(node) {
                return new r5js.IdOrLiteralTransformer(node);
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.ELLIPSIS)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TEMPLATE_DATUM),
            {desugar: function(node) {
                return new r5js.IdOrLiteralTransformer(node);
            }
            }

        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneOrMore(r5js.parse.Nonterminals.TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DOT),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.DOTTED_LIST);
                for (var cur = node.at(r5js.parse.Nonterminals.TEMPLATE); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === r5js.parse.Terminals.ELLIPSIS) {
                        ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
                        cur = cur.nextSibling;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }

                return ans;
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.LIST);
                for (var cur = node.at(r5js.parse.Nonterminals.TEMPLATE); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === r5js.parse.Terminals.ELLIPSIS) {
                        ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
                        cur = cur.nextSibling;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN_VECTOR),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.TEMPLATE),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.VECTOR);
                for (var cur = node.at(r5js.parse.Nonterminals.TEMPLATE); cur; cur = cur.nextSibling) {
                    if (cur.nextSibling && cur.nextSibling.payload === r5js.parse.Terminals.ELLIPSIS) {
                        ans.addSubtransformer(new r5js.EllipsisTransformer(cur.desugar()));
                        cur = cur.nextSibling;
                    } else {
                        ans.addSubtransformer(cur.desugar());
                    }
                }
                return ans;
            }
            }
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.TICK),
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TEMPLATE),
            {desugar: function(node) {
                var ans = new r5js.ListLikeTransformer(r5js.DatumType.QUOTE);
                ans.addSubtransformer(node.at(r5js.parse.Nonterminals.TEMPLATE).desugar());
                return ans;
            }}
        ]
    );
};


// <template datum> -> <pattern datum>
r5js.Parser.prototype[r5js.parse.Nonterminals.TEMPLATE_DATUM] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.PATTERN_DATUM));
};


// <pattern identifier> -> <any identifier except ...>
r5js.Parser.prototype[r5js.parse.Nonterminals.PATTERN_IDENTIFIER] = function() {
    return this.rhs(
        r5js.parse.bnf.matchDatum(function(datum) {
	     /* TODO bl: Tests fail when I replace this type switch by
	        datum.isIdentifier(), suggesting that this argument is not
	        always a Datum. Investigate. */
            return datum.type === r5js.DatumType.IDENTIFIER &&
                datum.payload !== r5js.parse.Terminals.ELLIPSIS;
        }));
};

// <program> -> <command or definition>*
r5js.Parser.prototype[r5js.parse.Nonterminals.PROGRAM] = function() {
    return this.rhs(
        r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.COMMAND_OR_DEFINITION),
        {desugar: function(node, env) {
            return node.sequence(env);
        }
        }
    );
};


/* <command or definition> -> <command>
 | <definition>
 | <syntax definition>
 | (begin <command or definition>*)
 */
r5js.Parser.prototype[r5js.parse.Nonterminals.COMMAND_OR_DEFINITION] = function() {
    return this.alternation_(
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.DEFINITION)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.SYNTAX_DEFINITION)
        ],
        [
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.BEGIN),
            r5js.parse.bnf.zeroOrMore(r5js.parse.Nonterminals.COMMAND_OR_DEFINITION),
            r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN)
        ],
        [
            r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.COMMAND)
        ]);
};


// <command> -> <expression>
r5js.Parser.prototype[r5js.parse.Nonterminals.COMMAND] = function() {
    return this.rhs(r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.EXPRESSION));
};


// <syntax definition> -> (define-syntax <keyword> <transformer-spec>)
r5js.Parser.prototype[r5js.parse.Nonterminals.SYNTAX_DEFINITION] = function() {
    return this.rhs(
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.LPAREN),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.DEFINE_SYNTAX),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.KEYWORD),
        r5js.parse.bnf.oneNonterminal(r5js.parse.Nonterminals.TRANSFORMER_SPEC),
        r5js.parse.bnf.oneTerminal(r5js.parse.Terminals.RPAREN),
        {desugar: function(node, env) {
            var kw = node.at(r5js.parse.Nonterminals.KEYWORD).payload;
            var macro = node.at(r5js.parse.Nonterminals.TRANSFORMER_SPEC).desugar(env);
            if (!macro.allPatternsBeginWith(kw))
                throw new r5js.MacroError(kw, "all patterns must begin with " + kw);
            var anonymousName = newAnonymousLambdaName();
            env.addBinding(anonymousName, macro);
            return r5js.procs.newAssignment(
                kw,
                anonymousName,
                new r5js.Continuation()).
                setTopLevelAssignment().
                setSyntaxAssignment();
        }
        }
    );
};

/**
 * @param {!r5js.parse.Nonterminal=} opt_nonterminal
 * @return {r5js.Datum}
 * TODO bl: why does the compiler not accept an @override here?
 */
r5js.Parser.prototype.parse = function(opt_nonterminal) {
    var fun = this[opt_nonterminal || r5js.parse.Nonterminals.PROGRAM];
    if (fun) {
        var ans = fun.apply(this);

        if (ans && ans.nonterminals) {
            // See comments at top of Parser.
            if (this.fixParserSensitiveIds_) {
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
            return goog.isDef(opt_nonterminal) ? ans : null;
        }
    }
    else
        throw new r5js.InternalInterpreterError('unknown lhs: ' + opt_nonterminal);
};