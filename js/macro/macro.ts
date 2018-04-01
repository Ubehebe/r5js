import {Datum} from "../ast/datum";
import {Transformer} from "./transformer";
import {Identifier} from "../ast/identifier";
import {CompoundDatum} from "../ast/compound_datum";
import {PATTERN, PROGRAM, TEMPLATE} from "../parse/nonterminals";
import {TemplateBindings} from "./template_bindings";
import {Parser} from "../parse/parser";
import {isParserSensitiveId, newCpsName} from "../parse/rename_util";
import {SiblingBuffer} from "../ast/sibling_buffer";
import {List} from "../ast/list";
import {Error} from "../error";
import {ObjectValue} from "../value";
import {Environment} from "../runtime/environment";
import {ProcCallLike, ProcCallResult} from "../ast/proc_call_like";

export class Macro implements ObjectValue /* TODO bl almost certainly wrong */ {

  private readonly literalIdentifiers_: { [key: string]: boolean } = {};
  private isLetOrLetrecSyntax_: boolean = false;

  constructor(literalIdentifiers: Datum | null,
              rules: Datum | null,
              private definitionEnv: Environment,
              private readonly transformers: Transformer[] = []) {

    for (let curId = literalIdentifiers; curId; curId = curId.getNextSibling()) {
      this.literalIdentifiers_[(curId as Identifier).getPayload()] = true;
    }

    if (!transformers.length) {
      for (let rule = rules; rule; rule = rule.getNextSibling()) {
        // TODO bl improve
        const compoundDatum = rule as CompoundDatum;
        const pattern = compoundDatum.at(PATTERN)!.desugar(definitionEnv);
        const template = compoundDatum.at(TEMPLATE)!.desugar(definitionEnv);
        const transformer = new Transformer(pattern, template);
        this.transformers.push(transformer);
      }
    }
  }

  /**
   * Workaround for let-syntax and letrec-syntax.
   * This implementation rewrites let-syntax and letrec-syntax
   * as let and letrec respectively. For example,
   *
   * (let-syntax ((foo (syntax-rules () ((foo) 'hi)))) ...)
   *
   * desugars as
   *
   * (let ((foo [SchemeMacro object wrapped in a Datum])) ...)
   *
   * When this macro use is matched against the definition of let,
   * the wrapped SchemeMacro object will be added to the TemplateBindings
   * correctly. The problem arises during transcription: we cannot insert
   * the wrapped SchemeMacro object directly into the new parse tree,
   * because that parse tree will be handed to the parser, which won't know
   * what to do with SchemeMacros.
   *
   * Indirection comes to the rescue. We insert a new identifier node and
   * bind it in the current environment to the SchemeMacro. Later, on
   * the trampoline, we will look up the value of that identifier and
   * find the SchemeMacro as desired.
   *
   * We have to set the isLetOrLetrecSyntax flag on the macro to disallow
   * this behavior from the programmer. We cannot allow her to write
   *
   * (let ((x let*)) x)
   */
  setIsLetOrLetrecSyntax(): this {
    this.isLetOrLetrecSyntax_ = true;
    return this;
  }

  isLetOrLetrecSyntax(): boolean {
    return this.isLetOrLetrecSyntax_;
  }

  setDefinitionEnv(env: Environment) {
    this.definitionEnv = env;
  }

  /**
   * Should only be used during interpreter bootstrapping.
   * @return A clone of this macro.
   */
  clone(newDefinitionEnv: Environment): Macro {
    return new Macro(null, null, newDefinitionEnv, this.transformers);
  }

  /** @return True iff all of the macro's patterns begin with kw. */
  allPatternsBeginWith(kw: string): boolean {
    for (let i = 0; i < this.transformers.length; ++i) {
      if (this.transformers[i].getName() !== kw) {
        return false;
      }
    }
    return true;
  }

  /**
   * @param datum Datum to transcribe.
   * @param useEnv Environment to use for the transcription.
   * @param parserProvider Function that will return a new Parser for the given Datum.
   *     This is a hack to avoid instantiating a Parser directly in this file, which would cause
   *     a cyclic dependency between macro.js and parse.js.
   */
  transcribe(datum: Datum, useEnv: Environment, parserProvider: (Datum) => Parser) {
    let transformer, bindings, newDatumTree;
    for (let i = 0; i < this.transformers.length; ++i) {
      transformer = this.transformers[i];
      bindings = new TemplateBindings(
          useEnv,
          transformer.getPatternIds(),
          transformer.getTemplateRenameCandidates());
      if (transformer.matchInput(
              datum,
              this.literalIdentifiers_,
              this.definitionEnv,
              useEnv,
              bindings) &&
          (newDatumTree = transformer.getTemplate().toDatum(bindings))) {
        // this is a good place to see the TemplateBindings object
        // console.log(bindings.toString());

        const newParseTree = parserProvider(newDatumTree).parse(PROGRAM);

        /* R5RS 4.3: "If a macro transformer inserts a binding for an identifier
         (variable or keyword), the identifier will in effect be renamed
         throughout its scope to avoid conflicts with other identifiers.

         "If a macro transformer inserts a free reference to an
         identifier, the reference refers to the binding that was visible
         where the transformer was specified, regardless of any local bindings
         that may surround the use of the macro."

         It's easy to collect the set of identifiers inserted by a macro
         transformer: it's the set of identifiers in the template minus the set
         of identifiers in the pattern. But how do we determine which of these
         are supposed to be "free" and which are bindings and thus should be
         renamed?

         My current heuristic is to do a lookup in the macro's definition
         environment. If we find something, the identifier is probably
         supposed to refer to that. For example, the "+" in the pattern of

         (define-syntax foo (syntax-rules () ((foo x) (+ x x))))

         If we don't find a binding in the macro's definition environment, we
         suppose this is a new binding inserted by the transformer and
         defensively rename it.

         I don't think this is correct, but it works for the letrec
         macro definition, which is the most complex case I've tried so far. */
        const toRename = {};
        const candidates = transformer.getTemplateRenameCandidates();
        for (const id in candidates) {
          if (this.definitionEnv.hasBindingRecursive(id)) {
            // TODO: this is deeply weird. Because of this line,
            // IEnvironment has to extend ObjectValue. Which it shouldn't.
            // Find another way to do this.
            useEnv.addBinding(id, this.definitionEnv);
          } else if (!isParserSensitiveId(id)) {
            const tmpName = newCpsName();
            toRename[id] = tmpName;
            /* If the TemplateBindings object has detected that the same
             identifier is used in the input and (unrelatedly) in the template,
             id may be replaced in the template, so we have to manually add
             the binding here. See the logic at the end of
             TemplateBindings.prototype.addTemplateBinding. */
            if (bindings.wasRenamed(id) && useEnv.hasBindingRecursive(id)) {
              const binding = useEnv.get(id);
              if (binding !== null) {
                useEnv.addBinding(tmpName, binding);
              }
            }
          }
        }

        if (newParseTree) {
          /* We have to embed the new parse tree in a fake shell to do the
           replacement in case the entire newParseTree is an identifier that
           needs to be replaced (Datum.prototype.replaceChildren() only
           looks at a node's children).

           This is a problem that has surfaced more than once, so perhaps
           there is a better way to write replaceChildren.

           todo bl: we should be able to determine the id's in the template
           that will have to be renamed prior to transcription. That would
           save the following tree walk replacing all the identifiers. */
          const fake = new SiblingBuffer()
              .appendSibling(newParseTree)
              .toList(List);
          fake.replaceChildren(
              function (node) {
                return node instanceof Identifier && toRename[node.getPayload()];
              },
              function (node) {
                node.setPayload(toRename[node.getPayload()]);
                return node;
              }
          );
        } else {
          throw Error.parse(newDatumTree);
        }

        return newParseTree;
      }
    }
    throw Error.macro(
        this.transformers[0].getName(), 'no pattern match for input ' + datum);
  }

  evaluate(
      rawDatum: Datum,
      procCallLike: ProcCallLike,
      resultStruct: ProcCallResult,
      parserProvider: (Datum) => Parser) {
    const oldEnv = procCallLike.getEnv()!;
    const newEnv = oldEnv.child();
    const newParseTree = this.transcribe(rawDatum, newEnv, parserProvider);

    const next = procCallLike.getNext();
    /* Just like with tryNonPrimitiveProcedures, we have to remember when
     to jump back to the old environment. */
    if (oldEnv && next && !next.getEnv()) {
      next.setStartingEnv(oldEnv);
    }

    const newContinuable = /** @type {!ProcCallLike} */ (
        newParseTree.desugar(newEnv, true));
    newContinuable.setStartingEnv(newEnv);

    const last = newContinuable.getLast();
    if (next) {
      last.setNext(next);
    }
    last.setResultName(procCallLike.getResultName());
    resultStruct.setNext(newContinuable);
  }
}