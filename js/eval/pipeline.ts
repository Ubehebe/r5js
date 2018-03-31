import {Environment} from "../runtime/environment";
import {Error} from "../error";
import {newTokenStream, TokenStream} from "../scan/token_stream";
import {Datum, ProcCallLike, UNSPECIFIED_VALUE, VACUOUS_PROGRAM} from "../ast/datum";
import {newReader, Reader} from "../read/reader";
import {ParserImpl} from "../parse/parser_impl";
import {Nonterminal, PROGRAM} from "../parse/nonterminals";
import {InputPort} from "../io/input_port";
import {OutputPort} from "../io/output_port";
import {trampoline} from "../runtime/trampoline";

export class Pipeline {

  private readonly env: IEnvironment;

  constructor(rootEnv: IEnvironment) {
    this.env = new Environment(rootEnv);
  }

  scan(string: string): TokenStream {
    return newTokenStream(string);
  }

  read(tokenStream: TokenStream): Datum {
    return newReader(tokenStream).read();
  }

  /** @param nonterminal The nonterminal that should be the root of the parse tree. */
  parse(root: Datum, nonterminal: Nonterminal = PROGRAM): Datum {
    const ans = new ParserImpl(root).parse(nonterminal);
    if (!ans) {
      throw Error.parse(root);
    }
    return ans;
  }

  desugar(root: Datum): ProcCallLike {
    return /** @type {!ProcCallLike} */ (root.desugar(this.env, false));
  }

  Eval(continuable: ProcCallLike, inputPort: InputPort, outputPort: OutputPort): Value {
    // VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of
    // a special case that I don't care.
    return continuable as any === VACUOUS_PROGRAM
      ? UNSPECIFIED_VALUE
      : trampoline(continuable, this.env, inputPort, outputPort);
  }
}