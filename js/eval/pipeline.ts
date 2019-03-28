import {Datum, UNSPECIFIED_VALUE, VACUOUS_PROGRAM} from "../ast/datum";
import {ProcCallLike} from "../ast/proc_call_like";
import {Error} from "../error";
import {InputPort} from "../io/input_port";
import {OutputPort} from "../io/output_port";
import {Nonterminal, PROGRAM} from "../parse/nonterminals";
import {ParserImpl} from "../parse/parser_impl";
import {newReader, Reader} from "../read/reader";
import {Environment} from "../runtime/environment";
import {EnvironmentImpl} from "../runtime/environment_impl";
import {trampoline} from "../runtime/trampoline";
import {newTokenStream, TokenStream} from "../scan/token_stream";
import {Value} from "../value";

export class Pipeline {

  private readonly env: Environment;

  constructor(rootEnv: Environment) {
    this.env = new EnvironmentImpl(rootEnv);
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
    return root.desugar(this.env);
  }

  Eval(continuable: ProcCallLike, inputPort: InputPort, outputPort: OutputPort): Value {
    // VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of a special case that I don't care.
    return continuable as any === VACUOUS_PROGRAM
      ? UNSPECIFIED_VALUE
      : trampoline(continuable, this.env, inputPort, outputPort);
  }
}
