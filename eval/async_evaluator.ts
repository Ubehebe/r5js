import {InputPort, NULL_INPUT_PORT} from "../io/input_port";
import {NULL_OUTPUT_PORT, OutputPort} from "../io/output_port";
import {SchemeSources} from "../scm/scheme_sources";
import {boot} from "./boot";
import {Evaluator} from "./evaluator";

/** Wraps a synchronous evaluator in promises. */
export class AsyncEvaluator {

  private readonly evaluator: Evaluator;

  constructor(inputPort: InputPort = NULL_INPUT_PORT, outputPort: OutputPort = NULL_OUTPUT_PORT) {
    const sources = new SchemeSources();
    this.evaluator = boot(sources.syntax, sources.procedures, inputPort, outputPort);
  }

  /**
   * @param input Input to evaluate.
   * @return If evaluation succeeds, this promise will be resolved with a string representation
   * of the Scheme value (as if it was serialized with the {@code write} procedure).
   * If evaluation fails, the promise will be rejected with an {@link r5js.Error} explaining what
   * went wrong.
   */
  evaluate(input: string): Promise<string> {
    try {
      return Promise.resolve(this.evaluator.evaluate(input));
    } catch (e) {
      return Promise.reject(e);
    }
  }
}
