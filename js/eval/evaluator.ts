import {Pipeline} from "./pipeline";
import {InputPort} from "../io/input_port";
import {OutputPort} from "../io/output_port";
import {toWriteString} from "../runtime/valutil";

export class Evaluator {

  constructor(
    private readonly pipeline: Pipeline,
    private readonly inputPort: InputPort,
    private readonly outputPort: OutputPort) {}

  evaluate(input: string): string {
    return toWriteString(
      this.pipeline.Eval(
        this.pipeline.desugar(
          this.pipeline.parse(
            this.pipeline.read(
              this.pipeline.scan(input)))),
        this.inputPort,
        this.outputPort));
  }
}