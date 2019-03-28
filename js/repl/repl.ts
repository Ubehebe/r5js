import {Evaluator} from '../eval/evaluator';
import {isLineComplete} from "./replutil";
import {Terminal} from './terminal';

export class Repl {

    private awaitingEval = "";

    constructor(
        private readonly terminal: Terminal,
        private readonly evaluator: Evaluator) {}

    /** Starts the read-eval-print loop. */
    async start() {
        const line = await this.terminal.getNextLineOfInput();
        await this.handleInputLine(line);
    }

    private async handleInputLine(inputLine: string) {
        if (isLineComplete(this.awaitingEval += inputLine + ' ')) {
            const toEval = this.awaitingEval;
            this.awaitingEval = '';
            const val = this.evaluator.evaluate(toEval);
            this.terminal.print(val);
        }
        const next = await this.terminal.getNextLineOfInput();
        await this.handleInputLine(next);
        // TODO: print errors
    }
}
