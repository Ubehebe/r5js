import {Evaluator} from "../../eval/evaluator";
import {Pipeline} from "../../eval/pipeline";
import {CallbackBackedPort} from '../../io/callback_backed_port';
import {NULL_INPUT_PORT} from '../../io/input_port';
import {Repl} from '../../repl/repl';
import {EnvironmentImpl} from "../../runtime/environment_impl";
import {Terminal} from './terminal';

/** The main REPL method. */
export function repl() {
    const stdin = NULL_INPUT_PORT;
    const terminal = new Terminal();
    const stdout = new CallbackBackedPort(output => terminal.print(output));
    const evaluator = new Evaluator(new Pipeline(new EnvironmentImpl(null)), stdin, stdout);
    new Repl(terminal, evaluator).start();
}
