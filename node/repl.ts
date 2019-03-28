import {boot} from "../eval/boot";
import {CallbackBackedPort} from '../io/callback_backed_port';
import {NULL_INPUT_PORT} from '../io/input_port';
import {Repl} from '../repl/repl';
import {SchemeSources} from "../scm/scheme_sources";
import {Terminal} from './terminal';

/** The main REPL method. */
export function repl() {
    const stdin = NULL_INPUT_PORT;
    const terminal = new Terminal();
    const stdout = new CallbackBackedPort(output => terminal.print(output));
    const sources = new SchemeSources();
    const evaluator = boot(sources.syntax, sources.procedures, stdin, stdout);
    new Repl(terminal, evaluator).start();
}

repl();
