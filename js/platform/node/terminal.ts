import {Interface} from "readline";
import {Terminal as TerminalInterface} from '../../repl/terminal';

const readline = require('readline');

export class Terminal implements TerminalInterface {

  private readonly readline_: Interface;

  constructor() {
    this.readline_ = readline.createInterface({
      input: process.stdin,
      output: process.stdout,
      terminal: true,
      completer: () => {}
    });
    this.readline_.setPrompt('>> ');
    this.readline_.on('close', () => process.exit(0));
    this.readline_.prompt();
  }

  /** @override */
  getNextLineOfInput() {
    return new Promise<string>((resolve) => {
      this.readline_.once('line', resolve);
    });
  }

  /** @override */
  print(str: string) {
    console.log(str);
    this.readline_.prompt(); // TODO bl double-prompts on Scheme output
  }

  /** @override */
  error(str: string) {
    console.error(str);
    this.readline_.prompt(); // TODO bl double-prompts on Scheme output
  }
}
