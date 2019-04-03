import {boot} from "../eval/boot";
import {isLineComplete} from "../repl/replutil";
import {SchemeSources} from "../scm/scheme_sources";
import {MockTerminal} from "./mockterm";

document.addEventListener('DOMContentLoaded', main, false);

function main() {
  manualResize();  // get the nav and hero text looking good
  setupTerminal();
}

function setupTerminal() {
  const textArea = document.getElementById('play')! as HTMLTextAreaElement;
  const sources = new SchemeSources();
  const evaluator = boot(sources.syntax, sources.procedures);
  new MockTerminal(textArea, 80, 5, 500)
      .println(";; r5js") // TODO display banner
      .setPrompt('>> ')
      .pushInterpreter((string: string, terminal: MockTerminal) => evaluator.evaluate(string))
      .setInputCompleteHandler(isLineComplete)
      .start();
}

function manualResize() {
  const e = document.createEvent('Event');
  e.initEvent('resize', false, false);
  dispatchEvent(e);
}
