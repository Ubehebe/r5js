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
  const sources = new SchemeSources();
  const evaluator = boot(sources.syntax, sources.procedures);
  new MockTerminal({
    // Attach the MockTerminal to the first <textarea> in the document.
    // TODO: this is brittle. If the caller was vanilla JS, we could require the caller to pass in
    // the textarea element (dependency injection). But main() has no caller; it is executed as a
    // side effect of loading this module. Revisit this once the project's ES module infrastructure
    // is more mature.
    textArea: document.getElementsByTagName('textarea')[0] as HTMLTextAreaElement,
    interpreter: (string: string, terminal: MockTerminal) => evaluator.evaluate(string),
    prompt: '>> ',
    inputCompleteHandler: isLineComplete,
    numColumns: 80,
    charLatency: 5,
    lineLatency: 500})
      .println(";; r5js") // TODO display banner
      .start();
}

function manualResize() {
  const e = document.createEvent('Event');
  e.initEvent('resize', false, false);
  dispatchEvent(e);
}
