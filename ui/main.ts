import {boot} from "../eval/boot";
import {isLineComplete} from "../repl/replutil";
import {SchemeSources} from "../scm/scheme_sources";
import {MockTerminal} from "./mockterm";
import {BANNER_STAMPED as banner} from "./banner";

document.addEventListener('DOMContentLoaded', main, false);

async function main() {
  const sources = new SchemeSources();
  const evaluator = boot(sources.syntax, sources.procedures);
  const terminal = new MockTerminal({
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
    charLatency: 10,
    lineLatency: 500,
  });
  await terminal.println(banner);
  await terminal.start();
}
