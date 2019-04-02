import {AsyncQueue} from "./async_queue";

const INPUT_KEY = '\r'.charCodeAt(0);
const BACKSPACE = '\b'.charCodeAt(0);

export type Interpreter = (input: string, terminal: MockTerminal) => string|null;

/**
 * This is a very quick-and-dirty "terminal emulator" that sits on top of an HTML textarea. It's
 * called MockTerminal to make it clear that it doesn't provide any actual network connectivity
 * etc., just terminal-like styling.
 *
 * Before writing this, I tried the existing popular JavaScript "terminal emulators" like termlib.js
 * and the jQuery Terminal plugin, but they were both dead in the water on the iPad -- you couldn't
 * bring up the keyboard.
 *
 * TODO: This is clearly full of bugs. There are two main reasons: (1) the horrible state of keydown
 * API and (2) implementing a character device (a terminal) on top of what is essentially a block
 * device (the textarea, which was meant to provide snapshots of completed strings to the server
 * through forms). Hopefully, once the DOM Level 3 Events spec is standardized, we can rewrite this
 * using the proposed textinput event or the proposed "key" property of the existing keydown event.
 */
export class MockTerminal {

  private prompt = "";
  private readonly interpreters: Interpreter[] = [];
  private readonly printQueue: AsyncQueue;

  // Properties set by setters
  private lineStart = 0;
  private lineEnd = 0;
  private lineBuf: string|null = "";
  private charHtoW = 0;
  private inputCompleteHandler: ((_: string) => boolean) | undefined;

  constructor(
      private readonly textArea: HTMLTextAreaElement,
      private readonly numColumns = 80,
      charLatency = 0,
      private readonly lineLatency = 0) {

    this.printQueue = new AsyncQueue(charLatency);
    this.recordCharWidth();
    this.resize();

    textArea.addEventListener('keydown', (e) => this.onKeyDown(e), false);
    addEventListener('resize', () => this.resize(), false);
  }

  private onKeyDown(e: KeyboardEvent) {
    if (this.shouldSuppress(e)) {
      e.preventDefault();
    } else if (this.shouldEndLine(e)) {
      // The default behavior here would be to print a newline. But it would happen after this
      // handler completes, so the prompt  would have a stray newline after it. So we disable that
      // behavior.
      e.preventDefault();
      const input = this.getCurLine();
      const output = this.maybeInterpret(input);
      this.print('\n' + (output || '') + '\n\n' + this.prompt);
      // The above print call is asynchronous, so we have to update the various offsets in the
      // future, not the present.
      this.printQueue.enqueue(() => this.lineStart
          = this.lineEnd
          = this.textArea.selectionEnd
          = this.textArea.value.length);
    }
  }

  private shouldSuppress(keydownEvent: KeyboardEvent): boolean {
    // The current caret of the textarea, before this event makes it to the textarea.
    const cur = this.textArea.selectionEnd;

    if (this.textArea.selectionStart < cur && this.textArea.selectionStart < this.lineStart) {
      // If the user has selected some text that reaches back before the start of the current line,
      // don't do anything until they deselect it. The cut/copy/paste semantics are too hard using
      // just the keydown event.
      return true;
    } else if (cur < this.lineStart) {
      // If the caret is strictly before the current line, we disallow anything that would mutate that
      // text.
      return willMutateText(keydownEvent);
    } else if (cur === this.lineStart) {
      // If the caret is right at the beginning of the current line, we allow it unless they're
      // trying to backspace.
      return keydownEvent.keyCode === BACKSPACE;
    } else {
      // Otherwise, we're inside the current line, so the user should be allowed to do whatever, as
      // long as we're not currently printing to the terminal. (Of course, in real terminals, you
      // can type while the terminal is printing, but the DOM APIs make it difficult to do this. We
      // would like to say, "if a user presses a key while the terminal is printing, queue whatever
      // action the key press would have made and do it when the printing is over." But there's no
      // good way to figure out "what the action would have been" from the keydown API. That's the
      // point of the missing else clause in onKeyDown: we don't know what the action would have
      // been, but we're pretty sure it's OK, and we let the browser do it.)
      return this.printQueue.isRunning();
    }
  }

  println(line: any): this {
    // If line is an array, that means we should print out each element separately. Just for
    // convenience so clients don't have to insert newlines manually.
    if (line instanceof Array) {
      for (const l of line) {
        this.println(l).println('').pause(this.lineLatency);
      }
    } else if (typeof line === 'function') {
      this.println(line());
    } else if (line.bannerToString) {
      // If line is an object that has a bannerToString() function, use that because MockTerminal
      // supports displaying banners.
      this.println(line.bannerToString());
    } else if (typeof line !== 'string' && line.toString) {
      this.println(line.toString());
    } else {
      this.print('\n' + line);
    }
    return this;
  }

  private pause(ms: number): this {
    const periodsToPause = Math.floor(Math.abs(ms / this.printQueue.latency));
    for (let i = 0; i < periodsToPause; ++i) {
      this.printQueue.enqueue(() => {});
    }
    return this;
  }

  /**
   * In order to achieve the "crappy high latency terminal" effect, this function is asynchronous,
   * though it presents a synchronous interface to the programmer.
   */
  private print(string: string) {
    const wrapped = new StrWrapper(string);

    for (const s of string) {
      this.printQueue.enqueue(() => {
        this.textArea.value += wrapped.next();
        // Make sure we don't have to scroll down to see the latest output. Not sure how portable
        // this is.
        this.textArea.scrollTop = this.textArea.scrollHeight;
      });
    }

    this.printQueue.enqueue(() => this.textArea.selectionEnd = this.textArea.value.length);
    return this;
  }

  private shouldEndLine(e: KeyboardEvent): boolean {
    const lineEnd = this.textArea.selectionEnd;

    if (lineEnd < this.lineStart) {
      return false;
    } else {
      this.lineEnd = lineEnd; // an important side effect
      return e.keyCode === INPUT_KEY;
    }
  }

  private getCurLine(): string {
    return this.textArea.value.substr(
        this.lineStart,
        this.lineEnd - this.lineStart + 1);
  }

  pushInterpreter(interpreter: Interpreter): this {
    this.interpreters.push(interpreter);
    return this;
  }

  popInterpreter(): Interpreter|undefined {
    return this.interpreters.pop();
  }

  setInputCompleteHandler(inputCompleteHandler: (_: string) => boolean): this {
    this.inputCompleteHandler = inputCompleteHandler;
    return this;
  }

  private maybeInterpret(string: string): any {
    this.lineBuf = this.lineBuf
        ? this.lineBuf + '\n' + string
        : string;

    if (this.inputCompleteHandler
        && !this.inputCompleteHandler(this.lineBuf)) {
      return '...';
    } else {
      const input = this.lineBuf;
      this.lineBuf = null;
      try {
        // Go back down the stack of interpreters. The most recently pushed interpreter is the one
        // we should consult first.
        for (let i = this.interpreters.length - 1; i >= 0; --i) {
          const result = this.interpreters[i](input, this);
          if (result) {
            return result;
          }
        }
      } catch (e) {
        return e.toString();
      }
    }
  }

  start(): this {
    this.println(this.prompt);
    this.printQueue.enqueue(() => {
      this.lineStart
          = this.textArea.selectionStart
          = this.textArea.selectionEnd
          = this.textArea.value.length;
      this.lineEnd = this.lineStart + 1;
      this.lineBuf = null;
    });
    return this;
  }

  reset(): this {
    this.printQueue.enqueue(() => {
      this.textArea.value = '';
      this.lineStart
          = this.textArea.selectionStart
          = this.textArea.selectionEnd
          = this.textArea.value.length;
      this.lineEnd = this.lineStart + 1;
      this.lineBuf = null;
    });
    return this;
  }

  private recordCharWidth() {
    // Note that this assumes that the font style of the newly created span will be identical to the
    // textarea's font style. May want to document.
    const charSandbox = document.createElement('span');
    charSandbox.style.visibility = 'hidden';
    charSandbox.appendChild(document.createTextNode('X'));
    document.body.appendChild(charSandbox);
    const box = charSandbox.getBoundingClientRect();
    this.charHtoW = box.height / box.width;
  }

  resize() {
    const width = this.textArea.getBoundingClientRect().width;
    const charWidth = width / this.numColumns;
    const charHeight = charWidth * this.charHtoW;
    this.textArea.style.fontSize = `${charHeight}px`;
  }

  setPrompt(prompt: string): this {
    this.prompt = prompt;
    return this;
  }
}

class StrWrapper {
  private offset = 0;

  constructor(private readonly str: string) {
  }

  next(): string {
    return this.str.charAt(this.offset++);
  }
}

function willMutateText(e: KeyboardEvent): boolean {
  // These are just heuristics. It's hard to know in general if a keypress will change the
  // contents of a text input; the OS is the proper owner of that information. (For example,
  // someone could conceivably map shift-X/C/V to cut/copy/paste instead of the usual ctrl-X/C/V.)
  // We also don't want to get in the way of useful browser bindings like ctrl-T to open a new
  // tab.
  return (e.altKey || e.metaKey || e.ctrlKey)
      ? false
      : !isHarmlessKeyCode(e.keyCode);
}

/**
 * This is taken from Example 17-8 of "JavaScript: The Definitive Guide", 6th ed. (page 488). The
 * comment to that source code reads: "The legacy keyCode property of the keydown event object is
 * not standardized. But the following values seem to work for most browsers and OSes."
 */
function isHarmlessKeyCode(keyCode: number): boolean {
  switch (keyCode) {
    case 16: // shift
    case 17: // control
    case 18: // alt
    case 19: // pause
    case 20: // caps lock
    case 27: // escape
    case 33: // page up
    case 34: // page down
    case 35: // end
    case 36: // home
    case 37: // left
    case 38: // up
    case 39: // right
    case 40: // down
    case 45: // insert
    case 112: // F1
    case 113: // F2
    case 114: // F3
    case 115: // F4
    case 116: // F5
    case 117: // F6
    case 118: // F7
    case 119: // F8
    case 120: // F9
    case 121: // F10
    case 122: // F11
    case 123: // F12
    case 124: // F13
    case 125: // F14
    case 126: // F15
    case 127: // F16
    case 128: // F17
    case 129: // F18
    case 130: // F19
    case 131: // F20
    case 132: // F21
    case 133: // F22
    case 134: // F23
    case 135: // F24
      return true;
    default:
      return false;
  }
}
