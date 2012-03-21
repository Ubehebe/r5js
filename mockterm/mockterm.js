/* Copyright 2011, 2012 Brendan Linn

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* This is a very quick-and-dirty "terminal emulator" that sits on top
 of an HTML textarea. It's called MockTerminal to make it clear that it
 doesn't provide any actual network connectivity etc., just terminal-like
 styling.

 Before writing this, I tried the existing popular JavaScript "terminal
 emulators" like termlib.js and the jQuery Terminal plugin, but they
 were both dead in the water on the iPad -- you couldn't bring up
 the keyboard.

 todo: This is clearly full of bugs. There are two main reasons:
 (1) the horrible state of keydown API and (2) implementing a character
 device (a terminal) on top of what is essentially a block device
 (the textarea, which was meant to provide snapshots of completed strings
 to the server through forms). Hopefully, once the DOM Level 3 Events
 spec is standardized, we can rewrite this using the proposed textinput event
 or the proposed "key" property of the existing keydown event. */
function MockTerminal(textArea, numColumns, charLatency, lineLatency) {
    this.textArea = textArea;

    /* Properties set by setters
     this.lineStart;
     this.lineEnd;
     this.lineBuf; */

    // May want to customize these, or, if not, move to prototype
    this.inputKey = '\r'.charCodeAt(0);
    this.backspace = '\b'.charCodeAt(0);

    this.prompt = '';
    this.banner = '';

    this.interpreters = [];

    var self = this;

    this.numColumns = numColumns || 80;
    this.printQueue = new AsyncQueue(charLatency || 0);
    this.lineLatency = lineLatency || 0;
    this.recordCharWidth();
    this.resize();

    textArea.addEventListener('keydown', function (e) { self.onKeyDown(e); });
    addEventListener('resize', function() {
        self.resize();
    });
}

MockTerminal.prototype.onKeyDown = function(e) {
    if (this.shouldSuppress(e)) {
        e.preventDefault();
    } else if (this.shouldEndLine(e)) {
        /* The default behavior here would be to print a newline.
         But it would happen after this handler completes, so the prompt
         would have a stray newline after it. So we disable that behavior. */
        e.preventDefault();
        var self = this;
        var input = this.getCurLine();
        var output = this.maybeInterpret(input);
        this.print('\n' + (output || '') + '\n\n' + this.prompt);
        /*The above print call is asynchronous, so we have to update the
         various offsets in the future, not the present. */
        this.printQueue.enqueue(function() {
            self.lineStart
                = self.lineEnd
                = self.textArea.selectionEnd
                = self.textArea.value.length;
        });
    }
};

MockTerminal.prototype.shouldSuppress = function(keydownEvent) {
    /* The current caret of the textarea, before this event makes it
     to the textarea. */
    var cur = this.textArea.selectionEnd;

    /* If the user has selected some text that reaches back before the start
     of the current line, don't do anything until they deselect it.
     The cut/copy/paste semantics are too hard using just the keydown event. */
    if (this.textArea.selectionStart < cur
        && this.textArea.selectionStart < this.lineStart) {
        return true;
    }

    /* If the caret is strictly before the current line, we disallow
     anything that would mutate that text. */
    else if (cur < this.lineStart)
        return this.willMutateText(keydownEvent);

    /* If the caret is right at the beginning of the current line,
     we allow it unless they're trying to backspace. */
    else if (cur === this.lineStart)
        return keydownEvent.keyCode === this.backspace;

    /* Otherwise, we're inside the current line, so the user should
     be allowed to do whatever, as long as we're not currently printing
     to the terminal.

     (Of course, in real terminals, you can type while the terminal
     is printing, but the DOM APIs make it difficult to do this. We would
     like to say, "if a user presses a key while the terminal is printing,
     queue whatever action the key press would have made and do
     it when the printing is over." But there's no good way to figure out
     "what the action would have been" from the keydown API. That's the
     point of the missing else clause in MockTerminal.prototype.onKeyDown:
     we don't know what the action would have been, but we're pretty sure
     it's OK, and we let the browser do it.) */
    else return this.printQueue.isRunning();
};

/* These are just heuristics. It's hard to know in general
 if a keypress will change the contents of a text input; the OS
 is the proper owner of that information. (For example, someone
 could conceivably map shift-X/C/V to cut/copy/paste instead of
 the usualy ctrl-X/C/V.) We also don't want to get in the way of
 useful browser bindings like ctrl-T to open a new tab. */
MockTerminal.prototype.willMutateText = function(e) {
    return (e.altKey || e.metaKey || e.ctrlKey)
        ? false
        : !this.isHarmlessKeyCode(e.keyCode);
};

/* This is taken from Example 17-8 of "JavaScript: The Definitive Guide",
 6th ed. (page 488). The comment to that source code reads:
 "The legacy keyCode property of the keydown event object is not standardized.
 But the following values seem to work for most browsers and OSes." */
MockTerminal.prototype.isHarmlessKeyCode = function(keyCode) {
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
};

MockTerminal.prototype.println = function(line) {
    /* If line is an array, that means we should print out each element
     separately. Just for convenience so clients don't have to insert
     newlines manually. */
    if (line instanceof Array) {
        for (var i = 0; i < line.length; ++i)
            this.println(line[i]).println('').pause(this.lineLatency);
    } else if (typeof line === 'function') {
        this.println(line());
    } else {
        this.print('\n' + line);
    }
    return this;
};

MockTerminal.prototype.pause = function(ms) {
    var periodsToPause = Math.floor(Math.abs(ms/this.printQueue.latency));
    for (var i=0; i<periodsToPause; ++i)
        this.printQueue.enqueue(function() {});
    return this;
};

/* In order to achieve the "crappy high latency terminal" effect,
 this function is asynchronous, though it presents a synchronous
 interface to the programmer. */
MockTerminal.prototype.print = function(string) {

    var self = this;
    var wrapped = new StrWrapper(string);

    for (var i=0; i<string.length; ++i) {
    this.printQueue.enqueue(function() {
        self.textArea.value += wrapped.next();
        /* Make sure we don't have to scroll down to see the latest output.
         Not sure how portable this is. */
        self.textArea.scrollTop = self.textArea.scrollHeight;
    });
    }

    this.printQueue.enqueue(function() {
        self.textArea.selectionEnd = self.textArea.value.length;
    });

    function StrWrapper(str) {
        this.str = str;
        this.offset = 0;
    }

    StrWrapper.prototype.next = function() {
        return this.str.charAt(this.offset++);
    };

    return this;
};

MockTerminal.prototype.shouldEndLine = function(e) {

    var lineEnd = this.textArea.selectionEnd;

    if (lineEnd < this.lineStart) {
        return false;
    } else {
        this.lineEnd = lineEnd; // an important side effect
        return e.keyCode === this.inputKey;
    }
};

MockTerminal.prototype.getCurLine = function() {
    return this.textArea.value.substr(
        this.lineStart,
        this.lineEnd - this.lineStart + 1);
};

MockTerminal.prototype.pushInterpreter = function(interpreter) {
    this.interpreters.push(interpreter);
    return this;
};

MockTerminal.prototype.popInterpreter = function() {
    var ans = this.interpreters.pop();
    return ans;
};

MockTerminal.prototype.setInputCompleteHandler = function(inputCompleteHandler) {
    this.inputCompleteHandler = inputCompleteHandler;
    return this;
};

MockTerminal.prototype.maybeInterpret = function(string) {

    this.lineBuf = this.lineBuf
        ? this.lineBuf + '\n' + string
        : string;

    if (this.inputCompleteHandler
        && !this.inputCompleteHandler(this.lineBuf)) {
        return '...';
    } else {
        var input = this.lineBuf;
        this.lineBuf = null;
        try {
            /* Go back down the stack of interpreters. The most recently
             pushed interpreter is the one we should consult first. */
            for (var i = this.interpreters.length - 1; i >= 0; --i) {
                var result = this.interpreters[i](input, this);
                if (result)
                    return result;
            }
        } catch (e) {
            return e.toString();
        }
    }
};

MockTerminal.prototype.start = function () {
    this.println(this.prompt);
    var self = this;
    this.printQueue.enqueue(function () {
        self.lineStart
            = self.textArea.selectionStart
            = self.textArea.selectionEnd
            = self.textArea.value.length;
        self.lineEnd = self.lineStart + 1;
        self.lineBuf = null;
    });
    return this;
};

MockTerminal.prototype.reset = function() {
    var self = this;
    this.printQueue.enqueue(function() {
        self.textArea.value = '';
        self.lineStart
                = self.textArea.selectionStart
                = self.textArea.selectionEnd
                = self.textArea.value.length;
            self.lineEnd = self.lineStart+1;
            self.lineBuf = null;
    });
    return this;
};

MockTerminal.prototype.recordCharWidth = function () {
    /* Note that this assumes that the font style of the newly created
     span will be identical to the textarea's font style. May want to
     document. */
    var charSandbox = document.createElement('span');
    charSandbox.style.visibility = 'hidden';
    charSandbox.appendChild(document.createTextNode('X'));
    document.body.appendChild(charSandbox);
    var box = charSandbox.getBoundingClientRect();
    this.charHtoW = box.height / box.width;
};

MockTerminal.prototype.resize = function () {
    var width = this.textArea.getBoundingClientRect().width;
//    console.log('width ' + width);
    var charWidth = width / this.numColumns;
    var charHeight = charWidth * this.charHtoW;
    this.textArea.style.fontSize = charHeight + 'px';
//    console.log('each char should be ' + charHeight);
};

MockTerminal.prototype.setPrompt = function (prompt) {
    this.prompt = prompt;
    return this;
};