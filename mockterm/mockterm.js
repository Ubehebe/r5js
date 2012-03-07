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
 to the server through forms). Hopefully, the HTML5 <input> API can remedy
 the situation. */
function MockTerminal(textArea) {
    this.hasFocus = textArea.autofocus;
    this.entryCode = 13; // enter. customize?
    this.numColumns = 80; // customize?
    this.textArea = textArea;
    this.prompt = '';
    this.banner = '';
    this.inputHandlers = [];

    this.inputKey = '\n'.charCodeAt(0);

    var self = this;

    textArea.addEventListener('focusin', function () {
        self.setFocus();
    });
    textArea.addEventListener('focusout', function () {
        self.unsetFocus();
    });

    window.addEventListener('resize', function () {
        self.resize();
    });

    window.addEventListener('keydown', function (e) { self.onKeyDown(e); });
}

MockTerminal.prototype.onKeyDown = function (e) {

    if (this.hasFocus) {
        if (e.keyCode === this.entryCode) {
            var input = this.textArea.value.substr(this.curStart, this.curStop - this.curStart + 1);
//                console.log('input: from ' + self.curStart + ' to ' + self.curStop + ': [' + input + ']');
            var output;
            for (var i=0; i < this.inputHandlers.length; ++i) {
                try {
                if (output = this.inputHandlers[i](input))
                    break;
                } catch (x) {
                    output = x.toString();
                    break;
                }
            }
            output = '\n' + output + '\n' + this.prompt;
            this.textArea.value += output;
            this.curStart = this.curStop + output.length + 2;
            if (input.length === 0)
                --this.curStart;
            this.curStop = this.oldLen = this.curStart;
        } else if (this.textArea.value.length === this.oldLen + 1) {
            ++this.oldLen;
            ++this.curStop;
        }
    }
};

MockTerminal.prototype.addInputHandler = function(closure) {
    this.inputHandlers.push(closure);
    return this;
};

MockTerminal.prototype.start = function () {
    this.textArea.defaultValue = this.banner + '\n' + this.prompt;
    this.curStart = this.curStop = this.oldLen = this.textArea.defaultValue.length;
    return this;
};

MockTerminal.prototype.recordCharWidth = function () {
    var charSandbox = document.createElement('span');
    charSandbox.className = this.textArea.className;
    charSandbox.style.visibility = 'hidden';
    charSandbox.appendChild(document.createTextNode('x'));
    charSandbox.style.marginRight = 'inherit';
    document.body.appendChild(charSandbox);
    var box = charSandbox.getBoundingClientRect();
    this.charHtoW = box.height / box.width;
};

MockTerminal.prototype.resize = function () {
    var width = this.textArea.getBoundingClientRect().width;
    console.log('width ' + width);
    var charWidth = width / this.numColumns;
    var charHeight = charWidth * this.charHtoW;
    this.textArea.style.fontSize = charHeight + 'px';
    console.log('each char should be ' + charHeight);
};

MockTerminal.prototype.setBanner = function (banner) {
    this.banner = banner;
    return this;
};

MockTerminal.prototype.setPrompt = function (prompt) {
    this.prompt = prompt;
    return this;
};

MockTerminal.prototype.setFocus = function () {
    console.log('setFocus');
    this.hasFocus = true;
    return this;
};

MockTerminal.prototype.unsetFocus = function () {
    console.log('unsetFocus');
    this.hasFocus = false;
    return this;
};
