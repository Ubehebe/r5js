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

/* This writes terminal input and output to the DOM. For example,
 you could use it to achieve an iPhone SMS-like effect, where input
 is echoed as text bubbles down the right side and output appears as
 different-colored text bubbles down the left side. This kind of UI may be
 more appropriate for REPL-like applications on mobile devices than
 emulating an entire terminal within a single textarea. */
function BlockTerm(textInput, outputContainer, latency) {

    this.textInput = textInput;
    this.outputContainer = outputContainer;
    this.interpreters = [];
    this.printQueue = new AsyncQueue(latency || 0);
    // Echoing should not have any latency
    this.echoQueue = new AsyncQueue(0);

    this.inputKey = '\r'.charCodeAt(0);

    var self = this;
    var cb = function(e) {
        if (e.keyCode === self.inputKey)
            self.onInputComplete();
    };

    textInput.addEventListener
        ? textInput.addEventListener('keydown', cb, false)
        : textInput.attachEvent('onkeydown', cb);
}

BlockTerm.prototype.setEchoTemplate = function(element) {
    this.echoTemplate = element;
    return this;
};

BlockTerm.prototype.setOutputTemplate = function(element) {
    this.outputTemplate = element;
    return this;
};

BlockTerm.prototype.onInputComplete = function() {
    var input = this.textInput.value;
    // Only echo input if we've explicitly set an echo template
    if (this.echoTemplate)
        this.print(input, this.echoTemplate, this.echoQueue);
    var output = this.interpret(input);

    /* todo bl: currently we don't have a good way of signaling
     "interpretation succeeded, but there is nothing to print". By convention,
     all my interpreters return ' ' in such situations, but in this terminal
     implementation, spaces may be visible (and highly annoying). Review the
     return values of all my interpreters when I get a chance. */

    /* IE<9 doesn't implement String.trim, so we'll let it print spurious
     spaces for now, until we fix the todo above. */
    var shouldPrintOutput = output && (!output.trim || output.trim().length);

    if (shouldPrintOutput) {
        // If we haven't set an output template, it will just appear as a text node
        this.print(output, this.outputTemplate);
    }

    this.textInput.value = '';
};

BlockTerm.prototype.interpret = function(input) {
    try {
        for (var i = this.interpreters.length-1; i >= 0; --i) {
            var result = this.interpreters[i](input, this);
            if (result)
                return result;
        }
    } catch(e) {
        return e.toString();
    }
};

BlockTerm.prototype.print = function(string, templateElement, printQueue) {
    var outputContainer = this.outputContainer;
    (printQueue || this.printQueue).enqueue(function () {
        var textNode = document.createTextNode(string);
        if (templateElement) {
            var newOutput = templateElement.cloneNode(false);
            newOutput.appendChild(textNode);
            outputContainer.appendChild(newOutput);
        } else {
            outputContainer.appendChild(textNode);
        }
    });

    return this;
};

BlockTerm.prototype.println = function(line) {
    /* If line is an array, that means we should print out each element
     separately. Just for convenience so clients don't have to insert
     newlines manually. */
    if (line instanceof Array) {
        for (var i = 0; i < line.length; ++i)
            this.println(line[i]);
    } else if (typeof line === 'function') {
        this.println(line());
    } else if (typeof line !== 'string' && line.toString) {
        this.println(line.toString());
    } else {
        this.print('\n' + line, this.outputTemplate);
    }
    return this;
};

BlockTerm.prototype.pause = function(ms) {
    // ???
    return this;
};

BlockTerm.prototype.reset  = function() {
    ; // no-op
    return this;
};

BlockTerm.prototype.pushInterpreter = function(interpreter) {
    this.interpreters.push(interpreter);
    return this;
};

BlockTerm.prototype.popInterpreter = function() {
    return this.interpreters.pop();
};

BlockTerm.prototype.start = function() {

};