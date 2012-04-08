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
function BlockTerm(textArea, outputContainer, submitElement) {

    this.textArea = textArea;
    this.outputContainer = outputContainer;
    this.interpreters = [];

    var self = this;

    submitElement.addEventListener('click', function() {
        self.onInputComplete();
    });
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
    var input = this.textArea.value;
    // Only echo input if we've explicitly set an echo template
    if (this.echoTemplate)
        this.print(input, this.echoTemplate);
    var output = this.interpret(input);

    // If we haven't set an output template, it will just appear as a text node
    this.print(output, this.outputTemplate);
    this.textArea.value = '';
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

BlockTerm.prototype.print = function(string, templateElement) {
    var textNode = document.createTextNode(string);
    if (templateElement) {
        var newOutput = templateElement.cloneNode();
        newOutput.appendChild(textNode);
        this.outputContainer.appendChild(newOutput);
    } else {
        this.outputContainer.appendChild(textNode);
    }

    return this;
};

BlockTerm.prototype.pause = function(ms) {
    // ???
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