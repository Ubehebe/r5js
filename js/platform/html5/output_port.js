goog.module('r5js.platform.html5.OutputPort');

const OutputPort = goog.require('r5js.OutputPort');

/** @implements {OutputPort} */
class Html5OutputPort {
    /** @param {function(string)} postMessage */
    constructor(postMessage) {
        /** @const @private */ this.postMessage_ = postMessage;
    }

    /** @override */
    write(value) {
        this.postMessage_(value);
    }

    /** @override */
    close() {}
}

exports = Html5OutputPort;

OutputPort.addImplementation(Html5OutputPort);
