goog.module('r5js.platform.html5.Message');

const Error = goog.require('r5js.Error');
const MessageType = goog.require('r5js.platform.html5.MessageType');

/**
 * Structure for messages passed between {@link r5js.platform.html5.Client}
 * and {@link r5js.platform.html5.Worker}. This type isn't goog.provided;
 * use the r5js.platform.html5.message.* helper functions instead.
 * @package
 */
class Message {
    /**
     * @param {!MessageType} type Message type. Ordinarily,
     * type tags are inferior to subclassing. However, the HTML5 structured clone
     * algorithm restricts the data that can pass between a worker and its parent.
     * To avoid several pitfalls (for example, functions can't be serialized),
     * we stick to a simple structure and use type tags.
     * @param {number} id Message id.
     * @param {string|!Error} content Message content.
     */
    constructor(type, id, content) {
        /** @const */ this.type = type;
        /** @const */ this.id = id;
        /** @const */ this.content = content;
    }

    /**
     * @param {number} id Message id.
     * @param {string} request Message content.
     * @return {!Message}
     */
    static newEvalRequest(id, request) {
        return new Message(MessageType.EVAL_REQUEST, id, request);
    }

    /**
     * @param {number} id Message id.
     * @param {string} value Message content.
     * @return {!Message}
     */
    static newEvalResponse(id, value) {
        return new Message(MessageType.EVAL_RESPONSE, id, value);
    }

    /**
     * @param {number} id Message id.
     * @param {!Error} error Error.
     * @return {!Message}
     */
    static newEvalError(id, error) {
        return new Message(MessageType.EVAL_ERROR, id, error);
    }

    /**
     * @param {string} value
     * @return {!Message}
     */
    static output(value) {
        return new Message(MessageType.OUTPUT, -1, value);
    }
}

exports = Message;







