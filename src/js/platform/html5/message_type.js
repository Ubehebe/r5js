goog.module('r5js.platform.html5.MessageType');

/**
 * @enum {number}
 * @package
 */
const MessageType = {
    EVAL_REQUEST: 0,
    EVAL_RESPONSE: 1,
    EVAL_ERROR: 2,
    OUTPUT: 3
};

exports = MessageType;
