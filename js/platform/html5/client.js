goog.module('r5js.platform.html5.Client');

const Evaluator = goog.require('r5js.Evaluator');
const EventType = goog.require('goog.events.EventType');
const Message = goog.require('r5js.platform.html5.Message');
const MessageType = goog.require('r5js.platform.html5.MessageType');
const OutputPort = goog.require('r5js.OutputPort');
const Promise = goog.require('goog.Promise');
const Resolver = goog.require('goog.promise.Resolver');

/** @implements {Evaluator} */
class Client {
    /** @param {!OutputPort} outputPort */
    constructor(outputPort) {
        /** @const @private */ this.worker_ = new Worker(r5js.platform.html5.Client.WORKER_SCRIPT);
        /** @const @private */ this.outputPort_ = outputPort;
        /** @const @private {!Array<!Resolver<?>>} */ this.resolvers_ = [];
        /** @private */ this.messageIdCounter_ = 0;

        this.worker_.addEventListener(EventType.MESSAGE, this.onMessage_.bind(this), false);
    }

    /** @override */
    evaluate(input) {
        const resolver = Promise.withResolver();
        const messageId = ++this.messageIdCounter_;
        this.resolvers_[messageId] = resolver;
        this.worker_.postMessage(Message.newEvalRequest(messageId, input));
        return resolver.promise;
    }

    /**
     * @param {!Event} e
     * @private
     */
    onMessage_(e) {
        e = /** @type {!MessageEvent} */ (e);
        const message = /** @type {!Message} */ (e.data);
        switch (message.type) {
            case MessageType.EVAL_RESPONSE:
                this.resolvers_[message.id].resolve(message.content);
                break;
            case MessageType.EVAL_ERROR:
                this.resolvers_[message.id].reject(message.content);
                break;
            case MessageType.OUTPUT:
                this.outputPort_.write(/** @type {string} */ (message.content));
                return;
        }
        delete this.resolvers_[message.id];
    }
}

exports = Client;

/** @define {string} Location of the worker script. */
goog.define('r5js.platform.html5.Client.WORKER_SCRIPT', '');