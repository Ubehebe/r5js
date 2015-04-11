goog.module('r5js.platform.html5.Terminal');

const Promise = goog.require('goog.Promise');
const Terminal = goog.require('r5js.Terminal');

/** @implements {Terminal} */
class Html5Terminal {
    /**
     * @param {?} jqconsole
     * @param {function(string):!Promise<boolean>} isLineComplete Function
     * to determine if a given line of user input is complete (= ready to be
     * evaluated).
     */
    constructor(jqconsole, isLineComplete) {
        /** @const @private */ this.jqconsole_ = jqconsole;
        /** @const @private */ this.isLineComplete_ = isLineComplete;
    }

    /**
     * @param {string} line
     * @param {!Function} cb
     * @private
     * @see https://github.com/replit/jq-console for details on the odd return
     * values.
     */
    multilineCallback_(line, cb) {
        this.isLineComplete_(line).then(done => cb(done ? false : 0));
    }

    /**
     * @override
     * @suppress {checkTypes} for the jqconsole integration
     */
    getNextLineOfInput() {
        return new Promise(function(resolve) {
            this.jqconsole_['Prompt'](
                true /* history_enabled */,
                resolve,
                this.multilineCallback_.bind(this),
                true /* async_multiline */);
        }, this);
    }

    /**
     * @override
     * @suppress {checkTypes} for the jqconsole integration
     */
    print(msg) {
        this.jqconsole_['Write'](msg + '\n', 'jqconsole-output');
    }

    /**
     * @override
     * @suppress {checkTypes} for the jqconsole integration
     */
    error(msg) {
        this.jqconsole_['Write'](msg + '\n', 'jqconsole-error');
    }
}

exports = Html5Terminal;
