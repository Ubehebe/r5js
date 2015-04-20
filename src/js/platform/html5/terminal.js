goog.module('r5js.platform.html5.Terminal');

const Promise = goog.require('goog.Promise');
const replutil = goog.require('r5js.replutil');
const Terminal = goog.require('r5js.Terminal');

/** @implements {Terminal} */
class Html5Terminal {
    /** @param {?} jqconsole */
    constructor(jqconsole) {
        /** @const @private */ this.jqconsole_ = jqconsole;
    }

    /**
     * @param {string} line
     * @param {!Function} cb
     * @private
     * @see https://github.com/replit/jq-console for details on the odd return values.
     */
    multilineCallback_(line, cb) {
        const done = replutil.isLineComplete(line);
        cb(done ? false : 0);
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
