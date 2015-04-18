goog.module('r5js.Repl');

const Error = goog.require('r5js.Error');
const Evaluator = goog.require('r5js.Evaluator');
const Promise = goog.require('goog.Promise');
const Terminal = goog.require('r5js.Terminal');

class Repl {
    /**
     * @param {!Terminal} terminal
     * @param {!Evaluator} evaluator
     * @param {function(string): !Promise<boolean>} isLineComplete
     */
    constructor(terminal, evaluator, isLineComplete) {
        /** @const @private */ this.terminal_ = terminal;
        /** @const @private */ this.evaluator_ = evaluator;
        /** @const @private */ this.isLineComplete_ = isLineComplete;
        /** @private */ this.awaitingEval_ = '';
    }

    /** Starts the read-eval-print loop. */
    start() {
        this.terminal_.getNextLineOfInput().then(
            this.handleInputLine, undefined /* opt_onRejected */, this);
    }

    /** @param {string} inputLine */
    handleInputLine(inputLine) {
        this.isLineComplete_(this.awaitingEval_ += inputLine + ' ')
            .then(complete => {
                if (complete) {
                    const toEval = this.awaitingEval_;
                    this.awaitingEval_ = '';
                    return this.evaluator_.evaluate(toEval);
                }
            }, undefined /* opt_onRejected */, this)
            .then(
                value => this.terminal_.print(value),
                error => this.terminal_.error((/** @type {!Error} */ (error)).msg), this)
            .thenAlways(() => {
                this.terminal_.getNextLineOfInput().then(
                    this.handleInputLine, undefined /* opt_onRejected */, this);
            }, this);
    }
}

exports = Repl;