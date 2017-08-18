goog.module('r5js.Repl');

const Error = goog.require('r5js.Error');
const Evaluator = goog.require('r5js.Evaluator');
const Promise = goog.require('goog.Promise');
const Terminal = goog.require('r5js.Terminal');
const replutil = goog.require('r5js.replutil');

class Repl {
    /**
     * @param {!Terminal} terminal
     * @param {!Evaluator} evaluator
     */
    constructor(terminal, evaluator) {
        /** @const @private */ this.terminal_ = terminal;
        /** @const @private */ this.evaluator_ = evaluator;
        /** @private */ this.awaitingEval_ = '';
    }

    /** Starts the read-eval-print loop. */
    start() {
        this.terminal_.getNextLineOfInput().then(
            this.handleInputLine, undefined /* opt_onRejected */, this);
    }

    /**
     * @param {string} inputLine
     * @private
     */
    handleInputLine(inputLine) {
        // TODO bl the resolve is technically not needed, but the control flow through this method
        // is tricky.
        Promise.resolve(replutil.isLineComplete(this.awaitingEval_ += inputLine + ' '))
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