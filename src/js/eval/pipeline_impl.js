goog.module('r5js.PipelineImpl');

const Environment = goog.require('r5js.Environment');
const error = goog.require('r5js.error');
const IEnvironment = goog.require('r5js.IEnvironment');
const ParserImpl = goog.require('r5js.ParserImpl');
const Pipeline = goog.require('r5js.Pipeline');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const ReaderImpl = goog.require('r5js.ReaderImpl');
const Scanner = goog.require('r5js.Scanner');
const trampoline = goog.require('r5js.trampoline');
const UNSPECIFIED_VALUE = goog.require('r5js.runtime.UNSPECIFIED_VALUE');
const VACUOUS_PROGRAM = goog.require('r5js.VACUOUS_PROGRAM');

/** @implements {Pipeline} */
class PipelineImpl {
    /** @param {!IEnvironment} rootEnv The root environment. */
    constructor(rootEnv) {
        /** @const @private {!IEnvironment} */
        this.env_ = new Environment(rootEnv);
    }

    /** @override */
    scan(string) {
        return new Scanner(string);
    }

    /** @override */
    read(scanner) {
        return new ReaderImpl(scanner).read();
    }

    /** @override */
    parse(root, opt_nonterminal) {
        var parser = new ParserImpl(root);
        var ans = goog.isDef(opt_nonterminal)
            ? parser.parse(opt_nonterminal)
            : parser.parse();
        if (!ans) {
            throw error.parse(root);
        }
        return ans;
    }

    /** @override */
    desugar(root) {
        return /** @type {!ProcCallLike} */ (root.desugar(this.env_, false));
    }

    /**
     * @override
     * @suppress {checkTypes} TODO bl the compiler believes the ternary
     * always evaluates to false.
     */
    Eval(continuable, inputPort, outputPort) {
        // VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of
        // a special case that I don't care.
        return continuable === VACUOUS_PROGRAM
            ? UNSPECIFIED_VALUE
            : trampoline(continuable, this.env_, inputPort, outputPort);
    }
}

exports = PipelineImpl;
