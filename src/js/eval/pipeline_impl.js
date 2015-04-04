goog.provide('r5js.PipelineImpl');

goog.require('r5js.Environment');
goog.require('r5js.ParserImpl');
goog.require('r5js.Pipeline');
goog.require('r5js.ReaderImpl');
goog.require('r5js.Scanner');
goog.require('r5js.VACUOUS_PROGRAM');
goog.require('r5js.error');
goog.require('r5js.runtime.UNSPECIFIED_VALUE');
goog.require('r5js.trampoline');

r5js.PipelineImpl = /** @implements {r5js.Pipeline} */ class {
    /** @param {!r5js.IEnvironment} rootEnv The root environment. */
    constructor(rootEnv) {
        /** @const @private {!r5js.IEnvironment} */
        this.env_ = new r5js.Environment(rootEnv);
    }

    /** @override */
    scan(string) {
        return new r5js.Scanner(string);
    }

    /** @override */
    read(scanner) {
        return new r5js.ReaderImpl(scanner).read();
    }

    /** @override */
    parse(root, opt_nonterminal) {
        var parser = new r5js.ParserImpl(root);
        var ans = goog.isDef(opt_nonterminal) ?
            parser.parse(opt_nonterminal) :
            parser.parse();
        if (!ans) {
            throw r5js.error.parse(root);
        }
        return ans;
    }

    /** @override */
    desugar(root) {
        return /** @type {!r5js.ProcCallLike} */ (root.desugar(this.env_, false));
    }

    /**
     * @override
     * @suppress {checkTypes} TODO bl the compiler believes the ternary
     * always evaluates to false.
     */
    Eval(continuable, inputPort, outputPort) {
        // r5js.VACUOUS_PROGRAM isn't a ProcCallLike, but this is enough of
        // a special case that I don't care.
        return continuable === r5js.VACUOUS_PROGRAM ?
            r5js.runtime.UNSPECIFIED_VALUE :
            r5js.trampoline(continuable, this.env_, inputPort, outputPort);
    }
};
