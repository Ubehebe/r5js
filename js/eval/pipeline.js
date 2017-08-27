goog.module('r5js.Pipeline');

const Datum = goog.require('r5js.Datum');
const Environment = goog.require('r5js.Environment');
const Error = goog.require('r5js.Error');
const IEnvironment = goog.require('r5js.IEnvironment');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const ParserImpl = goog.require('r5js.ParserImpl');
const Reader = goog.require('r5js.Reader');
const TokenStream = goog.require('r5js.TokenStream');
const UNSPECIFIED_VALUE = goog.require('r5js.UNSPECIFIED_VALUE');
const VACUOUS_PROGRAM = goog.require('r5js.VACUOUS_PROGRAM');
const trampoline = goog.require('r5js.trampoline');
const {Nonterminal} = goog.require('r5js.parse.Nonterminals');
const {ProcCallLike} = goog.require('r5js.ProcCallLike');
const {Value} = goog.require('r5js.Value');

/** @interface */
class Pipeline {
    /**
     * @param {string} string The string to scan.
     * @return {!TokenStream} A token stream representing the input string.
     */
    scan(string) {}

    /**
     * @param {!TokenStream} tokenStream A token input stream.
     * @return {?Datum} The root of the datum tree, or null if reading failed.
     */
    read(tokenStream) {}

    /**
     * @param {!Datum} root The root to parse.
     * @param {!Nonterminal=} opt_nonterminal The nonterminal
     * that should be the root of the parse tree.
     * @return {!Datum}
     */
    parse(root, opt_nonterminal) {}

    /**
     * @param {!Datum} root The root to desugar.
     * @return {!ProcCallLike}
     */
    desugar(root) {}

    /**
     * @param {!ProcCallLike} continuable The continuable to evaluate.
     * @param {!InputPort} inputPort Port to use as current-input-port.
     * @param {!OutputPort} outputPort Port to use as current-output-port.
     * @return {!Value}
     */
    Eval(continuable, inputPort, outputPort) {}

    /**
     * @param {!IEnvironment} rootEnv
     * @return {!Pipeline}
     */
    static forEnvironment(rootEnv) {
        return new Impl(rootEnv);
    }
}

/** @implements {Pipeline} */
class Impl {
    /** @param {!IEnvironment} rootEnv The root environment. */
    constructor(rootEnv) {
        /** @const @private {!IEnvironment} */
        this.env_ = new Environment(rootEnv);
    }

    /** @override */
    scan(string) {
        return TokenStream.forText(string);
    }

    /** @override */
    read(scanner) {
        return Reader.forTokenStream(scanner).read();
    }

    /** @override TODO bl shouldn't be necessary */
    parse(root, opt_nonterminal) {
        var parser = new ParserImpl.ParserImpl(root);
        var ans = goog.isDef(opt_nonterminal)
            ? parser.parse(opt_nonterminal)
            : parser.parse();
        if (!ans) {
            throw Error.parse(root);
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

exports = Pipeline;