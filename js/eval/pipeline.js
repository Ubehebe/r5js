goog.module('r5js.Pipeline');

const {Datum, ProcCallLike, UNSPECIFIED_VALUE, VACUOUS_PROGRAM} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/datum');
const Environment = goog.require('r5js.Environment');
const {InputPort} = require('/js/io/input_port_collect_es6_sources.es6/node_modules/__main__/js/io/input_port');
const Reader = goog.require('r5js.Reader');
const {TokenStream} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/ast/token_stream');
const trampoline = goog.require('r5js.trampoline');
const {Error} = require('/js/error_collect_es6_sources.es6/node_modules/__main__/js/error');
const {Nonterminal} = require('/js/ast/datum_collect_es6_sources.es6/node_modules/__main__/js/parse/nonterminals');
const {OutputPort} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');
const {ParserImpl} = goog.require('r5js.ParserImpl');

class Pipeline {

    /** @param {!IEnvironment} rootEnv The root environment. */
    constructor(rootEnv) {
        /** @const @private {!IEnvironment} */
        this.env_ = new Environment(rootEnv);
    }

    /**
     * @param {string} string The string to scan.
     * @return {!TokenStream} A token stream representing the input string.
     */
    scan(string) {
        return TokenStream.forText(string);
    }

    /**
     * @param {!TokenStream} tokenStream A token input stream.
     * @return {!Datum} The root of the datum tree.
     */
    read(tokenStream) {
        return Reader.forTokenStream(tokenStream).read();
    }

    /**
     * @param {!Datum} root The root to parse.
     * @param {!Nonterminal=} nonterminal The nonterminal
     * that should be the root of the parse tree.
     * @return {!Datum}
     */
    parse(root, nonterminal=undefined) {
        const ans = new ParserImpl(root).parse(nonterminal);
        if (!ans) {
            throw Error.parse(root);
        }
        return ans;
    }

    /**
     * @param {!Datum} root The root to desugar.
     * @return {!ProcCallLike}
     */
    desugar(root) {
        return /** @type {!ProcCallLike} */ (root.desugar(this.env_, false));
    }

    /**
     * @param {!ProcCallLike} continuable The continuable to evaluate.
     * @param {!InputPort} inputPort Port to use as current-input-port.
     * @param {!OutputPort} outputPort Port to use as current-output-port.
     * @return {!Value}
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