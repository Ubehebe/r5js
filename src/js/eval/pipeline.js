goog.module('r5js.Pipeline');

const Datum = goog.require('r5js.Datum');
const InputPort = goog.require('r5js.InputPort');
const OutputPort = goog.require('r5js.OutputPort');
const Nonterminal = goog.require('r5js.parse.Nonterminal');
const ProcCallLike = goog.require('r5js.ProcCallLike');
const TokenStream = goog.require('r5js.TokenStream');
const Value = goog.require('r5js.runtime.Value');

/** @interface */
class Pipeline {
 /**
  * @param {string} string The string to scan.
  * @return {!TokenStream} A token stream representing the input string.
  */
 scan(string) {}

 /**
  * @param {!TokenStream} tokenStream A token input stream.
  * @return {Datum} The root of the datum tree, or null if reading failed.
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
}

exports = Pipeline;