/**
 * @fileoverview Forked from https://github.com/dcodeIO/node.js-closure-compiler-externs,
 * which does not compile in pedantic mode.
 * @externs
 */

 class Stream {
   constructor(options) {}
 }

 class NodeReadableStream extends Stream {}

 const process = function() {};

 /** @type {NodeReadableStream} */ process.stdin;
/** @type {NodeReadableStream} */ process.stdout;

 /** @param {number=} code */
 process.exit = function(code) {};

 /**
  * @param {string} name
  * @return {*}
  */
 function require(name) {}

 const readline = {};
 readline.createInterface = function(options) {};