#Gay Lisp
Gay Lisp is the first standards-compliant (R5RS) Scheme that runs in the browser, no plugins or extensions required. You can also run it in a server environment with [Node](http://nodejs.org/).

For a working demo and tutorial, check out the [project website](http://gay-lisp.org/).
The project website also has a general overview of Lisp and Scheme; the present document is a technical overview.

##Building

###Browser-based REPL
- `git submodule init && git submodule update && make repl`
- Visit `build/index.html` in a browser.

(The interpreter itself has no dependencies on any JavaScript libraries. The default HTML UI has a single submodule dependency, on [term.js](http://github.com/Ubehebe/term.js/), but this is easily replaced.)

###Node-based REPL
- `make node-repl && node build/node-repl.js`

##Testing
`make test` runs the test suite from the command line. It requires Node.

The `Makefile` has a number of other targets, including `make smslike` (an alternative UI mimicking a mobile phone's text message UI) and `make interpreter` (no UI, just the JavaScript library). Most of the targets have variants ending in `-min` that will minify the JavaScript output. They require the [Google Closure Compiler](https://developers.google.com/closure/compiler/) `compiler.jar` to be present in the working directory.

##Architecture
Gay Lisp is written in JavaScript conforming to Ecmascript 3rd edition, so it should run in a wide variety of current and legacy browsers.

There is a regex-based scanner and a recursive descent parser, both handwritten.

Programs are incrementally transformed into continuation-passing style and evaluated on a trampoline.

Primitive Scheme values are implemented by their primitive JavaScript counterparts as much as practical; in particular, the semantics of numerical operations are mostly those of JavaScript, while still complying with the Scheme standard.

All the non-primitive syntax and procedures are written in Scheme and read in when the interpreter starts up.

There is currently no code generation; it's just an interpreter. A major goal is to generate JavaScript. This would allow the inclusion of Scheme code in HTML `<script>` tags in the manner of [CoffeeScript](http://coffeescript.org/).
