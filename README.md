#Gay Lisp

This is a temporary readme. The interpreter is complete, but the user
interface and JavaScript/DOM APIs need work.

##Building
Gay Lisp has no dependencies on any JavaScript libraries.

To build Gay Lisp, you will need the standard Unix command-line tools.
This means you should probably be using Linux or a Mac.

###To build the interpreter plus the web interface
`make` (or `make repl`), then point your browser to `build/repl.html`.

###To build just the interpreter
`make interpreter`. This will give you a single file,
`build/gay-lisp-<version>.js`, that exports the public API.

###Minification
The build process can optionally minify the JavaScript output using the
[Google Closure Compiler](https://developers.google.com/closure/compiler/).
If you place the Closure Compiler's `compiler.jar` in the project root, you
can type `make repl-min` or `make interpreter-min` to get minified versions
of the above.

##Testing
`make test` runs several hundred unit tests. It requires [Node](http://nodejs.org/).