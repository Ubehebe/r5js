This is a temporary readme. The interpreter is complete, but the user
interface and JavaScript/DOM APIs need work.

#Building
The interpreter has no dependencies on any JavaScript libraries.
The default web interface to the interpreter depends on [jQuery](http://jquery.com)
and the excellent [jQuery Terminal plugin](http://terminal.jcubic.pl/).

##Building the interpreter and web interface
1. `git submodule init && git submodule update` to bring in the correct
version of the jQuery Terminal plugin.
2. `make` (or `make repl`)

Point your browser to `build/repl.html`. The interpreter should be
running in a terminal-like interface.

##Building just the interpreter JavaScript library.
1. `make interpreter`

The library will be in a single file, `build/gay-lisp-<version>.js`, suitable
for inclusion into HTML.

##Minification
The build process can optionally minify the JavaScript output using the
[Google Closure Compiler](https://developers.google.com/closure/compiler/).
If you place the Closure Compiler's `compiler.jar` in the project root, you
can type `make repl-min` or `make interpreter-min` to get minified versions
of the above.

#Testing
`make test` runs several hundred unit tests. It requires [Node](http://nodejs.org/).