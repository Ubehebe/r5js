# r5js [![Build Status](https://drone.io/github.com/Ubehebe/r5js/status.png)](https://drone.io/github.com/Ubehebe/r5js/latest)

r5js is an implementation of the
[Scheme](http://en.wikipedia.org/wiki/Scheme_(programming_language))
programming language. It is written in JavaScript and runs in
modern web browsers and [Node](http://nodejs.org/).

Unlike other JavaScript Scheme implementations, r5js aims for full compliance
with [R5RS](http://www.schemers.org/Documents/Standards/R5RS/HTML/),
the fifth edition of the Scheme specification. It supports hygienic macros,
first-class continuations, and proper tail recursion. It includes over 700 tests
exercising most of the language's facilities.

r5js is free software, licensed under
[GPLv3](http://www.gnu.org/copyleft/gpl.html).

## Building

r5js uses the [Google Closure](https://developers.google.com/closure/) tools
to build the application and manage dependencies. These tools require Java,
Python, and [Apache Ant](http://en.wikipedia.org/wiki/Apache_Ant) to be installed.

1. Clone the main repository:
   `git clone https://github.com/Ubehebe/r5js`
2. Clone the submodules:
   `cd r5js && git submodule init && git submodule update`
3. Build the Closure Compiler: `cd closure-compiler && ant && cd ..`
4. Make the Closure Library dependency scripts writable:
   `chmod a+x closure-library/closure/bin/build/*.py`
5. Run the unit tests from the command line: `make test`.
   This step requires Node.

## Running

### In a browser

1. At the command line, run `make test-server`.
   This launches a simple web server that serves the application.
   (It can't be served from a filesystem, because the application uses Ajax
   to fetch the Scheme standard library.)
2. In a browser, navigate to `localhost:8888/ui/index.html`.
   This page provides a simple read–eval–print loop using
   [jq-console](https://github.com/replit/jq-console).

### From the command line

Run `make node-repl`. This launches a simple read–eval–print loop
directly in the terminal, using Node.

## Developing

All commits should pass the pre-commit hook, which should be symlinked
into  `.git/hooks`:  `ln -s $PWD/hooks/pre-commit $PWD/.git/hooks/pre-commit`.

This hook performs linting and type-checking, and runs the unit tests before
committing. It requires the
[Closure Linter](https://developers.google.com/closure/utilities/) tools
`gjslint` and `fixjsstyle` to be installed and available via `$PATH`.
