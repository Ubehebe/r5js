<!--hello-->
<link rel="stylesheet" href="r5rs.css"/>
<script src="rollup.min.js"></script>

# r5js
<textarea class='terminal' id='play' rows="20" spellcheck="false" autofocus="autofocus"></textarea>

r5js is an interpreter for the [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
programming language. It is written in TypeScript.

r5js aims for full compliance with [R5RS](https://www.schemers.org/Documents/Standards/R5RS/HTML/),
the fifth edition of the Scheme specification. It supports hygienic macros,
first-class continuations, and proper tail recursion. It includes over 700 tests exercising most of
the language's facilities.

r5js is free software, licensed under [GPLv3](https://www.gnu.org/copyleft/gpl.html).

## Building
r5js is built using [Bazel](https://bazel.build) version 0.24+. To build, install Bazel, clone this
repository, and run `bazel build ...`.

## Running
`bazel run node:repl` brings up an r5js interpreter in the console.

TODO: expose a target for running in the browser.
