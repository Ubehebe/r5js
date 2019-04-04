# Technical overview

r5js is written in [TypeScript](https://www.typescriptlang.org) and built using
[Bazel](https://bazel.build). To build everything, clone the repository and type `bazel build ...`.
The first time this is run, Bazel will automatically download a number of tool dependencies needed
to build the project (node, tsc, etc.) and cache them. Subsequent Bazel invocations will be much
faster.

The repository exposes several dozen Bazel targets, which are units that can be built independently.
(`bazel query ...` lists them all.) The mappings from source files to Bazel targets are defined by
the BUILD files in each directory. Most of the targets are of type `ts_library`; each one is a
TypeScript compilation unit that goes into the overall r5js interpreter. Other targets include:

- `bazel run devserver` brings up a web server serving r5js in a simple UI.
- `bazel run node/repl` brings up a node-based r5js interpreter in a console.
- `bazel test ...` runs all the tests in the repository.

## Maintenance

Each commit should pass all the tests, and the code it touches should be properly formatted.
To enforce this, symlink `scripts/pre-commit.sh` to `.git/hooks/pre-commit`. This will cause git to
abort a commit if anything is amiss.


