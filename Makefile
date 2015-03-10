# Closure Library-related paths.
closure_root = closure-library

# Closure Compiler-related paths.
compiler_jar  = closure-compiler/build/compiler.jar
compiler      = java -client -XX:+TieredCompilation -jar $(compiler_jar)

# Input-related paths.
src = src/js

# Output-related paths.
outdir = build
deps = $(outdir)/deps.js

# Test-related paths.
test_main_class = r5js.test.main
test_outfile = $(outdir)/test-all.js
# test_opts can be overridden from the command line. Example:
# make test test_opts="type=integration verbose"
test_opts = type=unit verbose

# Node-related paths.
node_repl_main_class = r5js.repl.main
node_repl_outfile = $(outdir)/node-repl.js

# Target platform. This corresponds to r5js.PLATFORM in src/js/platform.js.
# Choices: html5, node.
PLATFORM = node

# First-time setup: clones submodules, builds compiler, etc.
.PHONY: setup
setup:
	git submodule init
	git submodule update
	cd closure-compiler && ant
	cd closure-library && chmod a+x closure/bin/build/*.py

# Runs the Closure linter on staged JS changes.
.PHONY: lint
lint:
	@command -v gjslint > /dev/null 2>&1 || \
		{ echo >&2 "gjslint is required for linting."; exit 1; }
	@git diff --name-only --cached --diff-filter=MA \
	| grep "\.js" \
	| xargs gjslint --strict

# Applies lint fixes suggested by gjslint.
.PHONY: fix
fix:
	@command -v fixjsstyle > /dev/null 2>&1 || \
		{ echo >&2 "fixjsstyle is required to run the fix target."; exit 1; }
	@git diff --name-only --cached --diff-filter=MA \
	| grep "\.js" \
	| xargs fixjsstyle --strict

# Runs the Node-based REPL.
.PHONY: node-repl
node-repl:
	@command -v node > /dev/null 2>&1 || \
		{ echo >&2 "node is required for testing."; exit 1; }
	@node -e "require('./target/node-repl').$(node_repl_main_class)();"
