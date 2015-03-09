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

# Android-related paths.
android_main_class = r5js.platform.android.main
android_outfile = $(outdir)/r5js-android.js

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

# Compiles the Android port.
.PHONY: android
android:
	@mkdir -p $(outdir)
	@find $(src) $(closure_root) -name "*\.js" \
	| xargs printf "\-\-js %s " \
	| xargs $(compiler) \
		--js $(closure_root)/closure/goog/deps.js \
		--closure_entry_point=$(android_main_class) \
		--closure_entry_point=$(test_main_class) \
		--only_closure_dependencies \
		--define r5js.PLATFORM=\'android\' \
		--externs=custom-externs/android.js \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		> $(android_outfile)

# Runs the Node-based REPL.
.PHONY: node-repl
node-repl: compile-node-repl
node-repl:
	@command -v node > /dev/null 2>&1 || \
		{ echo >&2 "node is required for testing."; exit 1; }
	@node -e "require('./build/node-repl').$(node_repl_main_class)();"

# Cleans everything up.
.PHONY: clean
clean:
	@rm -rf $(outdir)
