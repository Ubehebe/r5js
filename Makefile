# Closure Library-related paths.
closure_root = closure-library
closure_bin  = $(closure_root)/closure/bin/build
builder      = $(closure_bin)/closurebuilder.py
depswriter   = $(closure_bin)/depswriter.py

# Closure Compiler-related paths.
compiler_jar  = closure-compiler/build/compiler.jar
compiler      = java -client -XX:+TieredCompilation -jar $(compiler_jar)

# Input-related paths.
src = src/js

# Output-related paths.
main_class = r5js.main
outdir = build
deps = $(outdir)/deps.js

# Static server-related paths.
static_port = 8888
static_root = http://localhost:$(static_port)

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

# Brings the Closure JS dependencies up-to-date.
.PHONY: deps
deps:
	@mkdir -p $(outdir)
	@$(depswriter) --root_with_prefix="$(src) ../../../$(src)" > $(deps)

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

# Runs the Closure Compiler on the codebase as strictly as possible.
.PHONY: typecheck
typecheck:
	@find $(src) -name "*.js" \
	| xargs printf "\-\-input %s " \
	| xargs $(builder) --root=$(src) --root=$(closure_root) \
	| xargs printf "\-\-js %s " \
	| xargs $(compiler) \
		--js $(closure_root)/closure/goog/deps.js \
		--externs=externs/buffer.js \
		--externs=externs/core.js \
		--externs=externs/events.js \
		--externs=externs/fs.js \
		--externs=externs/process.js \
		--externs=externs/readline.js \
		--externs=externs/stream.js \
		--externs=custom-externs/android.js \
		--warning_level VERBOSE \
		--jscomp_error accessControls \
		--jscomp_error ambiguousFunctionDecl \
		--jscomp_error checkRegExp \
		--jscomp_error checkTypes \
		--jscomp_error checkVars \
		--jscomp_error const \
		--jscomp_error constantProperty \
		--jscomp_error deprecated \
		--jscomp_error duplicateMessage \
		--jscomp_error globalThis \
		--jscomp_error internetExplorerChecks \
		--jscomp_error invalidCasts \
		--jscomp_error misplacedTypeAnnotation \
		--jscomp_error missingProperties \
		--jscomp_error nonStandardJsDocs \
		--jscomp_error suspiciousCode \
		--jscomp_error strictModuleDepCheck \
		--jscomp_error typeInvalidation \
		--jscomp_error undefinedNames \
		--jscomp_error undefinedVars \
		--jscomp_error unknownDefines \
		--jscomp_error uselessCode \
		--jscomp_error visibility \
		> /dev/null

# Compiles the Node-based REPL.
.PHONY: compile-node-repl
compile-node-repl:
	@mkdir -p $(outdir)
	@find $(src) $(closure_root) -name "*\.js" \
	| xargs printf "\-\-js %s " \
	| xargs $(compiler) \
		--closure_entry_point=$(node_repl_main_class) \
		--only_closure_dependencies \
		--define r5js.PLATFORM=\'node\' \
		--externs=externs/buffer.js \
		--externs=externs/core.js \
		--externs=externs/events.js \
		--externs=externs/fs.js \
		--externs=externs/process.js \
		--externs=externs/readline.js \
		--externs=externs/stream.js \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		> $(node_repl_outfile)

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

# Compiles the test suite.
.PHONY: compile-tests
compile-tests:
	@mkdir -p $(outdir)
	@find $(src) $(closure_root) -name "*\.js" \
	| xargs printf "\-\-js %s " \
	| xargs $(compiler) \
		--js $(closure_root)/closure/goog/deps.js \
		--only_closure_dependencies \
		--closure_entry_point=$(test_main_class) \
		--define r5js.PLATFORM=\'$(PLATFORM)\' \
		--externs=externs/buffer.js \
		--externs=externs/core.js \
		--externs=externs/events.js \
		--externs=externs/fs.js \
		--externs=externs/process.js \
		--externs=externs/stream.js \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		> $(test_outfile)

# Runs the Node-based REPL.
.PHONY: node-repl
node-repl: compile-node-repl
node-repl:
	@command -v node > /dev/null 2>&1 || \
		{ echo >&2 "node is required for testing."; exit 1; }
	@node -e "require('./build/node-repl').$(node_repl_main_class)();"

# Launches an HTTP server to serve the test suite to browsers.
# The test suite can be reached at /test/test.html.
# (The test suite cannot be served directly from the filesystem because
# it uses Ajax, which doesn't work for file:// URLs.
.PHONY: test-server
test-server: deps
	@command -v python > /dev/null 2>&1 || \
		{ echo >&2 "python is required for running the test server."; exit 1; }
	@python -m SimpleHTTPServer $(static_port)

# Cleans everything up.
.PHONY: clean
clean:
	@rm -rf $(outdir)
