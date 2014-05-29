version = `cat VERSION`
output = build/gay-lisp-$(version).js
unit_tests = build/unit_tests.scm

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

.PHONY: deps
deps:
	@mkdir -p $(outdir)
	@$(depswriter) --root_with_prefix="$(src) ../../../$(src)" > $(deps)


# Only lint staged JS changes.
.PHONY: lint
lint:
	@command -v gjslint > /dev/null 2>&1 || \
		{ echo >&2 "gjslint is required for linting."; exit 1; }
	@git diff --name-only --cached --diff-filter=MA \
	| grep "\.js" \
	| xargs gjslint --strict

.PHONY: fix
fix:
	@command -v fixjsstyle > /dev/null 2>&1 || \
		{ echo >&2 "fixjsstyle is required to run the fix target."; exit 1; }
	@git diff --name-only --cached --diff-filter=MA \
	| grep "\.js" \
	| xargs fixjsstyle --strict

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

.PHONY: compile-node-repl
compile-node-repl:
	@mkdir -p $(outdir)
	@find $(src) -name "*\.js" \
	| xargs printf "\-\-input %s " \
	| xargs $(builder) --root=$(src) --root=$(closure_root) \
	| xargs printf "\-\-js %s " \
	| xargs $(compiler) \
		--js $(closure_root)/closure/goog/deps.js \
		--closure_entry_point=$(node_repl_main_class) \
		--externs=externs/buffer.js \
		--externs=externs/core.js \
		--externs=externs/events.js \
		--externs=externs/fs.js \
		--externs=externs/process.js \
		--externs=externs/readline.js \
		--externs=externs/stream.js \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		> $(node_repl_outfile)

.PHONY: compile-tests
compile-tests:
	@mkdir -p $(outdir)
	@find $(src) -name "*\.js" \
	| xargs printf "\-\-input %s " \
	| xargs $(builder) --root=$(src) --root=$(closure_root) \
	| xargs printf "\-\-js %s " \
	| xargs $(compiler) \
		--js $(closure_root)/closure/goog/deps.js \
		--closure_entry_point=$(test_main_class) \
		--externs=externs/buffer.js \
		--externs=externs/core.js \
		--externs=externs/events.js \
		--externs=externs/fs.js \
		--externs=externs/process.js \
		--externs=externs/stream.js \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		> $(test_outfile)

.PHONY: test
test: compile-tests
test:
	@command -v node > /dev/null 2>&1 || \
		{ echo >&2 "node is required for testing."; exit 1; }
	@node -e "require('./build/test-all').r5js.test.main(process.argv, process.env);" $(test_opts)

.PHONY: node-repl
node-repl: compile-node-repl
node-repl:
	@command -v node > /dev/null 2>&1 || \
		{ echo >&2 "node is required for testing."; exit 1; }
	@node -e "require('./build/node-repl').$(node_repl_main_class)();"

.PHONY: test-server
test-server: deps
	@command -v python > /dev/null 2>&1 || \
		{ echo >&2 "python is required for running the test server."; exit 1; }
	@python -m SimpleHTTPServer $(static_port)

.PHONY: clean
clean:
	@rm -rf $(outdir)
