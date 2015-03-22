# Runs the Closure linter on staged JS changes.
.PHONY: lint
lint:
	@command -v gjslint > /dev/null 2>&1 || \
		{ echo >&2 "gjslint is required for linting."; exit 1; }
	@git diff --name-only --cached --diff-filter=MA \
	| grep "\.js" \
	| xargs gjslint --strict --custom_jsdoc_tags=package

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
	@mvn -q install
	@node -e "require('./target/node-repl').r5js.repl.main();"
