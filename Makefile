# The build process inserts src/js/* inside the body of the anonymous function
# in src/api/api.js in order to hide as much of the implementation as possible.
# It also embeds src/scm/syntax.scm and src/scm/procedures.scm as string
# literals into the body of the same function.

output = build/gay_lisp.js
unit_tests = build/unit_tests.scm

# Target the first line after the first brace in src/api/api.js
gay-lisp: firstBrace = `grep -m1 -A1 -n { src/api/api.js | head -1 | sed -e 's/^\([0-9]*\).*/\1/'`
gay-lisp: afterFirstBrace = `grep -m1 -A1 -n { src/api/api.js | tail -1 | sed -e 's/^\([0-9]*\).*/\1/'`
gay-lisp: banner_src = src/banner.txt
gay-lisp:
	mkdir -p build
	head -n$(firstBrace) src/api/api.js > $(output)
	cat src/js/*.js >> $(output)
	echo "\nvar syntax = \"\c" >> $(output)
	# Remove Scheme comments, escape Scheme backslashes and quotes, compress whitespace
	# (Note that we compress whitespace inside string literals, which is not really correct...)
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < src/scm/r5rs-syntax.scm | tr -s '\n\t ' ' ' >> $(output)
	echo "\";" >> $(output)
	echo "var procedures = \"\c" >> $(output)
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < src/scm/r5rs-procedures.scm | tr -s '\n\t ' ' ' >> $(output)
	echo "\";" >> $(output)

	# Embed the banner
	echo "\nvar banner = \"\c" >> $(output)

	cp $(banner_src) build/tmp
	echo "\n;; Version \c" >> build/tmp
	cat VERSION >> build/tmp
	echo "\n;; Built on \c" >> build/tmp
	echo `date` >> build/tmp
	cat build/tmp | sed -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' | awk '{ printf "%s\\n", $$0}' >> $(output)
	echo "\";" >> $(output)
	rm build/tmp

	tail -n+$(afterFirstBrace) src/api/api.js >> $(output)
	cp src/html/repl.html build/
	cp lib/* build/
	cat test/framework/* | sed -e 's/;.*//' | tr -s '\n\t ' ' ' >> $(unit_tests)
	cat test/*.scm | sed -e 's/;.*//' | tr -s '\n\t ' ' ' >> $(unit_tests)

# Requires Google Closure Compiler compiler.jar to be in pwd
min: gay-lisp
min:
	cat src/api/closure_exports.js >> $(output)
	java -jar compiler.jar --compilation_level ADVANCED_OPTIMIZATIONS < $(output) > tmp
	mv tmp $(output)

node: gay-lisp
node:
	# In a server environment, file size is not a big deal, so we
	# embed the Scheme-based unit tests directly in the module.
	# One less thing to lose track of.
	echo "var tests = \"\c" >> $(output)
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < $(unit_tests) | tr -s '\n\t ' ' ' >> $(output)
	echo "\";" >> $(output)
	cat src/api/node_exports.js >> $(output)

test: node
test:
	hash node 2>&- || echo >&2 "testing requires node"; exit
	node -e 'require("./build/gay_lisp").test()'

clean:
	rm -f build/*