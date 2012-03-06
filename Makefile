# The build process inserts src/js/* inside the body of the anonymous function
# in src/api/api.js in order to hide as much of the implementation as possible.
# It also embeds src/scm/syntax.scm and src/scm/procedures.scm as string
# literals into the body of the same function.

version = `cat VERSION`
output = build/gay-lisp-$(version).js
unit_tests = build/unit_tests.scm

# JS library plus a simple HTML terminal emulator
# (requires jQuery plugin in submodule)
repl: interpreter
repl: term = submodules/jquery.terminal
repl: files = $(term)/js/jquery.terminal-0.4.11.min.js
repl:
	cat src/html/repl.html | sed -e "s/gay-lisp\.js/gay-lisp-$(version).js/g" > build/repl.html
	cp src/css/repl.css build/
	cp $(files) build/

repl-min: interpreter-min repl
repl-min:
	sed -i -e "s/gay-lisp-$(version)\.js/gay-lisp-$(version)-min.js/g" build/repl.html

# Just make the JS library (no UI)
# Target the first line after the first brace in src/api/api.js
interpreter: firstBrace = `grep -m1 -A1 -n { src/api/api.js | head -1 | sed -e 's/^\([0-9]*\).*/\1/'`
interpreter: afterFirstBrace = `grep -m1 -A1 -n { src/api/api.js | tail -1 | sed -e 's/^\([0-9]*\).*/\1/'`
interpreter: banner_src = src/banner.txt
interpreter:
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
	echo "\n;; Version $(version) (source commit \c" >> build/tmp
	cat .git/refs/heads/master | sed -e 's/\(.\{7\}\).*/\1)/' >> build/tmp
	echo ";; Built on \c" >> build/tmp
	echo `date` >> build/tmp
	cat build/tmp | sed -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' | awk '{ printf "%s\\n", $$0}' >> $(output)
	echo "\";" >> $(output)
	rm build/tmp

	tail -n+$(afterFirstBrace) src/api/api.js >> $(output)
	cat test/framework/* | sed -e 's/;.*//' | tr -s '\n\t ' ' ' >> $(unit_tests)
	cat test/*.scm | sed -e 's/;.*//' | tr -s '\n\t ' ' ' >> $(unit_tests)


# Requires Google Closure Compiler compiler.jar to be in pwd
interpreter-min: interpreter
interpreter-min:
	cat src/api/closure_exports.js >> $(output)
	java -jar compiler.jar --compilation_level ADVANCED_OPTIMIZATIONS < $(output) > tmp
	mv tmp build/gay-lisp-$(version)-min.js

node: interpreter
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
	node -e "require('./build/gay-lisp-$(version)').test()"

clean:
	rm -f build/*