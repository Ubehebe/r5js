# The build process inserts src/js/* inside the body of the anonymous function
# in src/api/api.js in order to hide as much of the implementation as possible.
# It also embeds src/scm/syntax.scm and src/scm/procedures.scm as string
# literals into the body of the same function.
#
# Note that we're not doing minification on the JavaScript yet.
# We'll leave that to one of the many excellent libraries for the job.

output = build/gay_lisp.js

# Target the first line after the first brace in src/api/api.js
gay-lisp: firstBrace = `grep -m1 -A1 -n { src/api/api.js | head -1 | sed -e 's/^\([0-9]*\).*/\1/'`
gay-lisp: afterFirstBrace = `grep -m1 -A1 -n { src/api/api.js | tail -1 | sed -e 's/^\([0-9]*\).*/\1/'`
gay-lisp:
	mkdir -p build
	head -n$(firstBrace) src/api/api.js > $(output)
	cat src/js/*.js >> $(output)
	echo "\nvar syntax = \"\c" >> $(output)
	# Remove Scheme comments, escape Scheme backslashes and quotes, compress whitespace
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < src/scm/r5rs-syntax.scm | tr -s '\n\t ' ' ' >> $(output)
	echo "\";" >> $(output)
	echo "var procedures = \"\c" >> $(output)
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < src/scm/r5rs-procedures.scm | tr -s '\n\t ' ' ' >> $(output)
	echo "\";" >> $(output)
	tail -n+$(afterFirstBrace) src/api/api.js >> $(output)
	cp src/html/test_build.html build/test.html

clean:
	rm -f build/*