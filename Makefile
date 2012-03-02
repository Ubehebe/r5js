# The "build process" just throws together all the JavaScript source files,
# minifies the Scheme source files, and embeds them into two global string
# literals at the end of the big JavaScript file, appropriately escaped:
#
# 's/;.*//'         gets rid of Scheme comments
# 's/\\/\\\\/g'     escapes backslashes (in Scheme character literals, though
#                   we don't seem to have any in either of the libraries)
# 's/\"/\\\"/g'     escapes quotation marks (Scheme string literals)
# tr -s '\n\t ' ' ' compresses whitespace
#
# I'm not too sure why we didn't have to figure out the dependencies among
# the JavaScript source files.
#
# Note that we're not doing minification on the JavaScript yet.
# We'll leave that to one of the many excellent libraries for the job.
r5js:
	mkdir -p build
	cat src/js/*.js > build/r5js.js
	echo "\nvar syntax = \"\c" >> build/r5js.js
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < src/scm/r5rs-syntax.scm | tr -s '\n\t ' ' ' >> build/r5js.js
	echo "\";" >> build/r5js.js
	echo "var procedures = \"\c" >> build/r5js.js
	sed -e 's/;.*//' -e 's/\\/\\\\/g' -e 's/\"/\\\"/g' < src/scm/r5rs-procedures.scm | tr -s '\n\t ' ' ' >> build/r5js.js
	echo "\";" >> build/r5js.js
	cp src/html/test_build.html build/test.html

clean:
	rm -f build/*