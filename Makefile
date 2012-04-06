website: spec
website:
	mkdir -p build
	cp rotary-nav/*.js build/

	cd gay-lisp && make clean && make repl && cp build/*.js ../build/
	# Must be after we make gay-lisp because we intentionally overwrite
	# build/main.js.
	cp js/* build/
	cp html/* build/
	cat build/*.js > tmp && rm build/*.js && mv tmp build/all.js

website-min: website
website-min:
	java -jar compiler.jar < build/all.js > tmp && mv tmp build/all.js

spec: .fake
spec:
	mkdir -p build/img
	# The spec takes a long time to parse, delaying the load and
	# DOMContentLoaded events, so we bring it in via ajax instead.
	# Unfortunately, that means it has to be valid XHTML. I've
	# carefully ensured spec/r5rs.html is valid HTML5 and (with
	# the addition of the following line) XHTML(5), but perhaps
	# we should run xmllint during the build process.
	echo '<?xml version="1.0" encoding="UTF-8"?>' > build/r5rs.xhtml
	cat spec/r5rs.html >> build/r5rs.xhtml
	cp spec/img/* build/img/
	cp css/r5rs.css build/

min: spec
min:
	mkdir -p build
	cd gay-lisp && make clean && make repl-min && cp build/* ../build/

clean:
	rm -rf ./build/* && cd gay-lisp && make clean

.fake: