website: spec
website:
	mkdir -p build
	cp rotary-nav/*.js build/

	cd gay-lisp && make clean && make repl && cp build/* ../build/
	# Must be after we make gay-lisp because we intentionally overwrite
	# build/main.js.
	cp js/* build/

spec: .fake
spec:
	mkdir -p build/img
	cp spec/r5rs.html build/
	cp spec/img/* build/img/
	cp css/r5rs.css build/

min: spec
min:
	mkdir -p build
	cd gay-lisp && make clean && make repl-min && cp build/* ../build/

clean:
	rm -rf ./build/* && cd gay-lisp && make clean

.fake: