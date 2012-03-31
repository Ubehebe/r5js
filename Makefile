website: spec
website:
	mkdir -p build
	cp js/* build/
	cp rotary-nav/*.js build/

	cd gay-lisp && make clean && make repl && cp build/* ../build/

spec: .fake
spec:
	mkdir -p build/img
	cp spec/r5rs.* build/
	cp spec/img/* build/img/

min: spec
min:
	mkdir -p build
	cd gay-lisp && make clean && make repl-min && cp build/* ../build/

clean:
	rm -rf ./build/* && cd gay-lisp && make clean

.fake: