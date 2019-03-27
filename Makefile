website: spec
website:
	mkdir -p build
	cp rotary-nav/*.js build/

	cd gay-lisp && make clean && make repl && cp build/*.js ../build/
	# Must be after we make gay-lisp because we intentionally overwrite
	# build/main.js.
	cp js/* build/
	cat build/*.js > tmp && rm build/*.js && mv tmp build/all.js
	xsltproc xsl/desktop.xsl src/index.html > build/index.html

website-min: website
website-min:
	java -jar compiler.jar < build/all.js > tmp && mv tmp build/all.js


# todo bl: want to say "make deploy" and "make deploy mobile"
deploy: clean website-min
deploy:
	# Images are already on server
	rm -rf build/img
	cp robots.txt build/
	git tag `date +deploy_%Y_%m_%d_%H_%M_%S`
	scp build/* ubehebe_gaylisp@ssh.phx.nearlyfreespeech.net:/home/public/
	git push --tags

deploy-mobile: clean mobile-min
deploy-mobile:
	# Images are already on server
	rm -rf build/img
	cp robots.txt build/
	git tag `date +deploy_mobile_%Y_%m_%d_%H_%M_%S`
	scp build/* ubehebe_mgaylisp@ssh.phx.nearlyfreespeech.net:/home/public/
	git push --tags

mobile: spec
mobile:
	mkdir -p build
	cd gay-lisp && make clean && make smslike && cp build/*.js ../build/
	# Must be after we make gay-lisp because we intentionally overwrite
	# build/main.js.
	cp js/main.js build/
	cp css/mobile.css build/
	cp spec/r5rs.html build/
	mv build/main-smslike.js build/main.js
	cat build/*.js > tmp && mv tmp build/all.js
	xsltproc xsl/mobile.xsl src/index.html > build/index.html

mobile-min: mobile
mobile-min:
	java -jar compiler.jar < build/all.js > tmp && mv tmp build/all.js

min: spec
min:
	mkdir -p build
	cd gay-lisp && make clean && make repl-min && cp build/* ../build/

clean:
	rm -rf ./build/* && cd gay-lisp && make clean

.fake:
