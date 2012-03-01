This is a temporary readme. The interpreter is complete, but the user
interface and JavaScript/DOM APIs need work.

#Building
1. `make`
2. Point your browser to `build/test.html`. You will get a REPL-like window
and some buttons that run several hundred unit tests.
(`src/html/test_dev.html` has the same interface, but manually pulls in
all the source files, so you don't need to run `make` beforehand.)