var R5JS_nullEnv; // this is (null-environment 5)
var R5JS_R5RSEnv; // this is (scheme-report-environment 5)

function _R5JS() {
    this.timer = new FakeTimer(); // replace with Timer() for actual timing
}

var R5JS = new _R5JS();

_R5JS.prototype._scan = function(text) {
    return new Scanner(text);
};

/* This is just for debugging; the real pipeline requests the tokens
 one at a time. */
_R5JS.prototype._tokenize = function(text) {
    return this._scan(text).tokenize();
};

_R5JS.prototype._read = function(scanner) {
    this.timer.start('_read');
    return new Reader(scanner).read();
};

_R5JS.prototype._parse = function(root, lhs) {
    this.timer.start('_parse');
    return new Parser(root).parse(lhs);
};

_R5JS.prototype._desugar = function(root, env) {
    // todo bl reuse for repl
    if (!env)
        env = new Environment('global', R5JS_R5RSEnv);
    this.timer.start('_desug');
    return root.desugar(env).setStartingEnv(env);
};

_R5JS.prototype._eval = function(continuable) {
    this.timer.start('_eval');
    return trampoline(continuable);
};

_R5JS.prototype.eval = function(text) {
    this.timer.reset();

    var ans =
        this._eval(
            this._desugar(
                this._parse(
                    this._read(
                        this._scan(text)))));

    this.timer.stop();
    var report = this.timer.report();
    if (report)
        console.log(report);
    return ans ? ans.toString() : 'undefined';
};

// Just for convenience of evaling datums within the interpreter.
_R5JS.prototype.evalDatum = function(datum, env) {
    this.timer.suspend();
    var ans = this._eval(
        this._desugar(
            this._parse(datum), env));
    this.timer.unsuspend();
    return ans;
};

function bootstrap(syntaxLib, procLib) {
    R5JS_nullEnv = new Environment('null-environment-5');
    install(syntaxLib, R5JS_nullEnv);
    R5JS_nullEnv.seal();
    console.log('installed syntax lib ok');

    /* R5JS_R5RSEnv is the normal "root" environment. But we also have to
     support the "null environment", which is just the R5RS required syntax
     (no procedures). Example:

     (eval + (null-environment 5)) => +
     (eval '+ (null-environment 5)) => error (+ not defined)

     The easiest way to do this would be to put all the syntax definitions
     in R5JS_nullEnv, all the procedure definitions in R5JS_R5RSEnv, and
     set R5JS_R5RSEnv.enclosingEnv = R5JS_nullEnv. Unfortunately, macros
     require backlinks to their enclosing environments to resolve free
     identifiers correctly. If the macros are defined in the procedures'
     parent environment, things like

     (let ((x 1)) (+ x x))

     will fail, since + is defined in R5JS_R5RSEnv, which is unreachable
     from R5JS_nullEnv. So we make the null environment completely
     separate, and manually copy the bindings into R5JS_R5RSEnv
     (remembering to clone the macros and set their backlinks correctly).
     Ugh. */

    R5JS_R5RSEnv = R5JS_nullEnv.clone('scheme-report-environment-5');
    installBuiltins(R5JS_R5RSEnv);
    console.log('installed primitive procedures ok');
    install(procLib, R5JS_R5RSEnv);
    console.log('installed library procedures ok');
    R5JS_R5RSEnv.seal();
    console.log('interpreter is ready');
    console.log('----------------------------------------------------------------------');
}

function install(lib, env) {
    return trampoline(
        new Parser(
            new Reader(
                new Scanner(lib)
            ).read()
        ).parse()
            .desugar(env).setStartingEnv(env));
}

function installBuiltins(env) {
    for (var category in R5JS_builtins) {
        var procs = R5JS_builtins[category];
        for (var name in procs)
            registerBuiltin(name, procs[name], env);
    }
}