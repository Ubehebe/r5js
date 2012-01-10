var R5JS_nullEnv;
var R5JS_R5RSEnv;

function doEval(input, env) {
    return parseAndEval(
        new Reader(
            new Scanner(input)
        ).read()
        , env);
}

function parseAndEval(datum, env) {
    var parsed = new Parser(datum).parse();
    var ans;

    /* In the common case, we pass in no environment, which means
     we should make a new environment in front of the standard environment
     and evaluate the input in that context. Passing in an environment is
     only for the bootstrapping procedures like installSyntax() and
     installBuiltins().

     todo bl: make clearer for the common case. */
    if (!env) {
        /* todo bl: creating a new "global" environment for every
         start of the trampoline inhibits REPL-like incremental program
         construction. Shouldn't be too hard to reuse the global, though. */
        env = new Environment('global', R5JS_R5RSEnv);
        env.redefsOk = true;
    }

    if (parsed)
        ans = trampoline(parsed.desugar(env).setEnv(env));
    return ans
        ? ans.toString() // no internal data structures should escape the evaluator
        : 'undefined';
}

function installSyntax(syntaxLib) {
    R5JS_nullEnv = new Environment('null-environment-5');
    trampoline(
        new Parser(
            new Reader(
                new Scanner(syntaxLib)
            ).read()
        ).parse()
            .desugar(R5JS_nullEnv)
            .setEnv(R5JS_nullEnv)
    );
    R5JS_nullEnv.seal();
}

function installBuiltins() {
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

    for (var category in R5JS_builtins) {
        var procs = R5JS_builtins[category];
        for (var name in procs)
            registerBuiltin(name, procs[name], R5JS_R5RSEnv);
    }
    R5JS_R5RSEnv.seal();
}

function installLibrary(lib) {
    // todo bl
}