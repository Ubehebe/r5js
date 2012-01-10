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
    if (!env)
        env = new Environment('global', R5JS_R5RSEnv);

    if (parsed)
        ans = trampoline(parsed.desugar(env).setEnv(env));
    return ans && (!(ans instanceof Environment))
        ? ans.toString() // no internal data structures should escape the evaluator
        : 'undefined';
}

function installSyntax(syntaxLib) {
    R5JS_nullEnv = new Environment('dummy');
    R5JS_nullEnv = trampoline(
        new Parser(
            new Reader(
                new Scanner(syntaxLib)
            ).read()
        ).parse()
            .desugar(R5JS_nullEnv)
            .setEnv(R5JS_nullEnv)
    );
    R5JS_nullEnv.name = 'null-environment';
    R5JS_nullEnv.seal();
}

function installBuiltins() {
    if (!(R5JS_nullEnv instanceof Environment))
        throw new InternalInterpreterError('tried to install R5RS '
            + 'primitive procedures before installing R5RS syntax');

    R5JS_R5RSEnv = new Environment('scheme-report-environment 5', R5JS_nullEnv);


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