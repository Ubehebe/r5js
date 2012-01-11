var R5JS_nullEnv; // this is (null-environment 5)
var R5JS_R5RSEnv; // this is (scheme-report-environment 5)

var R5JS = {
    _scan: function(text) {
        return new Scanner(text);
    },

    /* This is just for debugging; the real pipeline requests the tokens
        one at a time. */
    _tokenize: function(text) {
        return R5JS._scan(text).tokenize();
    },

    _read: function(scanner) {
        return new Reader(scanner).read();
    },

    _parse: function(root, lhs) {
    return new Parser(root).parse(lhs);
    },

    _desugar: function(root, env) {
        if (!env) {
            /* todo bl: creating a new "global" environment for every
             start of the trampoline inhibits REPL-like incremental program
             construction. Shouldn't be too hard to reuse the global, though. */
            env = new Environment('global', R5JS_R5RSEnv);
            env.redefsOk = true;
        }
        return root.desugar(env).setEnv(env);
    },

    _eval: function(continuable) {
        return trampoline(continuable);
    },

    eval: function(text) {
        var ans =
            R5JS._eval(
                R5JS._desugar(
                    R5JS._parse(
                        R5JS._read(
                            R5JS._scan(text)))));
        return ans ? ans.toString() : 'undefined';
    },

    // Just for convenience of evaling datums within the interpreter.
    evalDatum: function(datum, env) {
        var ans =
            R5JS._eval(
            R5JS._desugar(
                R5JS._parse(datum), env));
        return ans ? ans.toString() : 'undefined';
    }
};

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