function doEval(input, lhs) {
    var env = newStdEnv();
    var ans = trampoline(
        new Parser(
            new Reader(
                new Scanner(input)
            ).read()
        ).parse(lhs)
            .desugar(env), env);
    return ans
        ? ans.toString() // no internal data structures should escape the evaluator
        : 'undefined';
}