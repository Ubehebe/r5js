function doEval(input, lhs) {
    return parseAndEval(
        new Reader(
            new Scanner(input)
        ).read()
        , newStdEnv(), lhs);
}

function parseAndEval(datum, env, lhs) {
    var parsed = new Parser(datum).parse(lhs);
    var ans;
    if (parsed)
        ans = trampoline(parsed.desugar(env), env);
    return ans
        ? ans.toString() // no internal data structures should escape the evaluator
        : 'undefined';
}