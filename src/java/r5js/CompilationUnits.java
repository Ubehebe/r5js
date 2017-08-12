package r5js;

import static r5js.EntryPoint.TEST_MAIN;

interface CompilationUnits {
    CompilationUnit HTML5_WORKER = CompilationUnit.of(
            "worker.js", Platform.HTML5)
            .entryPoint(EntryPoint.HTML5_WORKER)
            .build();

    /**
     * This contains {@link r5js.EntryPoint#TEST_MAIN}, so it isn't suitable to serve
     * in a production app.
     */
    CompilationUnit HTML5_DEV_CLIENT = CompilationUnit.of(
            "html5-devclient.js", Platform.HTML5)
            .entryPoint(EntryPoint.TEST_MAIN)
            .entryPoint(EntryPoint.HTML5_REPL_MAIN)
            .entryPoint(EntryPoint.HTML5_CONSOLE_MAIN)
            .customCompilerOptions(options -> {
                // HTML5_DEV_CLIENT requires a reference to the URL of the worker compilation unit
                // to start the Web Worker.
                options.setDefineToStringLiteral(
                        "r5js.platform.html5.Client.WORKER_SCRIPT",
                        HTML5_WORKER.buildArtifactName);
                return options;
            })
            .build();

    CompilationUnit NODE_REPL = CompilationUnit.of(
            "node-repl.js", Platform.NODE)
            .entryPoint(EntryPoint.NODE_REPL_MAIN)
            .build();

    CompilationUnit NODE_TESTS = CompilationUnit.of(
            "node-tests.js", Platform.NODE)
            .entryPoint(TEST_MAIN)
            .build();
}
