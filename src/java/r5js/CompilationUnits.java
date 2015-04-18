package r5js;

import static r5js.EntryPoint.ANDROID_MAIN;
import static r5js.EntryPoint.HTML5_REPL_MAIN;
import static r5js.EntryPoint.TEST_MAIN;

interface CompilationUnits {
    static final CompilationUnit ANDROID_REPL = CompilationUnit.of(
            "android.js", Platform.ANDROID)
            .entryPoint(ANDROID_MAIN)
            .build();

    static final CompilationUnit ANDROID_TESTS = CompilationUnit.of(
            "android-tests.js", Platform.ANDROID)
            .entryPoint(TEST_MAIN)
            .build();

    static final CompilationUnit HTML5_WORKER = CompilationUnit.of(
            "worker.js", Platform.HTML5)
            .entryPoint(EntryPoint.HTML5_WORKER)
            .build();

    /**
     * This contains {@link r5js.EntryPoint#TEST_MAIN}, so it isn't suitable to serve
     * in a production app.
     */
    static final CompilationUnit HTML5_DEV_CLIENT = CompilationUnit.of(
            "html5-devclient.js", Platform.HTML5)
            .entryPoint(EntryPoint.TEST_MAIN)
            .entryPoint(EntryPoint.HTML5_REPL_MAIN)
            .customCompilerOptions(options -> {
                // HTML5_DEV_CLIENT requires a reference to the URL of the worker compilation unit
                // to start the Web Worker.
                options.setDefineToStringLiteral(
                        "r5js.platform.html5.Client.WORKER_SCRIPT",
                        HTML5_WORKER.buildArtifactName);
                return options;
            })
            .build();

    static final CompilationUnit HTML5_PRODUCTION_CLIENT = CompilationUnit.of(
            "client.js", Platform.HTML5)
            .entryPoint(HTML5_REPL_MAIN)
            .customCompilerOptions(options -> {
                options.setDefineToStringLiteral(
                        "r5js.platform.html5.Client.WORKER_SCRIPT",
                        HTML5_WORKER.buildArtifactName);
                return options;
            })
            .build();

    static final CompilationUnit NASHORN_TESTS = CompilationUnit.of(
            "nashorn-tests.js", Platform.NASHORN)
            .entryPoint(TEST_MAIN)
            .build();

    static final CompilationUnit NODE_REPL = CompilationUnit.of(
            "node-repl.js", Platform.NODE)
            .entryPoint(EntryPoint.NODE_REPL_MAIN)
            .build();

    static final CompilationUnit NODE_TESTS = CompilationUnit.of(
            "node-tests.js", Platform.NODE)
            .entryPoint(TEST_MAIN)
            .build();
}
