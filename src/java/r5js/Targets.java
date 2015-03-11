package r5js;

import static r5js.CompilationUnit.HTML5_CLIENT;
import static r5js.EntryPoint.ANDROID_MAIN;
import static r5js.EntryPoint.REPL_MAIN;
import static r5js.EntryPoint.TEST_MAIN;
import static r5js.Platform.ANDROID;
import static r5js.Platform.HTML5;
import static r5js.Platform.NASHORN;
import static r5js.Platform.NODE;

interface Targets {

    static final Target ANDROID_REPL = Target.forPlatform(ANDROID)
            .compilationUnit(
                    new CompilationUnit.Builder("android.js", ANDROID_MAIN)
                            .build())
            .build();

    static final Target ANDROID_TESTS = Target.forPlatform(ANDROID)
            .compilationUnit(
                    new CompilationUnit.Builder("android-tests.js", TEST_MAIN)
                            .build())
            .build();

    static final Target HTML5_WORKER = Target.forPlatform(HTML5)
            .compilationUnit(CompilationUnit.HTML5_WORKER)
            .build();

    static final Target HTML5_REPL = HTML5_WORKER.plus()
            .compilationUnit(new CompilationUnit.Builder("html5-repl.js", REPL_MAIN)
                    .build())
            .build();

    static final Target HTML5_TESTS = HTML5_WORKER.plus()
            .compilationUnit(HTML5_CLIENT)
            .build();

    static final Target NASHORN_TESTS = Target.forPlatform(NASHORN)
            .compilationUnit(
                    new CompilationUnit.Builder("nashorn-tests.js", TEST_MAIN)
                    .build())
            .build();

    static final Target NODE_REPL = Target.forPlatform(NODE)
            .compilationUnit(
                    new CompilationUnit.Builder("node-repl.js", REPL_MAIN)
                            .build())
            .build();

    static final Target NODE_TESTS = Target.forPlatform(NODE)
            .compilationUnit(
                    new CompilationUnit.Builder("node-tests.js", TEST_MAIN)
                            .build())
            .build();
}
