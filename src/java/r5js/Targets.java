package r5js;

import static r5js.CompilationUnit.HTML5_TEST_RUNNER;
import static r5js.EntryPoint.ANDROID_MAIN;
import static r5js.EntryPoint.REPL_MAIN;
import static r5js.EntryPoint.TEST_MAIN;
import static r5js.Platform.ANDROID;
import static r5js.Platform.HTML5;
import static r5js.Platform.NASHORN;
import static r5js.Platform.NODE;

interface Targets {

    static final Target ANDROID_REPL = Target.forPlatform(ANDROID)
            .compilationUnit(ANDROID_MAIN.named("android.js"))
            .build();

    static final Target ANDROID_TESTS = Target.forPlatform(ANDROID)
            .compilationUnit(TEST_MAIN.named("android-tests.js"))
            .build();

    static final Target HTML5_WORKER = Target.forPlatform(HTML5)
            .compilationUnit(CompilationUnit.HTML5_WORKER)
            .build();

    static final Target HTML5_REPL = Target.forPlatform(HTML5)
            .include(HTML5_WORKER)
            .compilationUnit(REPL_MAIN.named("html5-repl.js"))
            .build();

    static final Target HTML5_TESTS = Target.forPlatform(HTML5)
            .include(HTML5_WORKER)
            .compilationUnit(HTML5_TEST_RUNNER)
            .build();

    static final Target HTML5_ALL = Target.forPlatform(HTML5)
            .include(HTML5_REPL)
            .include(HTML5_TESTS)
            .build();

    static final Target NASHORN_TESTS = Target.forPlatform(NASHORN)
            .compilationUnit(TEST_MAIN.named("nashorn-tests.js"))
            .build();

    static final Target NODE_REPL = Target.forPlatform(NODE)
            .compilationUnit(REPL_MAIN.named("node-repl.js"))
            .build();

    static final Target NODE_TESTS = Target.forPlatform(NODE)
            .compilationUnit(TEST_MAIN.named("node-tests.js"))
            .build();
}
