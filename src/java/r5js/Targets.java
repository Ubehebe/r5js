package r5js;

import static r5js.CompilationUnit.HTML5_TEST_RUNNER;
import static r5js.EntryPoint.ANDROID_MAIN;
import static r5js.EntryPoint.REPL_MAIN;
import static r5js.EntryPoint.TEST_MAIN;

import static r5js.Platform.Android;
import static r5js.Platform.Html5;
import static r5js.Platform.Nashorn;
import static r5js.Platform.Node;

interface Targets {

    static final Target<Android> ANDROID_REPL = Target.forPlatform(Android.class)
            .compilationUnit(ANDROID_MAIN.named("android.js"))
            .build();

    static final Target<Android> ANDROID_TESTS = Target.forPlatform(Android.class)
            .compilationUnit(TEST_MAIN.named("android-tests.js"))
            .build();

    static final Target<Html5> HTML5_WORKER = Target.forPlatform(Html5.class)
            .compilationUnit(CompilationUnit.HTML5_WORKER)
            .build();

    static final Target<Html5> HTML5_REPL = Target.forPlatform(Html5.class)
            .include(HTML5_WORKER)
            .compilationUnit(REPL_MAIN.named("html5-repl.js"))
            .build();

    static final Target<Html5> HTML5_TESTS = Target.forPlatform(Html5.class)
            .include(HTML5_WORKER)
            .compilationUnit(HTML5_TEST_RUNNER)
            .build();

    static final Target<Html5> HTML5_ALL = Target.forPlatform(Html5.class)
            .include(HTML5_REPL)
            .include(HTML5_TESTS)
            .build();

    static final Target<Nashorn> NASHORN_TESTS = Target.forPlatform(Nashorn.class)
            .compilationUnit(TEST_MAIN.named("nashorn-tests.js"))
            .build();

    static final Target<Node> NODE_REPL = Target.forPlatform(Node.class)
            .compilationUnit(REPL_MAIN.named("node-repl.js"))
            .build();

    static final Target<Node> NODE_TESTS = Target.forPlatform(Node.class)
            .compilationUnit(TEST_MAIN.named("node-tests.js"))
            .build();
}
