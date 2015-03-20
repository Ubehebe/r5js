package r5js;

import static r5js.CompilationUnit.HTML5_TEST_RUNNER;
import static r5js.EntryPoint.*;
import static r5js.Platform.Android;
import static r5js.Platform.Html5;
import static r5js.Platform.Nashorn;
import static r5js.Platform.Node;
import static r5js.CompilationUnit.named;

interface Targets {

    static final Target<Android> ANDROID_REPL = Target.forPlatform(Android.class)
            .compilationUnit(named("android.js").entryPoint(ANDROID_MAIN).build())
            .build();

    static final Target<Android> ANDROID_TESTS = Target.forPlatform(Android.class)
            .compilationUnit(named("android-tests.js").entryPoint(TEST_MAIN)
                    .build())
            .build();

    static final Target<Html5> HTML5_WORKER = Target.forPlatform(Html5.class)
            .compilationUnit(CompilationUnit.HTML5_WORKER)
            .build();

    static final Target<Html5> HTML5_REPL = Target.forPlatform(Html5.class)
            .include(HTML5_WORKER)
            .compilationUnit(CompilationUnit.HTML5_REPL)
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
            .compilationUnit(named("nashorn-tests.js").entryPoint(TEST_MAIN)
                    .build())
            .build();

    static final Target<Node> NODE_REPL = Target.forPlatform(Node.class)
            .compilationUnit(named("node-repl.js").entryPoint(NODE_REPL_MAIN)
                    .build())
            .build();

    static final Target<Node> NODE_TESTS = Target.forPlatform(Node.class)
            .compilationUnit(named("node-tests.js").entryPoint(TEST_MAIN).build())
            .build();
}
