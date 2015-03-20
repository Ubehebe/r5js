package r5js;

import static r5js.CompilationUnit.HTML5_REPL;
import static r5js.CompilationUnit.named;
import static r5js.EntryPoint.*;
import static r5js.Platform.*;

interface Targets {

    static final Target<Android> ANDROID_REPL = Target.of(
            Android.class,
            named("android.js").entryPoint(ANDROID_MAIN).build());

    static final Target<Android> ANDROID_TESTS = Target.of(
            Android.class,
            named("android-tests.js").entryPoint(TEST_MAIN).build());

    static final Target<Html5> HTML5_WORKER = Target.of(
            Html5.class,
            CompilationUnit.HTML5_WORKER);

    static final Target<Html5> HTML5_CLIENT = Target.of(
            Html5.class,
            HTML5_REPL);
// TODO bl            .compilationUnit(HTML5_TEST_RUNNER)

    static final Target<Nashorn> NASHORN_TESTS = Target.of(
            Nashorn.class,
            named("nashorn-tests.js").entryPoint(TEST_MAIN).build());

    static final Target<Node> NODE_REPL = Target.of(
            Node.class,
            named("node-repl.js").entryPoint(NODE_REPL_MAIN).build());

    static final Target<Node> NODE_TESTS = Target.of(
            Node.class,
            named("node-tests.js").entryPoint(TEST_MAIN).build());
}
