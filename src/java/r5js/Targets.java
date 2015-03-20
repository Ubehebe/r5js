package r5js;

import static r5js.CompilationUnit.HTML5_REPL;
import static r5js.CompilationUnit.of;
import static r5js.EntryPoint.*;
import static r5js.Platform.*;

interface Targets {

    static final Target ANDROID_REPL = Target.of(
            of("android.js", new Android()).entryPoint(ANDROID_MAIN).build());

    static final Target ANDROID_TESTS = Target.of(
            of("android-tests.js", new Android()).entryPoint(TEST_MAIN).build());

    static final Target HTML5_WORKER = Target.of(
            CompilationUnit.HTML5_WORKER);

    static final Target HTML5_CLIENT = Target.of(HTML5_REPL);
// TODO bl            .compilationUnit(HTML5_TEST_RUNNER)

    static final Target NASHORN_TESTS = Target.of(
            of("nashorn-tests.js", new Nashorn()).entryPoint(TEST_MAIN).build());

    static final Target NODE_REPL = Target.of(
            of("node-repl.js", new Node()).entryPoint(NODE_REPL_MAIN).build());

    static final Target NODE_TESTS = Target.of(
            of("node-tests.js", new Node()).entryPoint(TEST_MAIN).build());
}
