package r5js;

import static r5js.CompilationUnit.HTML5_CLIENT;
import static r5js.CompilationUnit.HTML5_WORKER;

interface Targets {

    static final Target ANDROID_REPL = new Target.Builder("android")
            .compilationUnit(
                    new CompilationUnit.Builder("android.js", EntryPoint.ANDROID_MAIN)
                            .extern("custom-externs/android.js")
                            .build())
            .build();

    static final Target ANDROID_TESTS = new Target.Builder("android")
            .compilationUnit(
                    new CompilationUnit.Builder("android-tests.js", EntryPoint.TEST_MAIN)
                            .extern("custom-externs/android.js")
                            .build())
            .build();

    static final Target HTML5_REPL = new Target.Builder("html5")
            .compilationUnit(HTML5_WORKER)
            .compilationUnit(new CompilationUnit.Builder("html5-repl.js", EntryPoint.REPL_MAIN)
                    .build())
            .build();

    static final Target HTML5_TESTS = new Target.Builder("html5")
            .compilationUnit(HTML5_CLIENT)
            .compilationUnit(HTML5_WORKER)
            .build();

    static final Target NASHORN_TESTS = new Target.Builder("nashorn")
            .compilationUnit(
                    new CompilationUnit.Builder("nashorn-tests.js", EntryPoint.TEST_MAIN)
                    .build())
            .build();

    static final Target NODE_REPL = new Target.Builder("node")
            .compilationUnit(
                    new CompilationUnit.Builder("node-repl.js", EntryPoint.REPL_MAIN)
                            .extern("externs/process.js")
                            .build())
            .build();

    static final Target NODE_TESTS = new Target.Builder("node")
            .compilationUnit(
                    new CompilationUnit.Builder("node-tests.js", EntryPoint.TEST_MAIN)
                            .extern("externs/process.js")
                            .build())
            .build();
}
