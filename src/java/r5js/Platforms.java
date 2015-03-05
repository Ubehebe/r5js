package r5js;

import static r5js.CompilationUnit.HTML5_CLIENT;
import static r5js.CompilationUnit.HTML5_WORKER;

interface Platforms {
    static final Platform ANDROID = new Platform.Builder("android")
            .compilationUnit(
                    new CompilationUnit.Builder("r5js-android.js", "r5js.test.main")
                            .extern("custom-externs/android.js")
                            .build())
            .build();

    static final Platform HTML5 = new Platform.Builder("html5")
            .compilationUnit(HTML5_CLIENT)
            .compilationUnit(HTML5_WORKER)
            .build();

    static final Platform NODE = new Platform.Builder("node")
            .compilationUnit(
                    new CompilationUnit.Builder("r5js-node.js", "r5js.test.main")
                            .extern("externs/process.js")
                            .build())
            .build();
}
