package r5js;

import com.google.common.collect.ImmutableList;

import java.io.IOException;
import java.nio.file.Path;

import static r5js.CompilationUnit.HTML5_CLIENT;
import static r5js.CompilationUnit.HTML5_WORKER;

final class Platform {

    static final Platform ANDROID = new Builder("android")
            .compilationUnit("r5js-android.js", "r5js.test.main")
            .build();

    static final Platform HTML5 = new Builder("html5")
            .compilationUnit(HTML5_CLIENT)
            .compilationUnit(HTML5_WORKER)
            .build();

    static final Platform NODE = new Builder("node")
            .compilationUnit("r5js-node.js", "r5js.test.main")
            .build();

    final String name;
    final ImmutableList<CompilationUnit.Input> inputs;

    Platform(String name, ImmutableList<CompilationUnit.Input> inputs) {
        this.name = name;
        this.inputs = inputs;
    }

    boolean relevant(Path path) {
        return path.getFileName().toString().endsWith(".js")
                && (path.startsWith("closure-library")
                || (path.startsWith("src/js") && isRelevantSourcePath(path)));
    }

    private boolean isRelevantSourcePath(Path path) {
        if (!path.startsWith("src/js/platform")) {
            return true;
        }

        Path parent = path.getParent();
        return parent.endsWith("platform") || parent.endsWith(name);
    }

    ImmutableList<CompilationUnit.Output> build() throws IOException {
        return R5RSBuilder.build(this);
    }

    private static final class Builder {
        final ImmutableList.Builder<CompilationUnit.Input> inputs = new ImmutableList.Builder<>();
        final String name;

        Builder(String name) {
            this.name = name;
        }

        Builder compilationUnit(String buildArtifactName, String closureEntryPoint) {
            inputs.add(new CompilationUnit.Input(buildArtifactName, closureEntryPoint));
            return this;
        }

        Builder compilationUnit(CompilationUnit.Input input) {
            inputs.add(input);
            return this;
        }

        Platform build() {
            return new Platform(name, inputs.build());
        }
    }
}
