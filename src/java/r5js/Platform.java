package r5js;

import com.google.common.collect.ImmutableList;

import java.io.IOException;
import java.nio.file.Path;

final class Platform {

    final String name;
    final ImmutableList<CompilationUnit> inputs;

    private Platform(String name, ImmutableList<CompilationUnit> inputs) {
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

    static final class Builder {
        final ImmutableList.Builder<CompilationUnit> inputs = new ImmutableList.Builder<>();
        final String name;

        Builder(String name) {
            this.name = name;
        }

        Builder compilationUnit(CompilationUnit input) {
            inputs.add(input);
            return this;
        }

        Platform build() {
            return new Platform(name, inputs.build());
        }
    }
}
