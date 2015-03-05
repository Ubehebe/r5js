package r5js;

import com.google.common.collect.ImmutableList;

final class CompilationUnit {

    private CompilationUnit() {}

    static final class Input {
        final String buildArtifactName;
        final String closureEntryPoint;
        final ImmutableList<String> externs;

        private Input(String buildArtifactName, String closureEntryPoint, ImmutableList<String> externs) {
            this.buildArtifactName = buildArtifactName;
            this.closureEntryPoint = closureEntryPoint;
            this.externs = externs;
        }

        static final class Builder {
            private final String buildArtifactName;
            private final String closureEntryPoint;
            private final ImmutableList.Builder<String> externs = new ImmutableList.Builder<>();

            Builder(String buildArtifactName, String closureEntryPoint) {
                this.buildArtifactName = buildArtifactName;
                this.closureEntryPoint = closureEntryPoint;
            }

            Builder extern(String extern) {
                externs.add(extern);
                return this;
            }

            Input build() {
                return new Input(buildArtifactName, closureEntryPoint, externs.build());
            }
        }
    }

    static final class Output {
        final String buildArtifactName;
        final byte[] bytes;

        private Output(String buildArtifactName, byte[] bytes) {
            this.buildArtifactName = buildArtifactName;
            this.bytes = bytes;
        }

        static Output from(Input input, byte[] bytes) {
            return new Output(input.buildArtifactName, bytes);
        }
    }

    static final CompilationUnit.Input HTML5_CLIENT
            = new Input.Builder("r5js-html5.js", "r5js.test.main").build();

    static final CompilationUnit.Input HTML5_WORKER
            = new Input.Builder("r5js-worker.js", "r5js.platform.html5.Worker").build();
}
