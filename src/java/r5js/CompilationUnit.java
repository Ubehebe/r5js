package r5js;

final class CompilationUnit {

    private CompilationUnit() {}

    static final class Input {
        final String buildArtifactName;
        final String closureEntryPoint;

        Input(String buildArtifactName, String closureEntryPoint) {
            this.buildArtifactName = buildArtifactName;
            this.closureEntryPoint = closureEntryPoint;
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
            = new Input("r5js-html5.js", "r5js.test.main");

    static final CompilationUnit.Input HTML5_WORKER
            = new Input("r5js-worker.js", "r5js.platform.html5.Worker");
}
