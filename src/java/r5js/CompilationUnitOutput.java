package r5js;

final class CompilationUnitOutput {
    private final String buildArtifactName;
    private final byte[] bytes;

    CompilationUnitOutput(String buildArtifactName, byte[] bytes) {
        this.buildArtifactName = buildArtifactName;
        this.bytes = bytes;
    }

    String getBuildArtifactName() {
        return buildArtifactName;
    }

    byte[] getBytes() {
        return bytes;
    }
}
