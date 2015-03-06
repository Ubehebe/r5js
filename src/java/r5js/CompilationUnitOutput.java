package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.JSError;

final class CompilationUnitOutput {
    private final String buildArtifactName;
    private final byte[] bytes;
    final ImmutableList<JSError> errors;
    final ImmutableList<JSError> warnings;

    CompilationUnitOutput(
            String buildArtifactName,
            byte[] bytes,
            ImmutableList<JSError> errors,
            ImmutableList<JSError> warnings) {
        this.buildArtifactName = buildArtifactName;
        this.bytes = bytes;
        this.errors = errors;
        this.warnings = warnings;
    }

    boolean success() {
        return errors.isEmpty() && warnings.isEmpty();
    }

    String getBuildArtifactName() {
        return buildArtifactName;
    }

    byte[] getBytes() {
        return bytes;
    }
}
