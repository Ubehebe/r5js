package r5js;

import com.google.common.collect.ImmutableList;

final class CompilationResult {
    final ImmutableList<CompilationUnitOutput> outputs;

    CompilationResult(ImmutableList<CompilationUnitOutput> outputs) {
        this.outputs = outputs;
    }
}
