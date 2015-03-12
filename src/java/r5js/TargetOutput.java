package r5js;

import com.google.common.collect.ImmutableList;

final class TargetOutput {
    final ImmutableList<CompilationUnitOutput> outputs;

    TargetOutput(ImmutableList<CompilationUnitOutput> outputs) {
        this.outputs = outputs;
    }
}
