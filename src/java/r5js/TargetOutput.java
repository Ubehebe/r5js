package r5js;

import com.google.common.collect.ImmutableList;

final class TargetOutput {
    final ImmutableList<CompilationUnitOutput> outputs;

    TargetOutput(ImmutableList<CompilationUnitOutput> outputs) {
        this.outputs = outputs;
    }

    TargetOutput merge(TargetOutput other) {
        return new TargetOutput(
                new ImmutableList.Builder<CompilationUnitOutput>()
                .addAll(outputs)
                .addAll(other.outputs)
                .build());
    }
}
