package r5js;

import com.google.common.collect.ImmutableList;

final class CompilationResult {

    final ImmutableList<CompilationUnitOutput> outputs;
    final boolean success;

    CompilationResult(ImmutableList<CompilationUnitOutput> outputs) {
        this.outputs = outputs;
        this.success = outputs.stream().allMatch(CompilationUnitOutput::success);
    }

}
