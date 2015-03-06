package r5js;

import com.google.common.collect.ImmutableList;

final class CompilationResult {

    final ImmutableList<CompilationUnit.Output> outputs;
    final boolean success;

    CompilationResult(ImmutableList<CompilationUnit.Output> outputs) {
        this.outputs = outputs;
        this.success = outputs.stream().allMatch(CompilationUnit.Output::success);
    }
}
