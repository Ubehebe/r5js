package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.*;

import java.util.Arrays;
import java.util.stream.Collectors;

final class CompilationResult {
    final boolean success;
    final String compiled;
    final ImmutableList<JSError> errors;
    final ImmutableList<JSError> warnings;

    CompilationResult(
            boolean success,
            String compiled,
            ImmutableList<JSError> errors,
            ImmutableList<JSError> warnings) {
        this.success = success;
        this.compiled = compiled;
        this.errors = errors;
        this.warnings = warnings;
    }

    static CompilationResult fromUnderlying(Result underlying, com.google.javascript.jscomp.Compiler compiler) {
        ImmutableList<JSError> errors = onlyRelevant(underlying.errors);
        ImmutableList<JSError> warnings = onlyRelevant(underlying.warnings);
        boolean success = errors.isEmpty() && warnings.isEmpty();
        return new CompilationResult(
                success,
                success ? compiler.toSource() : null,
                errors,
                warnings);
    }

    private static ImmutableList<JSError> onlyRelevant(JSError[] errors) {
        return ImmutableList.copyOf(
                Arrays.stream(errors)
                        .filter(CompilationResult::isRelevant)
                        .collect(Collectors.toList()));
    }

    static boolean isRelevant(JSError error) {
        return error.sourceName != null && error.sourceName.startsWith("src/js");
    }
}
