package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.CheckEventfulObjectDisposal;
import com.google.javascript.jscomp.ClosureCodingConvention;
import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.DependencyOptions;

import java.util.function.UnaryOperator;

import static com.google.javascript.jscomp.CheckLevel.ERROR;

final class CompilationUnit {

    private CompilationUnit() {}

    private static CompilerOptions defaultCompilerOptions() {
        CompilerOptions options = new CompilerOptions();
        options.setAggressiveVarCheck(ERROR);
        options.setBrokenClosureRequiresLevel(ERROR);
        options.setCheckEventfulObjectDisposalPolicy(
                CheckEventfulObjectDisposal.DisposalCheckingPolicy.AGGRESSIVE);
        options.setCheckGlobalNamesLevel(ERROR);
        options.setCheckGlobalThisLevel(ERROR);
        options.setCheckProvides(ERROR);
        options.setCheckRequires(ERROR);
        options.setCheckSuspiciousCode(true);
        options.setCheckSymbols(true);
        options.setCheckTypes(true);
        options.setCodingConvention(new ClosureCodingConvention());
        options.setLanguageIn(CompilerOptions.LanguageMode.ECMASCRIPT6_STRICT);
        options.setLanguageOut(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT);
        options.setReportMissingOverride(ERROR);
        options.setInferConst(true);
        options.setInferTypes(true);
        CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(options);
        return options;
    }

    static final class Input {
        final String buildArtifactName;
        final String closureEntryPoint;
        final ImmutableList<String> externs;
        final UnaryOperator<CompilerOptions> customCompilerOptions;

        private Input(
                String buildArtifactName,
                String closureEntryPoint,
                ImmutableList<String> externs,
                UnaryOperator<CompilerOptions> customCompilerOptions) {
            this.buildArtifactName = buildArtifactName;
            this.closureEntryPoint = closureEntryPoint;
            this.externs = externs;
            this.customCompilerOptions = customCompilerOptions;
        }

        CompilerOptions getCompilerOptions() {
            CompilerOptions options = defaultCompilerOptions();
            options.setDependencyOptions(
                    new DependencyOptions()
                            .setDependencyPruning(true)
                            .setDependencySorting(true)
                            .setEntryPoints(ImmutableList.of(closureEntryPoint))
                            .setMoocherDropping(true)); // There are moochers in the Closure Library >:|
            return customCompilerOptions.apply(options);
        }

        static final class Builder {
            private final String buildArtifactName;
            private final String closureEntryPoint;
            private final ImmutableList.Builder<String> externs = new ImmutableList.Builder<>();
            private UnaryOperator<CompilerOptions> customCompilerOptions = UnaryOperator.identity();

            Builder(String buildArtifactName, String closureEntryPoint) {
                this.buildArtifactName = buildArtifactName;
                this.closureEntryPoint = closureEntryPoint;
            }

            Builder extern(String extern) {
                externs.add(extern);
                return this;
            }

            Builder customCompilerOptions(UnaryOperator<CompilerOptions> options) {
                customCompilerOptions = options;
                return this;
            }

            Input build() {
                return new Input(
                        buildArtifactName,
                        closureEntryPoint,
                        externs.build(),
                        customCompilerOptions);
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
            = new Input.Builder("r5js-html5.js", "r5js.test.main")
            .customCompilerOptions(options -> {
                // The HTML5 client compilation unit requires a reference to the URL of the worker
                // compilation unit to start the Web Worker.
                options.setDefineToStringLiteral(
                        "r5js.platform.html5.Client.WORKER_SCRIPT",
                        CompilationUnit.HTML5_WORKER.buildArtifactName);
                return options;
            })
            .build();

    static final CompilationUnit.Input HTML5_WORKER
            = new Input.Builder("r5js-worker.js", "r5js.platform.html5.Worker").build();
}
