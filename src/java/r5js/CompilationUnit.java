package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.CheckEventfulObjectDisposal;
import com.google.javascript.jscomp.ClosureCodingConvention;
import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.DependencyOptions;
import com.google.javascript.jscomp.JSError;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import static com.google.javascript.jscomp.CheckLevel.ERROR;

final class CompilationUnit {

    private final String buildArtifactName;
    private final String closureEntryPoint;
    private final ImmutableList<String> externs;
    private final UnaryOperator<CompilerOptions> customCompilerOptions;

    private CompilationUnit(
            String buildArtifactName,
            String closureEntryPoint,
            ImmutableList<String> externs,
            UnaryOperator<CompilerOptions> customCompilerOptions) {
        this.buildArtifactName = buildArtifactName;
        this.closureEntryPoint = closureEntryPoint;
        this.externs = externs;
        this.customCompilerOptions = customCompilerOptions;
    }

    String getBuildArtifactName() {
        return buildArtifactName;
    }

    private List<SourceFile> getExterns() throws IOException {
        List<SourceFile> externs = new ArrayList<>();
        addDefaultCompilerExterns(externs);
        this.externs.stream()
                .map(SourceFile::fromFile)
                .forEach(externs::add);
        return externs;
    }

    private static void addDefaultCompilerExterns(List<SourceFile> sourceFiles) throws IOException {
        try (ZipFile zip = new ZipFile("target/dependency/externs.zip")) {
            Enumeration<? extends ZipEntry> entries = zip.entries();
            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                sourceFiles.add(SourceFile.fromInputStream(
                        entry.getName(),
                        zip.getInputStream(entry),
                        StandardCharsets.UTF_8));
            }
        }
    }

    private CompilerOptions getCompilerOptions() {
        CompilerOptions options = defaultCompilerOptions();
        options.setDependencyOptions(
                new DependencyOptions()
                        .setDependencyPruning(true)
                        .setDependencySorting(true)
                        .setEntryPoints(ImmutableList.of(closureEntryPoint))
                        .setMoocherDropping(true)); // There are moochers in the Closure Library >:|
        return customCompilerOptions.apply(options);
    }

    CompilationUnitOutput compile(List<SourceFile> sources) throws IOException {
        Compiler compiler = new Compiler();
        compiler.setErrorManager(new ErrorManager(System.err));
        Result underlying = compiler.compile(
                getExterns(),
                sources,
                getCompilerOptions());
        ImmutableList<JSError> errors = onlyRelevant(underlying.errors);
        ImmutableList<JSError> warnings = onlyRelevant(underlying.warnings);
        return new CompilationUnitOutput(
                buildArtifactName,
                compiler.toSource().getBytes(StandardCharsets.UTF_8),
                errors,
                warnings);
    }

    private static ImmutableList<JSError> onlyRelevant(JSError[] errors) {
        return ImmutableList.copyOf(
                Arrays.stream(errors)
                        .filter(ErrorManager::isRelevant)
                        .collect(Collectors.toList()));
    }

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

        CompilationUnit build() {
            return new CompilationUnit(
                    buildArtifactName,
                    closureEntryPoint,
                    externs.build(),
                    customCompilerOptions);
        }
    }

    static final CompilationUnit HTML5_CLIENT = new Builder("html5-tests.js", "r5js.test.main")
            .customCompilerOptions(options -> {
                // The HTML5_TESTS client compilation unit requires a reference to the URL of the worker
                // compilation unit to start the Web Worker.
                options.setDefineToStringLiteral(
                        "r5js.platform.html5.Client.WORKER_SCRIPT",
                        CompilationUnit.HTML5_WORKER.buildArtifactName);
                return options;
            })
            .build();

    static final CompilationUnit HTML5_WORKER
            = new Builder("worker-tests.js", "r5js.platform.html5.Worker").build();
}