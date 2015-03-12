package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.CheckEventfulObjectDisposal;
import com.google.javascript.jscomp.ClosureCodingConvention;
import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.DependencyOptions;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import static com.google.javascript.jscomp.CheckLevel.ERROR;

/**
 * A compilation unit is a set of JavaScript files that are compiled together.
 */
final class CompilationUnit {

    private final String buildArtifactName;
    private final EntryPoint entryPoint;
    private final UnaryOperator<CompilerOptions> customCompilerOptions;

    private CompilationUnit(
            String buildArtifactName,
            EntryPoint entryPoint,
            UnaryOperator<CompilerOptions> customCompilerOptions) {
        this.buildArtifactName = buildArtifactName;
        this.entryPoint = entryPoint;
        this.customCompilerOptions = customCompilerOptions;
    }

    String getBuildArtifactName() {
        return buildArtifactName;
    }

    private ImmutableList<SourceFile> getExterns(ImmutableList<String> platformExterns)
            throws IOException {
        ImmutableList.Builder<SourceFile> externs = new ImmutableList.Builder<>();
        addDefaultCompilerExterns(externs);
        platformExterns.stream()
                .map(SourceFile::fromFile)
                .forEach(externs::add);
        return externs.build();
    }

    private static void addDefaultCompilerExterns(ImmutableList.Builder<SourceFile> sourceFiles)
            throws IOException {
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
                        .setEntryPoints(ImmutableList.of(entryPoint.getEntryPoint()))
                        .setMoocherDropping(true)); // There are moochers in the Closure Library >:|
        return customCompilerOptions.apply(options);
    }

    /**
     * @throws IOException if the externs cannot be fetched
     * @throws java.lang.IllegalStateException if compilation results in errors or warnings
     */
    CompilationUnitOutput compile(List<SourceFile> sources, ImmutableList<String> platformExterns)
            throws IOException {
        Compiler compiler = new Compiler();
        compiler.setErrorManager(new ErrorManager(System.err));
        Result underlying = compiler.compile(
                getExterns(platformExterns),
                sources,
                getCompilerOptions());
        Stream.concat(Arrays.stream(underlying.warnings), Arrays.stream(underlying.errors))
                .filter(ErrorManager::isRelevant)
                .findAny()
                .ifPresent(ignore -> { throw new IllegalStateException(); });
        return new CompilationUnitOutput(
                buildArtifactName,
                compiler.toSource().getBytes(StandardCharsets.UTF_8));
    }

    private static CompilerOptions defaultCompilerOptions() {
        CompilerOptions options = new CompilerOptions();
        options.setAggressiveVarCheck(ERROR);
        options.setAmbiguateProperties(true);
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
        // TODO bl this breaks the HTML5 port at runtime! Find out why.
        // options.setDisambiguateProperties(true);
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
        private final EntryPoint entryPoint;
        private UnaryOperator<CompilerOptions> customCompilerOptions = UnaryOperator.identity();

        Builder(String buildArtifactName, EntryPoint entryPoint) {
            this.buildArtifactName = buildArtifactName;
            this.entryPoint = entryPoint;
        }

        Builder customCompilerOptions(UnaryOperator<CompilerOptions> options) {
            customCompilerOptions = options;
            return this;
        }

        CompilationUnit build() {
            return new CompilationUnit(
                    buildArtifactName,
                    entryPoint,
                    customCompilerOptions);
        }
    }

    static final CompilationUnit HTML5_TEST_RUNNER = new Builder("html5-tests.js", EntryPoint.TEST_MAIN)
            .customCompilerOptions(options -> {
                // HTML5_TEST_RUNNER requires a reference to the URL of the worker compilation unit
                // to start the Web Worker.
                options.setDefineToStringLiteral(
                        "r5js.platform.html5.Client.WORKER_SCRIPT",
                        CompilationUnit.HTML5_WORKER.buildArtifactName);
                return options;
            })
            .build();

    static final CompilationUnit HTML5_WORKER = EntryPoint.HTML5_WORKER.named("worker.js");
}