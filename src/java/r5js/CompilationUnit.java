package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.CheckEventfulObjectDisposal;
import com.google.javascript.jscomp.ClosureCodingConvention;
import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.DependencyOptions;
import com.google.javascript.jscomp.PropertyRenamingPolicy;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;
import com.google.javascript.jscomp.VariableRenamingPolicy;
import com.google.javascript.jscomp.WarningLevel;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import static com.google.javascript.jscomp.CheckLevel.ERROR;

/**
 * A compilation unit is a set of JavaScript files that are compiled together.
 */
final class CompilationUnit {

    final String buildArtifactName;
    final Platform platform;
    private final ImmutableList<String> entryPoints;
    private final UnaryOperator<CompilerOptions> customCompilerOptions;

    private CompilationUnit(
            String buildArtifactName,
            Platform platform,
            ImmutableList<EntryPoint> entryPoints,
            UnaryOperator<CompilerOptions> customCompilerOptions) {
        this.buildArtifactName = buildArtifactName;
        this.platform = platform;
        this.entryPoints = ImmutableList.copyOf(entryPoints.stream()
                .map(EntryPoint::getEntryPoint)
                .collect(Collectors.toList()));
        this.customCompilerOptions = customCompilerOptions;
    }

    static Builder of(String buildArtifactName, Platform platform) {
        return new Builder(buildArtifactName, platform);
    }

    String getBuildArtifactName() {
        return buildArtifactName;
    }

    private ImmutableList<SourceFile> getExterns()
            throws IOException {
        ImmutableList.Builder<SourceFile> externs = new ImmutableList.Builder<>();
        addDefaultCompilerExterns(externs);
        platform.externs().stream()
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
                        .setEntryPoints(entryPoints)
                        .setMoocherDropping(true)); // There are moochers in the Closure Library >:|
        return customCompilerOptions.apply(options);
    }

    /**
     * @throws IOException if the externs cannot be fetched
     * @throws java.lang.IllegalStateException if compilation results in errors or warnings
     */
    CompilationUnitOutput compile()
            throws IOException {
        List<SourceFile> sources = SourceFileCollector.forPlatform(platform).getSourceFiles();
        Compiler compiler = new Compiler();
        compiler.setErrorManager(new ErrorManager(System.err));
        Result underlying = compiler.compile(
                getExterns(),
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

        // TODO bl: ADVANCED_OPTIMIZATIONS renames all unquoted variables by default
        // and also collapses properties. But these two options interact to crash
        // the html5 and node repls. Setting to LOCAL for now to minimize code size.
        options.setRenamingPolicy(
                VariableRenamingPolicy.LOCAL, PropertyRenamingPolicy.ALL_UNQUOTED);
        WarningLevel.VERBOSE.setOptionsForWarningLevel(options);
        return options;
    }

    static final class Builder {
        private final String buildArtifactName;
        private final Platform platform;
        private final ImmutableList.Builder<EntryPoint> entryPoints = new ImmutableList.Builder<>();
        private UnaryOperator<CompilerOptions> customCompilerOptions = UnaryOperator.identity();

        private Builder(String buildArtifactName, Platform platform) {
            this.buildArtifactName = buildArtifactName;
            this.platform = platform;
        }

        Builder entryPoint(EntryPoint entryPoint) {
            this.entryPoints.add(entryPoint);
            return this;
        }

        Builder customCompilerOptions(UnaryOperator<CompilerOptions> options) {
            customCompilerOptions = options;
            return this;
        }

        CompilationUnit build() {
            return new CompilationUnit(
                    buildArtifactName,
                    platform,
                    entryPoints.build(),
                    customCompilerOptions);
        }
    }
}