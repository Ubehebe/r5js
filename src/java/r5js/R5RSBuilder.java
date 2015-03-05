package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.CheckEventfulObjectDisposal;
import com.google.javascript.jscomp.CheckLevel;
import com.google.javascript.jscomp.ClosureCodingConvention;
import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.Compiler;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.DependencyOptions;
import com.google.javascript.jscomp.JSError;
import com.google.javascript.jscomp.PrintStreamErrorManager;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import static com.google.javascript.jscomp.CheckLevel.ERROR;


/**
 * Builds the project. This includes locating the sources, dependencies, and externs,
 * compiling the JavaScript sources, bundling the Scheme sources into the JavaScript
 * blob, and reporting errors.
 */
final class R5RSBuilder {

    private static CompilerOptions compilerOptions() {
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

    private R5RSBuilder() {}

    static ImmutableList<CompilationUnit.Output> build(Platform platform) throws IOException {
        ImmutableList.Builder<CompilationUnit.Output> outputs = new ImmutableList.Builder<>();
        for (CompilationUnit.Input input : platform.inputs) {
            outputs.add(build(input, platform));
        }
        return outputs.build();
    }

    private static CompilationUnit.Output build(
            CompilationUnit.Input input, Platform platform) throws IOException {
        Compiler compiler = new Compiler();
        compiler.setErrorManager(new ErrorManager(System.err));
        CompilerOptions options = compilerOptions();
        options.setDependencyOptions(
                new DependencyOptions()
                        .setDependencyPruning(true)
                        .setDependencySorting(true)
                        .setEntryPoints(ImmutableList.of(input.closureEntryPoint))
                        .setMoocherDropping(true)); // There are moochers in the Closure Library >:|
        // The HTML5 client compilation unit requires a reference to the URL of the worker
        // compilation unit to start the Web Worker.
        if (input == CompilationUnit.HTML5_CLIENT) {
            options.setDefineToStringLiteral(
                    "r5js.platform.html5.Client.WORKER_SCRIPT",
                    CompilationUnit.HTML5_WORKER.buildArtifactName);
        }
        Result underlying = compiler.compile(getExterns(), getSourceFiles(platform), options);
        CompilationResult result = CompilationResult.fromUnderlying(underlying, compiler);
        if (!result.success) {
            throw new IllegalStateException();
        }
        return CompilationUnit.Output.from(input, result.compiled.getBytes());
    }

    private static ImmutableList<JSError> onlyRelevant(JSError[] errors) {
        return ImmutableList.copyOf(
                Arrays.stream(errors)
                        .filter(R5RSBuilder::isRelevant)
                        .collect(Collectors.toList()));
    }

    private static boolean isRelevant(JSError error) {
        return error.sourceName != null && error.sourceName.startsWith("src/js");
    }

    private static List<SourceFile> getExterns() throws IOException {
        List<SourceFile> externs = new ArrayList<>();
        addExternsFromZip(externs);
        ImmutableList.of(
                "externs/buffer.js",
                "externs/core.js",
                "externs/events.js",
                "externs/fs.js",
                "externs/process.js",
                "externs/readline.js",
                "externs/stream.js",
                "custom-externs/android.js")
                .stream()
                .map(SourceFile::fromFile)
                .forEach(externs::add);
        return externs;
    }

    private static List<SourceFile> getSourceFiles(Platform platform) throws IOException {
        List<SourceFile> sourceFiles = new ArrayList<>();
        for (SchemeSource schemeSource : SchemeSource.values()) {
            sourceFiles.add(schemeSource.bundle());
        }
        collectJsFilesIn("src/js", sourceFiles, platform::relevant);
        collectJsFilesIn("closure-library", sourceFiles, path -> path.getFileName().toString()
                .endsWith(".js"));
        return sourceFiles;
    }



    private static void addExternsFromZip(List<SourceFile> sourceFiles) throws IOException {
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

    private static void collectJsFilesIn(String root, List<SourceFile> sourceFiles, Predicate<Path> filter) throws IOException {
        Files.walkFileTree(Paths.get(root), new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) throws IOException {
                if (filter.test(path)) {
                    sourceFiles.add(SourceFile.fromFile(path.toFile()));
                }
                return FileVisitResult.CONTINUE;
            }
        });
    }

    private static class ErrorManager extends PrintStreamErrorManager {
        public ErrorManager(PrintStream stream) {
            super(stream);
        }

        @Override
        public void println(CheckLevel level, JSError error) {
            if (isRelevant(error)) {
                super.println(level, error);
            }
        }

        @Override
        public void report(CheckLevel level, JSError error) {
            if (isRelevant(error)) {
                super.report(level, error);
            }
        }
    }

    public static class CompilationResult {
        public final boolean success;
        public final String compiled;
        public final ImmutableList<JSError> errors;
        public final ImmutableList<JSError> warnings;

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

        static CompilationResult fromUnderlying(Result underlying, Compiler compiler) {
            ImmutableList<JSError> errors = onlyRelevant(underlying.errors);
            ImmutableList<JSError> warnings = onlyRelevant(underlying.warnings);
            boolean success = errors.isEmpty() && warnings.isEmpty();
            return new CompilationResult(
                    success,
                    success ? compiler.toSource() : null,
                    errors,
                    warnings);
        }
    }
}
