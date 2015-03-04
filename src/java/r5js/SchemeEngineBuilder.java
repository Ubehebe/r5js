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

final class SchemeEngineBuilder {

    private static final CompilerOptions OPTIONS = new CompilerOptions();
    static {
        OPTIONS.setAggressiveVarCheck(ERROR);
        OPTIONS.setBrokenClosureRequiresLevel(ERROR);
        OPTIONS.setCheckEventfulObjectDisposalPolicy(
                CheckEventfulObjectDisposal.DisposalCheckingPolicy.AGGRESSIVE);
        OPTIONS.setCheckGlobalNamesLevel(ERROR);
        OPTIONS.setCheckGlobalThisLevel(ERROR);
        OPTIONS.setCheckProvides(ERROR);
        OPTIONS.setCheckRequires(ERROR);
        OPTIONS.setCheckSuspiciousCode(true);
        OPTIONS.setCheckSymbols(true);
        OPTIONS.setCheckTypes(true);
        OPTIONS.setCodingConvention(new ClosureCodingConvention());
        OPTIONS.setLanguageIn(CompilerOptions.LanguageMode.ECMASCRIPT6_STRICT);
        OPTIONS.setLanguageOut(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT);
        OPTIONS.setReportMissingOverride(ERROR);
        OPTIONS.setInferConst(true);
        OPTIONS.setInferTypes(true);
        CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(OPTIONS);
    }

    private SchemeEngineBuilder() {}

    static byte[] build(Platform platform) throws IOException {
        Compiler compiler = new Compiler();
        compiler.setErrorManager(new ErrorManager(System.err));
        OPTIONS.setDependencyOptions(
                new DependencyOptions()
                        .setDependencyPruning(true)
                        .setDependencySorting(true)
                        .setEntryPoints(platform.closureEntryPoints)
                        .setMoocherDropping(true)); // There are moochers in the Closure Library >:|
        if (platform == Platform.HTML5) {
            OPTIONS.setDefineToStringLiteral("r5js.platform.html5.Client.WORKER_SCRIPT", "TODO bl");
        }
        Result underlying = compiler.compile(getExterns(), getSourceFiles(platform), OPTIONS);
        CompilationResult result = CompilationResult.fromUnderlying(underlying, compiler);
        if (!result.success) {
            throw new IllegalStateException();
        }
        return result.compiled.getBytes();
    }

    private static ImmutableList<JSError> onlyRelevant(JSError[] errors) {
        return ImmutableList.copyOf(
                Arrays.stream(errors)
                        .filter(SchemeEngineBuilder::isRelevant)
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
//        sourceFiles.add(SchemeSource.SYNTAX.bundle());
//        sourceFiles.add(SchemeSource.PROCEDURES.bundle());
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
