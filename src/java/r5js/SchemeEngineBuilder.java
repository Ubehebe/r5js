package r5js;

import com.google.common.collect.ImmutableList;
import com.google.common.io.ByteStreams;
import com.google.javascript.jscomp.*;
import com.google.javascript.jscomp.Compiler;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static com.google.javascript.jscomp.CheckLevel.ERROR;

final class SchemeEngineBuilder {

    private static final String PLATFORM_DEFINITION = "r5js.PLATFORM";

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
        OPTIONS.setLanguage(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT);
        OPTIONS.setReportMissingOverride(ERROR);
        OPTIONS.setInferConst(true);
        OPTIONS.setInferTypes(true);
        CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(OPTIONS);
    }

    private final Compiler compiler;

    SchemeEngineBuilder() {
        compiler = new Compiler();
    }

    CompilationResult build(Platform platform) throws IOException {
        return build(platform, new PrintStream(ByteStreams.nullOutputStream()));
    }

    CompilationResult build(Platform platform, PrintStream err) throws IOException {
        compiler.setErrorManager(new ErrorManager(err));
        OPTIONS.setDependencyOptions(
                new DependencyOptions()
                        .setDependencyPruning(true)
                        .setDependencySorting(true)
                        .setEntryPoints(platform.closureEntryPoints));
        OPTIONS.setDefineToStringLiteral(PLATFORM_DEFINITION, platform.closureDefineName);
        Result underlying = compiler.compile(getExterns(), getSourceFiles(), OPTIONS);
        return CompilationResult.fromUnderlying(underlying, compiler);
    }

    private static ImmutableList<JSError> onlyRelevant(JSError[] errors) {
        return ImmutableList.copyOf(
                Arrays.stream(errors)
                        .filter(SchemeEngineBuilder::isRelevant)
                        .collect(Collectors.toList()));
    }

    private static boolean isRelevant(JSError error) {
        return error.sourceName.startsWith("src/js");
    }

    private static List<SourceFile> getExterns() throws IOException {
        List<SourceFile> externs = new ArrayList<>();
        collectJsFilesIn("closure-compiler/externs", externs);
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

    private static List<SourceFile> getSourceFiles() throws IOException {
        List<SourceFile> sourceFiles = new ArrayList<>();
        collectJsFilesIn("src/js", sourceFiles);
        collectJsFilesIn("closure-library", sourceFiles);
        return sourceFiles;
    }

    private static void collectJsFilesIn(String root, List<SourceFile> sourceFiles) throws IOException {
        Files.walkFileTree(Paths.get(root), new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                String filename = file.toString();
                if (filename.endsWith(".js")) {
                    sourceFiles.add(SourceFile.fromFile(file.toFile()));
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
            System.out.printf("%d%n", compiled != null ? compiled.length() : 0);
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
