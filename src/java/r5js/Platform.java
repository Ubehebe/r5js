package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.*;
import com.google.javascript.jscomp.Compiler;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.function.Predicate;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

final class Platform {

    final String name;
    final ImmutableList<CompilationUnit> inputs;

    private Platform(String name, ImmutableList<CompilationUnit> inputs) {
        this.name = name;
        this.inputs = inputs;
    }

    boolean relevant(Path path) {
        return path.getFileName().toString().endsWith(".js")
                && (path.startsWith("closure-library")
                || (path.startsWith("src/js") && isRelevantSourcePath(path)));
    }

    private boolean isRelevantSourcePath(Path path) {
        if (!path.startsWith("src/js/platform")) {
            return true;
        }

        Path parent = path.getParent();
        return parent.endsWith("platform") || parent.endsWith(name);
    }

    /**
     * Builds the project. This includes locating the sources, dependencies, and externs,
     * compiling the JavaScript sources, bundling the Scheme sources into the JavaScript
     * blob, and reporting errors.
     */
    ImmutableList<CompilationUnit.Output> build() throws IOException {
        ImmutableList.Builder<CompilationUnit.Output> outputs = new ImmutableList.Builder<>();
        for (CompilationUnit input : inputs) {
            outputs.add(build(input));
        }
        return outputs.build();
    }

    private CompilationUnit.Output build(CompilationUnit input) throws IOException {
        Compiler compiler = new Compiler();
        compiler.setErrorManager(new ErrorManager(System.err));
        Result underlying = compiler.compile(
                getExterns(input),
                getSourceFiles(),
                input.getCompilerOptions());
        CompilationResult result = CompilationResult.fromUnderlying(underlying, compiler);
        if (!result.success) {
            throw new IllegalStateException();
        }
        return CompilationUnit.Output.from(input, result.compiled.getBytes());
    }

    private static List<SourceFile> getExterns(CompilationUnit input) throws IOException {
        List<SourceFile> externs = new ArrayList<>();
        addDefaultCompilerExterns(externs);
        input.getExterns().stream()
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

    private List<SourceFile> getSourceFiles() throws IOException {
        List<SourceFile> sourceFiles = new ArrayList<>();
        for (SchemeSource schemeSource : SchemeSource.values()) {
            sourceFiles.add(schemeSource.bundle());
        }
        collectJsFilesIn("src/js", sourceFiles, this::relevant);
        collectJsFilesIn("closure-library", sourceFiles, path -> path.getFileName().toString()
                .endsWith(".js"));
        return sourceFiles;
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

    static final class Builder {
        final ImmutableList.Builder<CompilationUnit> inputs = new ImmutableList.Builder<>();
        final String name;

        Builder(String name) {
            this.name = name;
        }

        Builder compilationUnit(CompilationUnit input) {
            inputs.add(input);
            return this;
        }

        Platform build() {
            return new Platform(name, inputs.build());
        }
    }
}
