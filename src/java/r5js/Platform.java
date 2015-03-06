package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.SourceFile;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

final class Platform {

    private final String name;
    private final ImmutableList<CompilationUnit> inputs;

    private Platform(String name, ImmutableList<CompilationUnit> inputs) {
        this.name = name;
        this.inputs = inputs;
    }

    /**
     * Builds the project. This includes locating the sources, dependencies, and externs,
     * compiling the JavaScript sources, bundling the Scheme sources into the JavaScript
     * blob, and reporting errors.
     * @throws java.lang.IllegalStateException if compilation fails.
     */
    CompilationResult build() throws IOException {
        List<SourceFile> sourceFiles = getSourceFiles();
        ImmutableList.Builder<CompilationUnitOutput> builder = new ImmutableList.Builder<>();
        for (CompilationUnit input : inputs) {
            builder.add(input.compile(sourceFiles));
        }
        CompilationResult result = new CompilationResult(builder.build());
        if (!result.success) {
            throw new IllegalStateException();
        }
        return result;
    }

    private boolean relevant(Path path) {
        return path.getFileName().toString().endsWith(".js")
                && (path.startsWith("closure-library")
                || (path.startsWith("src/js") && isRelevantSourcePath(path)));
    }

    private boolean isRelevantSourcePath(Path path) {
        if (!path.startsWith("src/js/platform")) {
            return true;
        }

        Path parent = path.getParent();
        return parent.endsWith("platform")
                || parent.endsWith("common")
                || parent.endsWith(name);
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
