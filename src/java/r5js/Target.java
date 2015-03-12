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

/**
 * A target is the conceptual unit of deployment.
 *
 * <p>A target is targeted to a specific {@link Platform}.
 *
 * <p>Targets normally contain a single {@link CompilationUnit}, but can contain
 * multiple compilation units when required by the platform.
 * (For example, {@link Targets#HTML5_TESTS} contains one compilation unit for
 * the web worker that actually runs the interpreter, and another "client" compilation unit
 * for interacting with the worker.)
 */
final class Target {

    private final Platform platform;
    private final ImmutableList<CompilationUnit> inputs;

    private Target(Platform platform, ImmutableList<CompilationUnit> inputs) {
        this.platform = platform;
        this.inputs = inputs;
    }

    /**
     * Builds the target. This includes locating the sources, dependencies, and externs,
     * compiling the JavaScript sources, bundling the Scheme sources into the JavaScript
     * blob, and reporting errors.
     * @throws java.lang.IllegalStateException if compilation fails.
     */
    CompilationResult build() throws IOException {
        List<SourceFile> sourceFiles = getSourceFiles();
        ImmutableList.Builder<CompilationUnitOutput> builder = new ImmutableList.Builder<>();
        for (CompilationUnit input : inputs) {
            builder.add(input.compile(sourceFiles, platform.externs()));
        }
        return new CompilationResult(builder.build());
    }

    static Builder forPlatform(Platform platform) {
        return new Builder(platform);
    }

    Builder plus() {
        Builder builder = new Builder(platform);
        for (CompilationUnit input : inputs) {
            builder.compilationUnit(input);
        }
        return builder;
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
                || parent.endsWith(platform.toString());
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
        final Platform platform;

        private Builder(Platform platform) {
            this.platform = platform;
        }

        Builder compilationUnit(CompilationUnit input) {
            inputs.add(input);
            return this;
        }

        Target build() {
            return new Target(platform, inputs.build());
        }
    }
}
