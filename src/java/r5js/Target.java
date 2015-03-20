package r5js;

import com.google.common.base.Throwables;
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
final class Target<T extends Platform> {

    private final Platform platform;
    private final CompilationUnit compilationUnit;

    private Target(T platform, CompilationUnit compilationUnit) {
        this.platform = platform;
        this.compilationUnit = compilationUnit;
    }

    /**
     * Builds the target. This includes locating the sources, dependencies, and externs,
     * compiling the JavaScript sources, bundling the Scheme sources into the JavaScript
     * blob, and reporting errors.
     * @throws java.lang.RuntimeException if compilation fails.
     */
    TargetOutput build() {
        CompilationUnitOutput output;
        try {
            List<SourceFile> sourceFiles = getSourceFiles();
            output = compilationUnit.compile(sourceFiles, platform.externs());
        } catch (IOException e) {
            throw Throwables.propagate(e);
        }
        return new TargetOutput(ImmutableList.of(output));
    }

    static <T extends Platform> Builder<T> forPlatform(Class<T> platformClass) {
        try {
            return new Builder<>(platformClass.newInstance());
        } catch (IllegalAccessException | InstantiationException e) {
            throw Throwables.propagate(e);
        }
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
                || parent.endsWith(platform.getClass().getSimpleName().toLowerCase());
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

    static final class Builder<T extends Platform> {
        final T platform;
        CompilationUnit input;

        private Builder(T platform) {
            this.platform = platform;
        }

        Builder<T> compilationUnit(CompilationUnit input) {
            this.input = input;
            return this;
        }

        Target<T> build() {
            return new Target<>(platform, input);
        }
    }
}
