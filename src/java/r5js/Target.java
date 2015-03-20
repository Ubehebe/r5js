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
final class Target {
    private final CompilationUnit compilationUnit;

    private Target(CompilationUnit compilationUnit) {
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
            List<SourceFile> sourceFiles
                    = SourceFileCollector.forPlatform(compilationUnit.platform).getSourceFiles();
            output = compilationUnit.compile(sourceFiles);
        } catch (IOException e) {
            throw Throwables.propagate(e);
        }
        return new TargetOutput(ImmutableList.of(output));
    }

    static <T extends Platform> Target of(CompilationUnit input) {
        return new Target(input);
    }
}
