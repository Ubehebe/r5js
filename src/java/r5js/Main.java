package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.Result;
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

public final class Main {

    public static void main(String[] args) throws IOException {
        Compiler compiler = new Compiler(System.err);

        List<SourceFile> externs = getExterns();
        List<SourceFile> inputs = getSourceFiles();

        Result result = compiler.compile(externs, inputs);
        if (result.success) {
            String compiled = compiler.toSource();
            Files.write(Paths.get("r5js.js" /* TODO bl */), compiled.getBytes());
        }
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
}
