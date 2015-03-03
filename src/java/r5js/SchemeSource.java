package r5js;

import com.google.common.escape.Escaper;
import com.google.common.escape.Escapers;
import com.google.javascript.jscomp.SourceFile;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

enum SchemeSource {
    SYNTAX("src/scm/r5rs-syntax.scm", "SYNTAX"),
    PROCEDURES("src/scm/r5rs-procedures.scm", "PROCEDURES");

    private static final Escaper ESCAPER = Escapers.builder()
            .addEscape('"', "\\\"")
            .addEscape('\n', "\\n")
            .build();

    private final String filename;
    private final String googProvidedName;

    SchemeSource(String filename, String googProvidedName) {
        this.filename = filename;
        this.googProvidedName = googProvidedName;
    }

    private String format(String contents) {
        String escaped = ESCAPER.escape(contents);
        return String.format("goog.provide('%s');%n/** @const */var %s = \"%s\";%n",
                googProvidedName, googProvidedName, escaped);
    }

    SourceFile bundle() throws IOException {
        String contents = new String(
                Files.readAllBytes(Paths.get(filename)),
                StandardCharsets.UTF_8);
        return SourceFile.fromCode(filename, format(contents));
    }
}
