package r5js;

import com.google.common.escape.Escaper;
import com.google.common.escape.Escapers;
import com.google.javascript.jscomp.SourceFile;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

enum SchemeSource {
    SYNTAX("src/scm/r5rs-syntax.scm"),
    PROCEDURES("src/scm/r5rs-procedures.scm"),

    TEST_FRAMEWORK("src/scm/unit-test.scm"),

    TEST_FRAMEWORK_TESTS("src/scm/unit-test-tests.scm"),
    R5RS_TESTS("src/scm/r5rs-tests.scm"),
    NEGATIVE_TESTS("src/scm/negative-tests.scm"),
    OTHER_TESTS("src/scm/other-tests.scm");

    private static final Escaper ESCAPER = Escapers.builder()
            .addEscape('"', "\\\"")
            .addEscape('\n', "\\n")
            .addEscape('\\', "\\\\")
            .build();

    private final String filename;

    SchemeSource(String filename) {
        this.filename = filename;
    }

    private String format(String contents) {
        String escaped = ESCAPER.escape(contents);
        return String.format("goog.provide('%s');%n/** @const */var %s = \"%s\";%n",
                name(), name(), escaped);
    }

    SourceFile bundle() throws IOException {
        String contents = new String(
                Files.readAllBytes(Paths.get(filename)),
                StandardCharsets.UTF_8);
        return SourceFile.fromCode(filename, format(contents));
    }
}
