package r5js;

import com.google.common.base.Throwables;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class CompilationTest {

    @Test public void androidRepl() throws IOException {
        Targets.ANDROID_REPL.build().outputs.forEach(CompilationTest::writeOut);
    }

    @Test public void androidTests() throws IOException{
        Targets.ANDROID_TESTS.build().outputs.forEach(CompilationTest::writeOut);
    }

    @Test public void html5Repl() throws IOException {
        Targets.HTML5_REPL.build().outputs.forEach(CompilationTest::writeOut);
    }

    @Test public void html5Tests() throws IOException {
        Targets.HTML5_TESTS.build().outputs.forEach(CompilationTest::writeOut);
    }

    @Test public void nashornTests() throws IOException {
        Targets.NASHORN_TESTS.build().outputs.forEach(CompilationTest::writeOut);
    }

    @Test public void nodeRepl() throws IOException {
        Targets.NODE_REPL.build().outputs.forEach(CompilationTest::writeOut);
    }

    @Test public void nodeTests() throws IOException {
        Targets.NODE_TESTS.build().outputs.forEach(CompilationTest::writeOut);
    }

    private static void writeOut(CompilationUnitOutput output) {
        System.out.printf("%s\t%d%n", output.getBuildArtifactName(), output.getBytes().length);
        try {
            // It's a bit odd for a unit test to write directly to the Maven target directory.
            // The "right" way to do this would be a custom Maven plugin that builds the JavaScript,
            // but I don't think it's worth the complexity yet.
            Files.write(Paths.get("target", output.getBuildArtifactName()), output.getBytes());
        } catch (IOException e) {
            throw Throwables.propagate(e);
        }
    }
}
