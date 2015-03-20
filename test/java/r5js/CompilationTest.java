package r5js;

import com.google.common.base.Throwables;
import org.junit.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class CompilationTest {

    @Test public void androidRepl() throws IOException {
        writeOut(CompilationUnits.ANDROID_REPL.compile());
    }

    @Test public void androidTests() throws IOException{
        writeOut(CompilationUnits.ANDROID_TESTS.compile());
    }

    @Test public void html5() throws IOException {
        writeOut(CompilationUnits.HTML5_WORKER.compile());
        writeOut(CompilationUnits.HTML5_DEV_CLIENT.compile());
    }

    @Test public void nashornTests() throws IOException {
        writeOut(CompilationUnits.NASHORN_TESTS.compile());
    }

    @Test public void nodeRepl() throws IOException {
        writeOut(CompilationUnits.NODE_REPL.compile());
    }

    @Test public void nodeTests() throws IOException {
        writeOut(CompilationUnits.NODE_TESTS.compile());
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
