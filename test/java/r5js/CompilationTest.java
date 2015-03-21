package r5js;

import com.google.common.base.Throwables;
import com.google.common.primitives.Bytes;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
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
            byte[] toWrite = Bytes.concat(
                    formatPreamble(output.getBuildArtifactName()), output.getBytes());
            // It's a bit odd for a unit test to write directly to the Maven target directory.
            // The "right" way to do this would be a custom Maven plugin that builds the JavaScript,
            // but I don't think it's worth the complexity yet.
            Files.write(Paths.get("target", output.getBuildArtifactName()), toWrite);
        } catch (IOException | InterruptedException e) {
            throw Throwables.propagate(e);
        }
    }

    private static boolean repoIsClean() throws IOException, InterruptedException {
        Process p = new ProcessBuilder("git", "status", "--porcelain")
                .redirectOutput(ProcessBuilder.Redirect.PIPE)
                .start();
        p.waitFor();
        String output = new BufferedReader(new InputStreamReader(p.getInputStream())).readLine();
        return output == null;
    }

    private static String resolveGitHead() throws IOException, InterruptedException {
        Process p = new ProcessBuilder("git", "rev-parse", "HEAD").start();
        p.waitFor();
        return new BufferedReader(new InputStreamReader(p.getInputStream()))
                .readLine()
                .substring(0, 7); // truncate full SHA to something reasonable
    }

    private static byte[] formatPreamble(String buildArtifactName) throws IOException, InterruptedException {
        return (repoIsClean()
                ? String.format("/* %s %s */\n", buildArtifactName, resolveGitHead())
                : String.format("/* %s */\n", buildArtifactName))
                .getBytes(StandardCharsets.UTF_8);
    }
}
