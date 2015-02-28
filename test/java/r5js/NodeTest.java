package r5js;

import org.junit.Test;

import java.io.File;
import java.nio.file.Files;

import static com.google.common.truth.Truth.ASSERT;

public final class NodeTest {

    @Test
    public void runTestsInNode() throws Exception {
        byte[] bytes = Platform.NODE.build();
        File tmp = File.createTempFile("test-all", ".js");
        Files.write(tmp.toPath(), bytes);

        Runtime runtime = Runtime.getRuntime();

        int statusCode = runtime.exec(new String[]{"command", "-v", "node"}).waitFor();
        ASSERT.withFailureMessage("node is required for testing")
                .that(statusCode)
                .isEqualTo(0);

        String outfilePathForNode = tmp.getAbsolutePath();
        outfilePathForNode = outfilePathForNode.substring(0, outfilePathForNode.length() - 3);

        String nodeCommand = String.format(
                "require('%s').r5js.test.main(process.argv, process.env);", outfilePathForNode);

        statusCode = new ProcessBuilder("node", "-e", nodeCommand, "type=unit", "verbose")
                .inheritIO()
                .start()
                .waitFor();

        ASSERT.withFailureMessage("error in tests")
                .that(statusCode).isEqualTo(0);
    }
}
