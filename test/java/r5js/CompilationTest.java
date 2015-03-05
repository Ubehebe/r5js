package r5js;

import org.junit.Test;

import java.io.IOException;

public class CompilationTest {
    @Test
    public void android() throws IOException{
        Platforms.ANDROID.build().forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }

    @Test
    public void html5() throws IOException {
        Platforms.HTML5.build().forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }

    @Test
    public void node() throws IOException {
        Platforms.NODE.build().forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }
}
