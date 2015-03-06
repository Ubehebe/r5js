package r5js;

import org.junit.Test;

import java.io.IOException;

public class CompilationTest {
    @Test
    public void android() throws IOException{
        Platforms.ANDROID.build().outputs.forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }

    @Test
    public void html5() throws IOException {
        Platforms.HTML5.build().outputs.forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }

    @Test
    public void nashorn() throws IOException {
        Platforms.NASHORN.build().outputs.forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }

    @Test
    public void node() throws IOException {
        Platforms.NODE.build().outputs.forEach(output ->
                System.out.printf("%s\t%d%n", output.buildArtifactName, output.bytes.length));
    }
}
