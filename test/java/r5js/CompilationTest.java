package r5js;

import org.junit.Test;

import java.io.IOException;

public class CompilationTest {
    @Test public void androidTests() throws IOException{
        Targets.ANDROID_TESTS.build().outputs.forEach(CompilationTest::print);
    }

    @Test public void html5Tests() throws IOException {
        Targets.HTML5_TESTS.build().outputs.forEach(CompilationTest::print);
    }

    @Test public void nashornTests() throws IOException {
        Targets.NASHORN_TESTS.build().outputs.forEach(CompilationTest::print);
    }

    @Test public void nodeTests() throws IOException {
        Targets.NODE_TESTS.build().outputs.forEach(CompilationTest::print);
    }

    private static void print(CompilationUnitOutput output) {
        System.out.printf("%s\t%d%n", output.getBuildArtifactName(), output.getBytes().length);
    }
}
