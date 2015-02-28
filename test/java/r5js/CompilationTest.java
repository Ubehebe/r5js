package r5js;

import org.junit.Test;

import java.io.IOException;

public class CompilationTest {
    @Test
    public void android() throws IOException{
        byte[] bytes = Platform.ANDROID.build();
        System.out.printf("android %d%n", bytes.length);
    }

    @Test
    public void html5() throws IOException {
        byte[] bytes = Platform.HTML5.build();
        System.out.printf("html5 %d%n", bytes.length);
    }

    @Test
    public void node() throws IOException {
        byte[] bytes = Platform.NODE.build();
        System.out.printf("node %d%n", bytes.length);
    }
}
