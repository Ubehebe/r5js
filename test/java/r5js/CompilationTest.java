package r5js;

import org.junit.Test;

import java.io.IOException;

import static com.google.common.truth.Truth.assertThat;

public class CompilationTest {
    @Test
    public void android() throws IOException{
        assertThat(new SchemeEngineBuilder().build(Platform.ANDROID, System.err).success).isTrue();
    }

//    @Test TODO bl why is this particular target failing?
//    public void html5() throws IOException {
//        assertThat(new SchemeEngineBuilder().build(Platform.HTML5, System.err).success).isTrue();
//    }

    @Test
    public void node() throws IOException {
        assertThat(new SchemeEngineBuilder().build(Platform.NODE, System.err).success).isTrue();
    }
}
