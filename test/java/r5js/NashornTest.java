package r5js;

import com.google.common.truth.Truth;
import jdk.nashorn.api.scripting.NashornScriptEngineFactory;
import org.junit.Before;
import org.junit.Test;

import javax.script.ScriptEngine;
import javax.script.ScriptException;

public final class NashornTest {

    private static final NashornScriptEngineFactory factory = new NashornScriptEngineFactory();

    private ScriptEngine scriptEngine;

    @Before
    public void setUp() {
        scriptEngine = factory.getScriptEngine();
    }

    @Test
    public void testPassthrough() throws ScriptException {
        scriptEngine.eval("function foo(x) { return x; }");
        Object x = scriptEngine.eval(String.format("foo(%d);", 42));
        Truth.assertThat(x).isEqualTo(42);
    }
}
