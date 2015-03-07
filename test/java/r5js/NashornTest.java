package r5js;

import jdk.nashorn.api.scripting.NashornScriptEngineFactory;
import org.junit.Before;
import org.junit.Test;

import javax.script.ScriptEngine;
import javax.script.ScriptException;

import static com.google.common.truth.Truth.assertThat;

public final class NashornTest {

    private static final NashornScriptEngineFactory factory = new NashornScriptEngineFactory();

    private ScriptEngine scriptEngine;

    @Before
    public void setUp() {
        scriptEngine = factory.getScriptEngine();
    }

//    @Test
//    public void testPassthrough() throws Exception {
//        byte[] bytes = Platforms.NASHORN.build().outputs.get(0).getBytes();
//        scriptEngine.put("console", new Console());
//        scriptEngine.put("setTimeout", new BiFunction<ScriptObjectMirror, ScriptObjectMirror, ScriptObjectMirror>() {
//            @Override
//            public ScriptObjectMirror apply(ScriptObjectMirror callback, ScriptObjectMirror delay) {
//                callback.call()
//                System.out.println("!!!");
//                return delay;
//            }
//        });
//        scriptEngine.eval(new String(bytes, StandardCharsets.UTF_8));
//        Object hmm = scriptEngine.eval("r5js.test.main();");
//        System.out.println("");
//    }

    @Test
    public void installBindings() throws ScriptException {
        scriptEngine.put("foo", 42);
        Object blah = scriptEngine.eval("foo;");
        assertThat(blah).isEqualTo(42);
        scriptEngine.put("repeater", new Repeater());
        Object blah2 = scriptEngine.eval("repeater.repeat('42');");
        assertThat(blah2).isEqualTo("4242");
    }

    public static final class Repeater {
        public String repeat(String input) {
            return input + input;
        }
    }
}
