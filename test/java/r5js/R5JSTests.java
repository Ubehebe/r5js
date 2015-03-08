package r5js;

import com.google.common.base.Throwables;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;
import jdk.nashorn.api.scripting.NashornScriptEngineFactory;
import jdk.nashorn.api.scripting.ScriptObjectMirror;
import r5js.platform.nashorn.Console;

import javax.script.ScriptEngine;
import java.nio.charset.StandardCharsets;
import java.util.Timer;
import java.util.TimerTask;
import java.util.function.BiFunction;

public final class R5JSTests extends Application {

    private static final BiFunction<ScriptObjectMirror, Integer, Void> SET_TIMEOUT
            = (callback, millis) -> {
        Platform.runLater(() -> {
            new Timer(false).schedule(new TimerTask() {
                @Override
                public void run() {
                    try {
                        // Prior to java -version 1.8.0_40, this incorrectly throws
                        // a "no current global instance" exception.
                        // See https://bugs.openjdk.java.net/browse/JDK-8050977,
                        callback.call(null);
                    } catch (Throwable t) {
                        throw Throwables.propagate(t);
                    }
                }
            }, millis);
        });
        return null;
    };

    private ScriptEngine scriptEngine;
    private String r5jsSource;

    @Override
    public void init() throws Exception {
        scriptEngine = new NashornScriptEngineFactory().getScriptEngine();
        r5jsSource = new String(
                Targets.NASHORN_TESTS.build().outputs.get(0).getBytes(),
                StandardCharsets.UTF_8);
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        scriptEngine.put("setTimeout", SET_TIMEOUT);
        scriptEngine.put("console", new Console());
        scriptEngine.eval(r5jsSource);
        scriptEngine.eval("r5js.test.main();");
    }
}
