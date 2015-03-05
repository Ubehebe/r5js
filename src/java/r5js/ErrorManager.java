package r5js;

import com.google.javascript.jscomp.CheckLevel;
import com.google.javascript.jscomp.JSError;
import com.google.javascript.jscomp.PrintStreamErrorManager;

import java.io.PrintStream;

final class ErrorManager extends PrintStreamErrorManager {
    ErrorManager(PrintStream stream) {
        super(stream);
    }

    @Override
    public void println(CheckLevel level, JSError error) {
        if (CompilationResult.isRelevant(error)) {
            super.println(level, error);
        }
    }

    @Override
    public void report(CheckLevel level, JSError error) {
        if (CompilationResult.isRelevant(error)) {
            super.report(level, error);
        }
    }
}
