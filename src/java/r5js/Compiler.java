package r5js;

import com.google.javascript.jscomp.CheckLevel;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.JSError;
import com.google.javascript.jscomp.PrintStreamErrorManager;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

/**
 * Compiler subclass that only reports errors/warnings in src/js.
 */
final class Compiler extends com.google.javascript.jscomp.Compiler {

    Compiler(PrintStream err) {
        super(new PrintStreamErrorManager(err) {
            @Override
            public void println(CheckLevel level, JSError error) {
                if (isRelevant(error)) {
                    super.println(level, error);
                }
            }

            @Override
            public void report(CheckLevel level, JSError error) {
                if (isRelevant(error)) {
                    super.report(level, error);
                }
            }
        });
    }

    @Override
    public <T1 extends SourceFile, T2 extends SourceFile> Result compile(
            List<T1> externs, List<T2> inputs, CompilerOptions options) {
        Result result = super.compile(externs, inputs, options);
        return new Result(
                onlyRelevant(result.errors),
                onlyRelevant(result.warnings),
                result.debugLog,
                result.variableMap,
                result.propertyMap,
                result.namedAnonFunctionMap,
                result.functionInformationMap,
                result.sourceMap,
                result.externExport);
    }

    private static JSError[] onlyRelevant(JSError[] errors) {
        return Arrays.stream(errors).filter(Compiler::isRelevant).toArray(len -> new JSError[len]);
    }

    private static boolean isRelevant(JSError error) {
        return error.sourceName.startsWith("src/js");
    }
}
