package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.CheckEventfulObjectDisposal;
import com.google.javascript.jscomp.CheckLevel;
import com.google.javascript.jscomp.ClosureCodingConvention;
import com.google.javascript.jscomp.CompilationLevel;
import com.google.javascript.jscomp.CompilerOptions;
import com.google.javascript.jscomp.JSError;
import com.google.javascript.jscomp.PrintStreamErrorManager;
import com.google.javascript.jscomp.Result;
import com.google.javascript.jscomp.SourceFile;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

import static com.google.javascript.jscomp.CheckLevel.ERROR;

/**
 * Compiler subclass that only reports errors/warnings in src/js.
 */
final class Compiler extends com.google.javascript.jscomp.Compiler {

    private static final CompilerOptions OPTIONS = new CompilerOptions();
    static {
        OPTIONS.setAggressiveVarCheck(ERROR);
        OPTIONS.setBrokenClosureRequiresLevel(ERROR);
        OPTIONS.setCheckEventfulObjectDisposalPolicy(
                CheckEventfulObjectDisposal.DisposalCheckingPolicy.AGGRESSIVE);
        OPTIONS.setCheckGlobalNamesLevel(ERROR);
        OPTIONS.setCheckGlobalThisLevel(ERROR);
        OPTIONS.setCheckProvides(ERROR);
        OPTIONS.setCheckRequires(ERROR);
        OPTIONS.setCheckSuspiciousCode(true);
        OPTIONS.setCheckSymbols(true);
        OPTIONS.setCheckTypes(true);
        OPTIONS.setCodingConvention(new ClosureCodingConvention());
        OPTIONS.setLanguage(CompilerOptions.LanguageMode.ECMASCRIPT5_STRICT);
        OPTIONS.setReportMissingOverride(ERROR);
        OPTIONS.setInferConst(true);
        OPTIONS.setInferTypes(true);
        OPTIONS.setManageClosureDependencies(ImmutableList.of(EntryPoints.TEST));
        CompilationLevel.ADVANCED_OPTIMIZATIONS.setOptionsForCompilationLevel(OPTIONS);
    }

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
        disableThreads(); // TODO bl shouldn't be necessary
    }

    public <T1 extends SourceFile, T2 extends SourceFile> Result compile(
            List<T1> externs, List<T2> inputs) {
        Result result = super.compile(externs, inputs, OPTIONS);
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
