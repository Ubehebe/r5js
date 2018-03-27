load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

exports_files([
    "assignment.ts",
    "branch.ts",
    "call_with_current_continuation.ts",
    "continuation.ts",
    "dynamic_wind_continuation.ts",
    "environment.ts",
    "eof.ts",
    "errors.ts",
    "lambda.ts",
    "nil.ts",
    "primitive_procedures.ts",
    "proc_call.ts",
    "procedure.ts",
    "procspec.ts",
    "top_level_assignment.ts",
    "top_level_syntax_assignment.ts",
    "trampoline.ts",
    "trampoline_helper.ts",
    "user_defined_procedure.ts",
    "valutil.ts",
    "varargs_user_defined_procedure.ts",
])

ts_library(
    name = "ienvironment_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "ienvironment.ts",
    ],
    deps = [
        "//js:value_ts",
    ],
)