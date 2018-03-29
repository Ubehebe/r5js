load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

exports_files([
    "primitive_procedures.ts",
    "trampoline.ts",
])

ts_library(
    name = "ienvironment",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "ienvironment.ts",
    ],
    deps = [
        "//js:value",
    ],
)

ts_library(
    name = "runtime",
    srcs = [
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
        "proc_call.ts",
        "procedure.ts",
        "procspec.ts",
        "top_level_assignment.ts",
        "top_level_syntax_assignment.ts",
        "trampoline_helper.ts",
        "user_defined_procedure.ts",
        "valutil.ts",
        "varargs_user_defined_procedure.ts",
    ],
    deps = [
        "//js/ast:datum",
        "//js/ast:type",
        "//js/macro",
        "//js:error",
        "//js/io",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/parse:parser",
    ],
)