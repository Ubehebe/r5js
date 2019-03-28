load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//:__subpackages__",
    ],
)

ts_library(
    name = "environment",
    srcs = [
        "environment.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//base:value",
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
        "environment_impl.ts",
        "eof.ts",
        "errors.ts",
        "lambda.ts",
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
    tsconfig = "//:tsconfig.json",
    deps = [
        ":environment",
        "//ast:datum",
        "//ast:type",
        "//base:error",
        "//base:value",
        "//io",
        "//macro",
        "//parse:nonterminals",
        "//parse:parser",
        "//parse:rename_util",
        "//parse:terminals",
    ],
)

ts_library(
    name = "primitive_procedures",
    srcs = [
        "primitive_procedures.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":environment",
        ":runtime",
        ":trampoline",
        "//ast:datum",
        "//ast:type",
        "//base:error",
        "//base:value",
        "//io",
        "//parse:parser_impl",
    ],
)

ts_library(
    name = "trampoline",
    srcs = [
        "trampoline.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":environment",
        ":runtime",
        "//ast:datum",
        "//base:value",
        "//io",
        "//parse:parser_impl",
    ],
)
